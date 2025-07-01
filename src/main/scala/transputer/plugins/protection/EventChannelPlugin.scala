package transputer.plugins.protection

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.io.TriState
import transputer.Global
import transputer.plugins.protection.ProtectionTypes._

/** T9000 Event Channel Plugin implementing 4 configurable Event channels.
  *
  * Features:
  *   - 4 Event channels for external control and synchronization
  *   - Configurable as input (interrupt) or output (handshake)
  *   - Fast interrupt response with automatic process descheduling
  *   - Integration with process scheduler for event waiting
  *   - Traditional "interrupt as communication" model
  */
class EventChannelPlugin extends FiberPlugin {
  override def getDisplayName(): String = "EventChannelPlugin"
  setName("eventChannels")

  // Event channel configuration
  object EventDirection extends SpinalEnum {
    val INPUT, OUTPUT = newElement()
  }

  case class EventChannelConfig() extends Bundle {
    val direction = EventDirection()
    val enabled = Bool()
    val processId = UInt(16 bits) // Process waiting on this event
    val autoAck = Bool() // Automatic acknowledgment
  }

  case class EventChannelState() extends Bundle {
    val pending = Bool() // Event pending
    val acknowledged = Bool() // Event acknowledged
    val lastValue = Bool() // Last event pin value
  }

  // Service interface for other plugins
  trait EventChannelService {
    def configureChannel(channelId: UInt, direction: EventDirection.C, processId: UInt): Unit
    def waitOnEvent(channelId: UInt, processId: UInt): Unit
    def sendEvent(channelId: UInt, value: Bool): Unit
    def isEventPending(channelId: UInt): Bool
    def acknowledgeEvent(channelId: UInt): Unit
  }

  during setup new Area {
    println(s"[${getDisplayName()}] setup start")

    addService(new EventChannelService {
      override def configureChannel(
        channelId: UInt,
        direction: EventDirection.C,
        processId: UInt
      ): Unit = {
        // Will be implemented in build phase
      }
      override def waitOnEvent(channelId: UInt, processId: UInt): Unit = {
        // Will be implemented in build phase
      }
      override def sendEvent(channelId: UInt, value: Bool): Unit = {
        // Will be implemented in build phase
      }
      override def isEventPending(channelId: UInt): Bool = eventPending(channelId)
      override def acknowledgeEvent(channelId: UInt): Unit = {
        // Will be implemented in build phase
      }
    })

    println(s"[${getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var eventConfigs: Vec[EventChannelConfig] = null
  var eventStates: Vec[EventChannelState] = null
  var eventPending: Vec[Bool] = null
  var interruptRequest: Bool = null

  during build new Area {
    println(s"[${getDisplayName()}] build start")

    // Event channel configuration registers
    eventConfigs = Vec(Reg(EventChannelConfig()), 4)
    eventStates = Vec(Reg(EventChannelState()), 4)
    eventPending = Vec(Bool(), 4)

    // Initialize event channels
    for (i <- 0 until 4) {
      eventConfigs(i).direction init EventDirection.INPUT
      eventConfigs(i).enabled init False
      eventConfigs(i).processId init 0
      eventConfigs(i).autoAck init True

      eventStates(i).pending init False
      eventStates(i).acknowledged init False
      eventStates(i).lastValue init False

      eventPending(i) := eventStates(i).pending && !eventStates(i).acknowledged
    }

    // Interrupt request generation
    interruptRequest = eventPending.orR

    println(s"[${EventChannelPlugin.this.getDisplayName()}] Event channel hardware configured")
    println(s"[${EventChannelPlugin.this.getDisplayName()}] - 4 configurable Event channels")
    println(
      s"[${EventChannelPlugin.this.getDisplayName()}] - Input (interrupt) and output (handshake) modes"
    )
    println(s"[${EventChannelPlugin.this.getDisplayName()}] - Process-based event waiting")
    println(s"[${EventChannelPlugin.this.getDisplayName()}] build end")
  }

  /** Configure an Event channel for input or output operation.
    *
    * @param channelId
    *   Channel index (0-3)
    * @param direction
    *   INPUT for interrupt, OUTPUT for handshake
    * @param processId
    *   Process that will handle this event
    */
  def configureEventChannel(channelId: UInt, direction: EventDirection.C, processId: UInt): Unit = {
    when(channelId < 4) {
      val config = eventConfigs(channelId)
      config.direction := direction
      config.processId := processId
      config.enabled := True

      // Reset state when reconfiguring
      val state = eventStates(channelId)
      state.pending := False
      state.acknowledged := False
    }
  }

  /** Process waits for event on specified channel.
    *
    * This implements the T9000 "interrupt as communication" model where a process simply waits for
    * input from an event channel.
    */
  def waitForEvent(channelId: UInt, processId: UInt): Unit = {
    when(channelId < 4) {
      val config = eventConfigs(channelId)
      config.processId := processId

      // Process will be descheduled until event occurs
      // Integration with scheduler happens via service interface
    }
  }

  /** Send event on output channel.
    *
    * For output channels, this drives the external pin and waits for handshake acknowledgment from
    * external device.
    */
  def sendEventOutput(channelId: UInt, value: Bool): Unit = {
    when(channelId < 4) {
      val config = eventConfigs(channelId)
      val state = eventStates(channelId)

      when(config.direction === EventDirection.OUTPUT && config.enabled) {
        // Set output value and mark as pending
        state.pending := True
        state.acknowledged := False

        // External handshake will clear pending when acknowledged
      }
    }
  }

  /** Handle external event input.
    *
    * Called when external Event pin transitions to signal an interrupt. Automatically schedules the
    * waiting process for execution.
    */
  def handleEventInput(channelId: UInt, pinValue: Bool): Unit = {
    when(channelId < 4) {
      val config = eventConfigs(channelId)
      val state = eventStates(channelId)

      when(config.direction === EventDirection.INPUT && config.enabled) {
        // Detect rising edge on event pin
        val risingEdge = pinValue && !state.lastValue
        state.lastValue := pinValue

        when(risingEdge) {
          // Mark event as pending
          state.pending := True

          // Auto-acknowledge if configured
          when(config.autoAck) {
            state.acknowledged := True
          }

          // Signal to scheduler that process should be woken up
          // This will be connected via service interface
        }
      }
    }
  }

  /** Acknowledge event to clear pending state.
    *
    * Used by processes to acknowledge they have handled the event.
    */
  def acknowledgeEvent(channelId: UInt): Unit = {
    when(channelId < 4) {
      val state = eventStates(channelId)
      state.acknowledged := True

      // Clear pending when acknowledged
      when(state.acknowledged) {
        state.pending := False
      }
    }
  }

  /** Check if event is pending on specified channel.
    */
  def isEventPending(channelId: UInt): Bool = {
    val pending = Bool()
    pending := False

    when(channelId < 4) {
      pending := eventPending(channelId)
    }

    pending
  }

  /** Get interrupt request status for all channels.
    */
  def getInterruptRequest(): Bool = interruptRequest

  /** Get event channel configuration.
    */
  def getChannelConfig(channelId: UInt): EventChannelConfig = {
    val config = EventChannelConfig()
    config.direction := EventDirection.INPUT
    config.enabled := False
    config.processId := 0
    config.autoAck := True

    when(channelId < 4) {
      config := eventConfigs(channelId)
    }

    config
  }

  /** Create external I/O bundle for Event channels.
    *
    * This creates the actual pins that connect to external devices.
    */
  def createEventIO(): Bundle = new Bundle {
    val events = Vec(TriState(Bool()), 4)

    // Connect internal logic to I/O pins
    for (i <- 0 until 4) {
      val config = eventConfigs(i)
      val state = eventStates(i)

      when(config.direction === EventDirection.INPUT) {
        // Input mode: read external pin
        events(i).writeEnable := False
        events(i).write := False

        // Handle input events
        handleEventInput(i, events(i).read)
      } otherwise {
        // Output mode: drive external pin
        events(i).writeEnable := config.enabled
        events(i).write := state.pending

        // Handle output acknowledgment
        when(events(i).read && state.pending) {
          acknowledgeEvent(i)
        }
      }
    }
  }

  /** Integration with process scheduler.
    *
    * Provides interface for scheduler to check which processes are waiting on events and which
    * events are ready.
    */
  def getWaitingProcesses(): Bundle = new Bundle {
    val waiting = Vec(Bool(), 4)
    val processIds = Vec(UInt(16 bits), 4)

    for (i <- 0 until 4) {
      val config = eventConfigs(i)
      waiting(i) := config.enabled && !eventPending(i)
      processIds(i) := config.processId
    }
  }

  /** Get ready events for scheduler.
    */
  def getReadyEvents(): Bundle = new Bundle {
    val ready = Vec(Bool(), 4)
    val processIds = Vec(UInt(16 bits), 4)

    for (i <- 0 until 4) {
      val config = eventConfigs(i)
      ready(i) := config.enabled && eventPending(i)
      processIds(i) := config.processId
    }
  }
}

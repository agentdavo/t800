package transputer.plugins.event

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import transputer.Global._
import transputer.plugins.event.EventService

/** T9000 Event Handling and Interrupt System Plugin Implements comprehensive event management and
  * interrupt handling Based on T9000 hardware reference manual
  */
class EventPlugin extends FiberPlugin {
  override def getDisplayName(): String = "EventPlugin"
  setName("event")

  // Event System Configuration
  private val MAX_EVENT_CHANNELS = 32
  private val MAX_INTERRUPT_LEVELS = 8
  private val MAX_ERROR_TYPES = 16

  // Hardware will be created in build phase
  var eventMaskReg: Bits = null
  var eventStatusReg: Bits = null
  var eventEnabledReg: Bool = null
  var interruptEnabledReg: Bool = null
  var eventChannelReady: Vec[Bool] = null
  var eventChannelData: Vec[Bits] = null
  var eventChannelEnabled: Vec[Bool] = null
  var interruptRequestReg: Bool = null
  var interruptAcknowledgeReg: Bool = null
  var interruptVectorReg: Bits = null
  var interruptPriorityReg: UInt = null
  var errorCodeReg: Bits = null
  var errorAddressReg: UInt = null
  var errorFlagsReg: Bits = null
  var errorActive: Bool = null
  var timerExpiredFlags: Vec[Bool] = null
  var timerEventEnabled: Vec[Bool] = null
  var processReadyReg: Bool = null
  var processTerminatedReg: Bool = null
  var processEventsEnabled: Bool = null

  during setup new Area {
    println(s"[${EventPlugin.this.getDisplayName()}] setup start")
    println(s"[${EventPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${EventPlugin.this.getDisplayName()}] build start")

    // Initialize hardware
    eventMaskReg = Reg(Bits(32 bits)) init 0
    eventStatusReg = Reg(Bits(32 bits)) init 0
    eventEnabledReg = Reg(Bool()) init False
    interruptEnabledReg = Reg(Bool()) init False
    eventChannelReady = Vec(Reg(Bool()) init False, MAX_EVENT_CHANNELS)
    eventChannelData = Vec(Reg(Bits(32 bits)) init 0, MAX_EVENT_CHANNELS)
    eventChannelEnabled = Vec(Reg(Bool()) init False, MAX_EVENT_CHANNELS)
    interruptRequestReg = Reg(Bool()) init False
    interruptAcknowledgeReg = Reg(Bool()) init False
    interruptVectorReg = Reg(Bits(8 bits)) init 0
    interruptPriorityReg = Reg(UInt(3 bits)) init 0
    errorCodeReg = Reg(Bits(16 bits)) init 0
    errorAddressReg = Reg(UInt(32 bits)) init 0
    errorFlagsReg = Reg(Bits(16 bits)) init 0
    errorActive = Reg(Bool()) init False
    timerExpiredFlags = Vec(Reg(Bool()) init False, 2)
    timerEventEnabled = Vec(Reg(Bool()) init False, 2)
    processReadyReg = Reg(Bool()) init False
    processTerminatedReg = Reg(Bool()) init False
    processEventsEnabled = Reg(Bool()) init False

    val logic = new Area {

      // Event Priority Encoder - finds highest priority active event
      val eventPriorityEncoder = new Area {
        val activeEvents = Cat(eventChannelReady.reverse)
        val highestPriorityEvent = Reg(UInt(log2Up(MAX_EVENT_CHANNELS) bits)) init 0
        val eventPending = Reg(Bool()) init False

        // Simple priority encoding - find first set bit (lower numbered events have higher priority)
        highestPriorityEvent := OHToUInt(OHMasking.first(activeEvents))
        eventPending := activeEvents.orR
      }

      // Interrupt Controller
      val interruptController = new Area {
        val pendingInterrupts = Reg(Bits(MAX_INTERRUPT_LEVELS bits)) init 0
        val currentLevel = Reg(UInt(3 bits)) init 0
        val interruptActive = Reg(Bool()) init False

        // Interrupt priority logic
        when(interruptEnabledReg && !interruptActive) {
          val highestInterrupt = OHToUInt(OHMasking.first(pendingInterrupts))
          when(pendingInterrupts.orR) {
            currentLevel := highestInterrupt
            interruptActive := True
            interruptRequestReg := True
            interruptVectorReg := Cat(B"5'h00", highestInterrupt.asBits)
          }
        }

        // Interrupt acknowledgment handling
        when(interruptAcknowledgeReg) {
          pendingInterrupts(currentLevel) := False
          interruptActive := False
          interruptRequestReg := False
          interruptAcknowledgeReg := False
        }
      }

      // Error Detection and Reporting
      val errorHandler = new Area {
        val errorHistory = Reg(Vec(Bits(16 bits), 8)) // Keep history of last 8 errors
        val errorHistoryIndex = Reg(UInt(3 bits)) init 0

        // Error classification (T9000 error types)
        object ErrorTypes {
          val INTEGER_ERROR = 0x01
          val FLOATING_POINT_ERROR = 0x02
          val BREAKPOINT = 0x04
          val MEMORY_ERROR = 0x08
          val LINK_ERROR = 0x10
          val TIMEOUT_ERROR = 0x20
          val PROCESS_ERROR = 0x40
          val SYSTEM_ERROR = 0x80
        }

        // Update event status register based on active errors
        eventStatusReg := Cat(
          B"16'h0000", // Reserved bits 31:16
          errorFlagsReg // Error flags bits 15:0
        )

        // Error reporting logic
        when(errorActive) {
          // Log error to history
          errorHistory(errorHistoryIndex) := errorCodeReg
          errorHistoryIndex := (errorHistoryIndex + 1).resized

          // Trigger interrupt if enabled
          when(interruptEnabledReg) {
            interruptController.pendingInterrupts(7) := True // Highest priority for errors
          }
        }
      }

      // Event Channel Processing
      for (i <- 0 until MAX_EVENT_CHANNELS) {
        when(eventChannelEnabled(i) && eventChannelReady(i)) {
          // Set corresponding bit in event status register
          eventStatusReg(i) := True

          // Check if this event is masked
          when(eventMaskReg(i)) {
            // Event is enabled and not masked - trigger processing
            when(interruptEnabledReg) {
              interruptController.pendingInterrupts(1) := True // Event interrupt level
            }
          }
        }
      }

      // Timer Event Integration
      for (i <- 0 until 2) {
        when(timerEventEnabled(i) && timerExpiredFlags(i)) {
          eventStatusReg(i + 16) := True // Timer events at bits 16-17
          when(interruptEnabledReg) {
            interruptController.pendingInterrupts(2) := True // Timer interrupt level
          }
        }
      }

      // Process Event Integration
      when(processEventsEnabled) {
        when(processReadyReg) {
          eventStatusReg(24) := True // Process ready event at bit 24
        }
        when(processTerminatedReg) {
          eventStatusReg(25) := True // Process terminated event at bit 25
        }
        when(processReadyReg || processTerminatedReg) {
          when(interruptEnabledReg) {
            interruptController.pendingInterrupts(3) := True // Process interrupt level
          }
        }
      }

      // Event Selection Logic (for ALT construct support)
      val eventSelector = new Area {
        val selectionMask = Reg(Bits(32 bits)) init 0
        val selectedEvent = Reg(UInt(5 bits)) init 0
        val selectionValid = Reg(Bool()) init False

        // Find first ready event that matches selection mask
        val maskedEvents = eventStatusReg & selectionMask
        selectedEvent := OHToUInt(OHMasking.first(maskedEvents))
        selectionValid := maskedEvents.orR
      }
    }

    // Service Implementation
    addService(new EventService {
      override def eventMask: Bits = eventMaskReg
      override def eventStatus: Bits = eventStatusReg
      override def eventEnabled: Bool = eventEnabledReg
      override def interruptEnabled: Bool = interruptEnabledReg

      override def eventChannelReady(channel: Int): Bool = {
        if (channel < MAX_EVENT_CHANNELS) eventChannelReady(channel) else False
      }

      override def eventChannelData(channel: Int): Bits = {
        if (channel < MAX_EVENT_CHANNELS) eventChannelData(channel) else B"32'h0"
      }

      override def enableEventChannel(channel: Int): Unit = {
        if (channel < MAX_EVENT_CHANNELS) {
          eventChannelEnabled(channel) := True
        }
      }

      override def disableEventChannel(channel: Int): Unit = {
        if (channel < MAX_EVENT_CHANNELS) {
          eventChannelEnabled(channel) := False
          eventChannelReady(channel) := False
        }
      }

      override def interruptRequest: Bool = interruptRequestReg
      override def interruptAcknowledge: Bool = interruptAcknowledgeReg
      override def interruptVector: Bits = interruptVectorReg
      override def interruptPriority: UInt = interruptPriorityReg

      override def errorCode: Bits = errorCodeReg
      override def errorAddress: UInt = errorAddressReg
      override def errorFlags: Bits = errorFlagsReg

      override def clearError(errorType: Int): Unit = {
        if (errorType < MAX_ERROR_TYPES) {
          errorFlagsReg(errorType) := False
          when(errorFlagsReg === 0) {
            errorActive := False
          }
        }
      }

      override def reportError(errorCode: Bits, address: UInt): Unit = {
        errorCodeReg := errorCode.resized
        errorAddressReg := address
        errorActive := True

        // Set appropriate error flag based on error code
        val errorType = errorCode(3 downto 0).asUInt
        when(errorType < MAX_ERROR_TYPES) {
          errorFlagsReg(errorType) := True
        }
      }

      override def timerExpired(timer: Int): Bool = {
        if (timer < 2) timerExpiredFlags(timer) else False
      }

      override def enableTimerEvent(timer: Int): Unit = {
        if (timer < 2) {
          timerEventEnabled(timer) := True
        }
      }

      override def disableTimerEvent(timer: Int): Unit = {
        if (timer < 2) {
          timerEventEnabled(timer) := False
          timerExpiredFlags(timer) := False
        }
      }

      override def processReady: Bool = processReadyReg
      override def processTerminated: Bool = processTerminatedReg

      override def enableProcessEvents(): Unit = {
        processEventsEnabled := True
      }

      override def disableProcessEvents(): Unit = {
        processEventsEnabled := False
        processReadyReg := False
        processTerminatedReg := False
      }

      override def triggerEvent(eventType: Int, data: Bits): Unit = {
        if (eventType < MAX_EVENT_CHANNELS) {
          eventChannelReady(eventType) := True
          eventChannelData(eventType) := data.resized
        }
      }

      override def waitForEvent(eventMask: Bits): Bool = {
        logic.eventSelector.selectionMask := eventMask.resized
        logic.eventSelector.selectionValid
      }

      override def selectEvent(eventChannels: Vec[Bool]): UInt = {
        // Convert Vec[Bool] to selection mask and find first ready
        val selMask = Cat(eventChannels.reverse)
        logic.eventSelector.selectionMask := selMask.asBits
        logic.eventSelector.selectedEvent
      }

      override def updateRegisters(): Unit = {
        // Registers update automatically in SpinalHDL
      }
    })

    println(s"[${EventPlugin.this.getDisplayName()}] build end")
  }
}

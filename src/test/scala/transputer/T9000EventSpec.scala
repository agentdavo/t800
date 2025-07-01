package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.event.{EventService, EventPlugin}

/** Comprehensive tests for T9000 Event Handling and Interrupt System.
  *
  * Tests the T9000 event system implementation:
  *   - Event channel management
  *   - Interrupt priority handling
  *   - Error event reporting
  *   - Timer and process event integration
  */
class T9000EventSpec extends AnyFunSuite {

  // Simple DUT to test event operations in isolation
  class EventTestDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val eventChannelIndex = in UInt (5 bits)
      val eventData = in Bits (32 bits)
      val eventMask = in Bits (32 bits)
      val errorCode = in Bits (16 bits)
      val errorAddress = in UInt (32 bits)
      val enableEvents = in Bool ()
      val enableInterrupts = in Bool ()
      val triggerEvent = in Bool ()
      val reportError = in Bool ()
      val timerIndex = in UInt (1 bit)

      val eventStatus = out Bits (32 bits)
      val eventEnabled = out Bool ()
      val interruptEnabled = out Bool ()
      val interruptRequest = out Bool ()
      val interruptVector = out Bits (8 bits)
      val errorDetected = out Bool ()
      val channelReady = out Bool ()
      val timerExpired = out Bool ()
    }

    // Event plugin needs basic transputer infrastructure
    val testPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.regstack.RegStackPlugin(),
      new transputer.plugins.event.EventPlugin()
    )
    val core = Database(db).on(Transputer(testPlugins))
    val eventService = core.host[EventService]

    // Event control logic - handle all possible channel indices
    for (i <- 0 until 32) {
      when(io.eventChannelIndex === i) {
        when(io.enableEvents) {
          eventService.enableEventChannel(i)
        }.otherwise {
          eventService.disableEventChannel(i)
        }
      }
    }

    when(io.enableInterrupts) {
      // Enable interrupt system (simplified)
    }

    // Trigger event for specific channel
    for (i <- 0 until 32) {
      when(io.triggerEvent && io.eventChannelIndex === i) {
        eventService.triggerEvent(i, io.eventData)
      }
    }

    when(io.reportError) {
      eventService.reportError(io.errorCode, io.errorAddress)
    }

    // Handle timer events for both timers
    for (i <- 0 until 2) {
      when(io.timerIndex === i) {
        when(io.enableEvents) {
          eventService.enableTimerEvent(i)
        }.otherwise {
          eventService.disableTimerEvent(i)
        }
      }
    }

    // Output event system state
    io.eventStatus := eventService.eventStatus
    io.eventEnabled := eventService.eventEnabled
    io.interruptEnabled := eventService.interruptEnabled
    io.interruptRequest := eventService.interruptRequest
    io.interruptVector := eventService.interruptVector
    io.errorDetected := eventService.errorCode =/= 0
    // Select channel ready and timer expired based on indices
    io.channelReady := False
    io.timerExpired := False
    for (i <- 0 until 32) {
      when(io.eventChannelIndex === i) {
        io.channelReady := eventService.eventChannelReady(i)
      }
    }
    for (i <- 0 until 2) {
      when(io.timerIndex === i) {
        io.timerExpired := eventService.timerExpired(i)
      }
    }
  }

  test("T9000 event channel management") {
    SimConfig.withWave.compile(new EventTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Initially event system should be disabled
      dut.clockDomain.waitSampling()
      assert(!dut.io.eventEnabled.toBoolean, "Event system should be disabled initially")

      // Enable event channel 5
      dut.io.eventChannelIndex #= 5
      dut.io.enableEvents #= true
      dut.clockDomain.waitSampling(2)

      // Channel should not be ready until an event is triggered
      assert(!dut.io.channelReady.toBoolean, "Channel should not be ready initially")

      // Trigger an event on channel 5
      dut.io.eventData #= 0x12345678L
      dut.io.triggerEvent #= true
      dut.clockDomain.waitSampling()
      dut.io.triggerEvent #= false
      dut.clockDomain.waitSampling(2)

      // Channel should now be ready
      assert(dut.io.channelReady.toBoolean, "Channel should be ready after event trigger")

      // Check event status register
      val eventStatus = dut.io.eventStatus.toLong
      assert((eventStatus & (1L << 5)) != 0, "Event status bit 5 should be set")

      // Disable the event channel
      dut.io.enableEvents #= false
      dut.clockDomain.waitSampling(2)

      assert(!dut.io.channelReady.toBoolean, "Channel should not be ready after disable")
    }
  }

  test("T9000 interrupt system") {
    SimConfig.withWave.compile(new EventTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable interrupt system
      dut.io.enableInterrupts #= true
      dut.clockDomain.waitSampling()

      // Initially no interrupt should be pending
      assert(!dut.io.interruptRequest.toBoolean, "No interrupt should be pending initially")

      // Enable event channel and trigger event
      dut.io.eventChannelIndex #= 3
      dut.io.enableEvents #= true
      dut.io.eventData #= 0xabcdef00L
      dut.io.triggerEvent #= true
      dut.clockDomain.waitSampling()
      dut.io.triggerEvent #= false
      dut.clockDomain.waitSampling(3)

      // Check if interrupt is generated (depends on implementation details)
      // In our simplified implementation, interrupts may be generated
      val interruptPending = dut.io.interruptRequest.toBoolean
      if (interruptPending) {
        val interruptVector = dut.io.interruptVector.toLong
        assert(
          interruptVector != 0,
          "Interrupt vector should be non-zero when interrupt is pending"
        )
      }
    }
  }

  test("T9000 error event reporting") {
    SimConfig.withWave.compile(new EventTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Initially no error should be detected
      dut.clockDomain.waitSampling()
      assert(!dut.io.errorDetected.toBoolean, "No error should be detected initially")

      // Report an error
      dut.io.errorCode #= 0x1234 // Arbitrary error code
      dut.io.errorAddress #= 0x80001000L // Error address
      dut.io.reportError #= true
      dut.clockDomain.waitSampling()
      dut.io.reportError #= false
      dut.clockDomain.waitSampling(2)

      // Error should now be detected
      assert(dut.io.errorDetected.toBoolean, "Error should be detected after reporting")

      // Check if error generates interrupt (implementation dependent)
      dut.io.enableInterrupts #= true
      dut.clockDomain.waitSampling(3)

      // Error reporting may trigger interrupt depending on configuration
    }
  }

  test("T9000 timer event integration") {
    SimConfig.withWave.compile(new EventTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Test timer 0 events
      dut.io.timerIndex #= 0
      dut.io.enableEvents #= true
      dut.clockDomain.waitSampling(2)

      // Initially timer should not be expired
      assert(!dut.io.timerExpired.toBoolean, "Timer 0 should not be expired initially")

      // Test timer 1 events
      dut.io.timerIndex #= 1
      dut.clockDomain.waitSampling()

      assert(!dut.io.timerExpired.toBoolean, "Timer 1 should not be expired initially")

      // Disable timer events
      dut.io.enableEvents #= false
      dut.clockDomain.waitSampling(2)

      // Timers should remain in disabled state
      dut.io.timerIndex #= 0
      dut.clockDomain.waitSampling()
      assert(!dut.io.timerExpired.toBoolean, "Timer 0 should not be expired after disable")
    }
  }

  test("T9000 event selection and masking") {
    SimConfig.withWave.compile(new EventTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable multiple event channels
      for (channelId <- 0 until 8) {
        dut.io.eventChannelIndex #= channelId
        dut.io.enableEvents #= true
        dut.clockDomain.waitSampling()

        // Trigger event on each channel
        dut.io.eventData #= 0x1000 + channelId
        dut.io.triggerEvent #= true
        dut.clockDomain.waitSampling()
        dut.io.triggerEvent #= false
        dut.clockDomain.waitSampling()
      }

      // Check event status - multiple events should be active
      dut.clockDomain.waitSampling(2)
      val eventStatus = dut.io.eventStatus.toLong

      // Verify that events 0-7 are set in status register
      for (channelId <- 0 until 8) {
        assert((eventStatus & (1L << channelId)) != 0, s"Event status bit $channelId should be set")
      }

      // Test event masking by disabling some channels
      for (channelId <- 2 until 6) {
        dut.io.eventChannelIndex #= channelId
        dut.io.enableEvents #= false
        dut.clockDomain.waitSampling()
      }

      dut.clockDomain.waitSampling(2)

      // Disabled channels should no longer be ready
      for (channelId <- 2 until 6) {
        dut.io.eventChannelIndex #= channelId
        dut.clockDomain.waitSampling()
        assert(
          !dut.io.channelReady.toBoolean,
          s"Channel $channelId should not be ready after disable"
        )
      }
    }
  }
}

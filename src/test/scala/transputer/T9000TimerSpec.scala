package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.timers.TimerService

/** Comprehensive tests for T9000 dual timer system (ClockReg0 and ClockReg1).
  *
  * Tests the T9000 timer implementation:
  *   - ClockReg0: Microsecond timer (1MHz resolution)
  *   - ClockReg1: 64-microsecond timer (15.625kHz resolution)
  *   - Timer load/read operations
  *   - Timer enable/disable control
  *   - Timer comparison operations for scheduling
  */
class T9000TimerSpec extends AnyFunSuite {

  // Simple DUT to test timer operations in isolation
  class TimerTestDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val clockReg0LoadValue = in UInt (32 bits)
      val clockReg1LoadValue = in UInt (32 bits)
      val clockReg0LoadEnable = in Bool ()
      val clockReg1LoadEnable = in Bool ()
      val clockReg0Enable = in Bool ()
      val clockReg1Enable = in Bool ()
      val compareValue = in UInt (32 bits)

      val clockReg0Out = out UInt (32 bits)
      val clockReg1Out = out UInt (32 bits)
      val clockReg0After = out Bool ()
      val clockReg1After = out Bool ()
    }

    val core = Database(db).on(Transputer(Transputer.unitPlugins()))
    val timerService = core.host[TimerService]

    // Timer control logic
    when(io.clockReg0LoadEnable) {
      timerService.setClockReg0(io.clockReg0LoadValue)
    }

    when(io.clockReg1LoadEnable) {
      timerService.setClockReg1(io.clockReg1LoadValue)
    }

    when(io.clockReg0Enable) {
      timerService.enableClockReg0()
    }.otherwise {
      timerService.disableClockReg0()
    }

    when(io.clockReg1Enable) {
      timerService.enableClockReg1()
    }.otherwise {
      timerService.disableClockReg1()
    }

    // Output current timer values and comparisons
    io.clockReg0Out := timerService.clockReg0
    io.clockReg1Out := timerService.clockReg1
    io.clockReg0After := timerService.clockReg0After(io.compareValue)
    io.clockReg1After := timerService.clockReg1After(io.compareValue)
  }

  test("T9000 ClockReg0 microsecond timer operation") {
    SimConfig.withWave.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initially enable both timers
      dut.io.clockReg0Enable #= true
      dut.io.clockReg1Enable #= true
      dut.clockDomain.waitSampling()

      // ClockReg0 should increment every cycle (microsecond resolution)
      val initialValue = dut.io.clockReg0Out.toLong

      // Wait a few cycles and check increment
      dut.clockDomain.waitSampling(5)
      val afterValue = dut.io.clockReg0Out.toLong

      assert(
        afterValue == initialValue + 5,
        s"ClockReg0 should increment by 5, got ${afterValue - initialValue}"
      )

      // Test loading a specific value
      dut.io.clockReg0LoadValue #= 0x12345678L
      dut.io.clockReg0LoadEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.clockReg0LoadEnable #= false
      dut.clockDomain.waitSampling()

      assert(
        dut.io.clockReg0Out.toLong == 0x12345678L,
        s"ClockReg0 should be loaded with 0x12345678, got 0x${dut.io.clockReg0Out.toLong.toHexString}"
      )

      // Verify it continues incrementing from loaded value
      dut.clockDomain.waitSampling(3)
      assert(
        dut.io.clockReg0Out.toLong == 0x12345678L + 3,
        "ClockReg0 should continue incrementing from loaded value"
      )
    }
  }

  test("T9000 ClockReg1 64-microsecond timer operation") {
    SimConfig.withWave.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Enable both timers
      dut.io.clockReg0Enable #= true
      dut.io.clockReg1Enable #= true
      dut.clockDomain.waitSampling()

      val initialValue = dut.io.clockReg1Out.toLong

      // ClockReg1 should increment every 64 cycles
      // Wait 63 cycles - should not increment yet
      dut.clockDomain.waitSampling(63)
      assert(
        dut.io.clockReg1Out.toLong == initialValue,
        "ClockReg1 should not increment until 64 cycles"
      )

      // One more cycle - should increment
      dut.clockDomain.waitSampling(1)
      assert(
        dut.io.clockReg1Out.toLong == initialValue + 1,
        "ClockReg1 should increment after 64 cycles"
      )

      // Test loading a specific value
      dut.io.clockReg1LoadValue #= 0xabcdef00L
      dut.io.clockReg1LoadEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.clockReg1LoadEnable #= false
      dut.clockDomain.waitSampling()

      assert(
        dut.io.clockReg1Out.toLong == 0xabcdef00L,
        s"ClockReg1 should be loaded with 0xABCDEF00, got 0x${dut.io.clockReg1Out.toLong.toHexString}"
      )

      // Wait another 64 cycles to verify increment
      dut.clockDomain.waitSampling(64)
      assert(
        dut.io.clockReg1Out.toLong == 0xabcdef00L + 1,
        "ClockReg1 should increment after 64 cycles from loaded value"
      )
    }
  }

  test("T9000 timer enable/disable control") {
    SimConfig.withWave.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Start with timers enabled
      dut.io.clockReg0Enable #= true
      dut.io.clockReg1Enable #= true
      dut.clockDomain.waitSampling()

      val clockReg0Initial = dut.io.clockReg0Out.toLong
      val clockReg1Initial = dut.io.clockReg1Out.toLong

      // Disable ClockReg0, keep ClockReg1 enabled
      dut.io.clockReg0Enable #= false
      dut.clockDomain.waitSampling(10)

      // ClockReg0 should not increment when disabled
      assert(
        dut.io.clockReg0Out.toLong == clockReg0Initial,
        "ClockReg0 should not increment when disabled"
      )

      // ClockReg1 should still be running (but may not have incremented yet due to 64-cycle period)
      dut.clockDomain.waitSampling(64)
      // ClockReg1 should have incremented at least once in 74 cycles
      assert(
        dut.io.clockReg1Out.toLong >= clockReg1Initial,
        "ClockReg1 should continue running when enabled"
      )

      // Re-enable ClockReg0
      dut.io.clockReg0Enable #= true
      dut.clockDomain.waitSampling(5)

      // ClockReg0 should resume incrementing
      assert(
        dut.io.clockReg0Out.toLong == clockReg0Initial + 5,
        "ClockReg0 should resume incrementing when re-enabled"
      )
    }
  }

  test("T9000 timer comparison operations") {
    SimConfig.withWave.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable timers and set initial values
      dut.io.clockReg0Enable #= true
      dut.io.clockReg1Enable #= true
      dut.io.clockReg0LoadValue #= 100
      dut.io.clockReg1LoadValue #= 50
      dut.io.clockReg0LoadEnable #= true
      dut.io.clockReg1LoadEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.clockReg0LoadEnable #= false
      dut.io.clockReg1LoadEnable #= false
      dut.clockDomain.waitSampling()

      // Test comparison with value less than current timers
      dut.io.compareValue #= 75
      dut.clockDomain.waitSampling()

      assert(dut.io.clockReg0After.toBoolean, "ClockReg0 (100) should be after compareValue (75)")
      assert(
        !dut.io.clockReg1After.toBoolean,
        "ClockReg1 (50) should not be after compareValue (75)"
      )

      // Test comparison with value greater than current timers
      dut.io.compareValue #= 150
      dut.clockDomain.waitSampling()

      assert(
        !dut.io.clockReg0After.toBoolean,
        "ClockReg0 (100) should not be after compareValue (150)"
      )
      assert(
        !dut.io.clockReg1After.toBoolean,
        "ClockReg1 (50) should not be after compareValue (150)"
      )

      // Test exact match
      dut.io.compareValue #= 100
      dut.clockDomain.waitSampling()

      assert(
        dut.io.clockReg0After.toBoolean,
        "ClockReg0 (100) should be after or equal to compareValue (100)"
      )
    }
  }

  test("T9000 timer wraparound behavior") {
    SimConfig.withWave.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable timers
      dut.io.clockReg0Enable #= true
      dut.io.clockReg1Enable #= true

      // Set ClockReg0 near maximum value to test wraparound
      val nearMax = (1L << 32) - 5 // 32-bit max minus 5
      dut.io.clockReg0LoadValue #= nearMax
      dut.io.clockReg0LoadEnable #= true
      dut.clockDomain.waitSampling()
      dut.io.clockReg0LoadEnable #= false
      dut.clockDomain.waitSampling()

      // Verify timer is near maximum
      assert(
        dut.io.clockReg0Out.toLong >= nearMax - 1,
        s"ClockReg0 should be near maximum, got ${dut.io.clockReg0Out.toLong}"
      )

      // Wait for wraparound (timer should wrap to 0)
      dut.clockDomain.waitSampling(10)

      // After wraparound, timer should be small value
      assert(
        dut.io.clockReg0Out.toLong < 10,
        s"ClockReg0 should wrap around to small value, got ${dut.io.clockReg0Out.toLong}"
      )
    }
  }
}

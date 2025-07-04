package transputer.plugins.timers

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin._
import transputer.Global
import transputer.plugins.timers.TimerService

/** T9000 Timer Plugin implementing dual timer system.
  *
  * Implements the T9000 timer architecture:
  *   - ClockReg0: Microsecond timer (assuming 1MHz clock or scaled)
  *   - ClockReg1: 64-microsecond timer (ClockReg0 / 64)
  *
  * Both timers count upward and are used for process scheduling, timeouts, and general timing
  * operations in the T9000 architecture.
  */
class TimerPlugin extends FiberPlugin {
  override def getDisplayName(): String = "TimerPlugin"
  setName("timer")
  val version = "TimerPlugin v0.2"
  private var clockReg0: UInt = null // Microsecond timer
  private var clockReg1: UInt = null // 64-microsecond timer
  private var clockReg1Counter: UInt = null // Divider counter for ClockReg1
  private var clockReg0LoadReq: Bool = null
  private var clockReg1LoadReq: Bool = null
  private var clockReg0LoadVal: UInt = null
  private var clockReg1LoadVal: UInt = null
  private var clockReg0Enable: Bool = null
  private var clockReg1Enable: Bool = null
  during setup new Area {
    report(L"Initializing $version")
    println(s"[${TimerPlugin.this.getDisplayName()}] setup start")
    println(s"[${TimerPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${TimerPlugin.this.getDisplayName()}] build start")

    // Create all hardware in build phase where component context is available
    // T9000 timer registers
    clockReg0 = Reg(UInt(Global.WORD_BITS bits)) init (0) // Microsecond timer
    clockReg1 = Reg(UInt(Global.WORD_BITS bits)) init (0) // 64-microsecond timer
    clockReg1Counter = Reg(UInt(6 bits)) init (0) // Divider for ClockReg1 (0-63)

    // Load control signals
    clockReg0LoadReq = Reg(Bool()) init (False)
    clockReg1LoadReq = Reg(Bool()) init (False)
    clockReg0LoadVal = Reg(UInt(Global.WORD_BITS bits)) init (0)
    clockReg1LoadVal = Reg(UInt(Global.WORD_BITS bits)) init (0)

    // Enable control signals
    clockReg0Enable = Reg(Bool()) init (True)
    clockReg1Enable = Reg(Bool()) init (True)

    // Make registers visible in simulation
    clockReg0.simPublic()
    clockReg1.simPublic()
    clockReg1Counter.simPublic()
    clockReg0Enable.simPublic()
    clockReg1Enable.simPublic()

    // Register service after hardware creation
    addService(new TimerService {
      override def clockReg0: UInt = TimerPlugin.this.clockReg0
      override def clockReg1: UInt = TimerPlugin.this.clockReg1

      override def setClockReg0(value: UInt): Unit = {
        clockReg0LoadReq := True
        clockReg0LoadVal := value
      }

      override def setClockReg1(value: UInt): Unit = {
        clockReg1LoadReq := True
        clockReg1LoadVal := value
      }

      override def enableClockReg0(): Unit = clockReg0Enable := True
      override def enableClockReg1(): Unit = clockReg1Enable := True
      override def disableClockReg0(): Unit = clockReg0Enable := False
      override def disableClockReg1(): Unit = clockReg1Enable := False

      override def clockReg0Enable: Bool = TimerPlugin.this.clockReg0Enable
      override def clockReg1Enable: Bool = TimerPlugin.this.clockReg1Enable
      override def updateRegisters(): Unit = {} // Registers update automatically

      override def clockReg0After(time: UInt): Bool = clockReg0 >= time
      override def clockReg1After(time: UInt): Bool = clockReg1 >= time
    })

    // Handle ClockReg0 (microsecond timer) load requests
    when(clockReg0LoadReq) {
      clockReg0 := clockReg0LoadVal
      clockReg0LoadReq := False
    }

    // Handle ClockReg1 (64-microsecond timer) load requests
    when(clockReg1LoadReq) {
      clockReg1 := clockReg1LoadVal
      clockReg1LoadReq := False
    }

    // ClockReg0 increments every cycle when enabled (simulating microsecond resolution)
    when(!clockReg0LoadReq && clockReg0Enable) {
      clockReg0 := clockReg0 + 1
    }

    // ClockReg1 increments every 64 cycles when enabled (64-microsecond resolution)
    when(!clockReg1LoadReq && clockReg1Enable) {
      clockReg1Counter := clockReg1Counter + 1
      when(clockReg1Counter === 63) {
        clockReg1 := clockReg1 + 1
        clockReg1Counter := 0
      }
    }

    // Reset counter when ClockReg1 is loaded
    when(clockReg1LoadReq) {
      clockReg1Counter := 0
    }

    println(s"[${TimerPlugin.this.getDisplayName()}] build end")
  }
}

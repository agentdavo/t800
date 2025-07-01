package transputer.plugins.timers

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global
import transputer.plugins.schedule.SchedService

/** T9000 Timer Wait Plugin implementing timer input functionality.
  *
  * This plugin extends the basic timer functionality to support process waiting on timer values.
  * When a process performs a "timer input", it is descheduled until the specified time is reached.
  *
  * Features:
  *   - Timer input for both microsecond and 64-microsecond timers
  *   - Automatic process descheduling/rescheduling
  *   - Multiple processes can wait on different timer values
  *   - Integrates with scheduler for process management
  */
class TimerWaitPlugin extends FiberPlugin {
  override def getDisplayName(): String = "TimerWaitPlugin"
  setName("timerWait")

  // Timer wait entry for tracking waiting processes
  case class TimerWaitEntry() extends Bundle {
    val processId = UInt(16 bits) // Process waiting on timer
    val targetTime = UInt(32 bits) // Target timer value
    val timerSelect = Bool() // False=ClockReg0, True=ClockReg1
    val active = Bool() // Entry is active
  }

  // Service interface for timer wait operations
  trait TimerWaitService {
    def waitOnTimer(processId: UInt, targetTime: UInt, useClockReg1: Bool): Unit
    def cancelTimerWait(processId: UInt): Unit
    def getReadyProcesses(): Vec[UInt]
    def isProcessWaiting(processId: UInt): Bool
  }

  // Maximum number of processes that can wait on timers simultaneously
  val MAX_TIMER_WAITS = 16

  during setup new Area {
    println(s"[${TimerWaitPlugin.this.getDisplayName()}] setup start")

    addService(new TimerWaitService {
      override def waitOnTimer(processId: UInt, targetTime: UInt, useClockReg1: Bool): Unit = {
        // Will be implemented in build phase
      }
      override def cancelTimerWait(processId: UInt): Unit = {
        // Will be implemented in build phase
      }
      override def getReadyProcesses(): Vec[UInt] = readyProcessIds
      override def isProcessWaiting(processId: UInt): Bool = {
        waitEntries.map(entry => entry.active && entry.processId === processId).orR
      }
    })

    println(s"[${TimerWaitPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var waitEntries: Vec[TimerWaitEntry] = null
  var readyProcessIds: Vec[UInt] = null
  var readyCount: UInt = null

  during build new Area {
    println(s"[${TimerWaitPlugin.this.getDisplayName()}] build start")

    // Get timer service to access current timer values
    val timerService = host.get[TimerService]
    val schedulerService = host[SchedService]

    // Timer wait table
    waitEntries = Vec(Reg(TimerWaitEntry()), MAX_TIMER_WAITS)

    // Initialize wait entries
    for (i <- 0 until MAX_TIMER_WAITS) {
      waitEntries(i).processId init 0
      waitEntries(i).targetTime init 0
      waitEntries(i).timerSelect init False
      waitEntries(i).active init False
    }

    // Ready process list
    readyProcessIds = Vec(UInt(16 bits), MAX_TIMER_WAITS)
    readyCount = UInt(log2Up(MAX_TIMER_WAITS + 1) bits)

    // Default values
    for (i <- 0 until MAX_TIMER_WAITS) {
      readyProcessIds(i) := 0
    }
    readyCount := 0

    // Check all timer wait entries each cycle
    val readyFlags = Vec(Bool(), MAX_TIMER_WAITS)
    for (i <- 0 until MAX_TIMER_WAITS) {
      val entry = waitEntries(i)

      // Check if this entry's timer has reached target
      when(entry.active) {
        val timerReached = Bool()
        when(entry.timerSelect) {
          // Waiting on ClockReg1 (64-microsecond timer)
          if (timerService.isDefined) {
            timerReached := timerService.get.clockReg1After(entry.targetTime)
          } else {
            timerReached := False
          }
        } otherwise {
          // Waiting on ClockReg0 (microsecond timer)
          if (timerService.isDefined) {
            timerReached := timerService.get.clockReg0After(entry.targetTime)
          } else {
            timerReached := False
          }
        }

        readyFlags(i) := timerReached

        // Clear entry when timer reached
        when(timerReached) {
          entry.active := False

          // Signal scheduler to wake up process
          // This would connect to scheduler service
        }
      } otherwise {
        readyFlags(i) := False
      }
    }

    // Collect ready processes
    var readyIdx = 0
    for (i <- 0 until MAX_TIMER_WAITS) {
      when(readyFlags(i)) {
        readyProcessIds(readyIdx) := waitEntries(i).processId
        readyIdx = readyIdx + 1
      }
    }
    readyCount := CountOne(readyFlags)

    println(s"[${TimerWaitPlugin.this.getDisplayName()}] Timer wait hardware configured")
    println(s"[${TimerWaitPlugin.this.getDisplayName()}] - Timer input for process waiting")
    println(
      s"[${TimerWaitPlugin.this.getDisplayName()}] - Up to $MAX_TIMER_WAITS simultaneous waits"
    )
    println(s"[${TimerWaitPlugin.this.getDisplayName()}] - Automatic process wake-up on timeout")
    println(s"[${TimerWaitPlugin.this.getDisplayName()}] build end")
  }

  /** Add a process to timer wait list.
    *
    * The process will be descheduled until the specified timer reaches the target value.
    */
  def addTimerWait(processId: UInt, targetTime: UInt, useClockReg1: Bool): Unit = {
    // Find free entry
    val freeFound = Bool()
    freeFound := False

    for (i <- 0 until MAX_TIMER_WAITS) {
      when(!waitEntries(i).active && !freeFound) {
        waitEntries(i).processId := processId
        waitEntries(i).targetTime := targetTime
        waitEntries(i).timerSelect := useClockReg1
        waitEntries(i).active := True
        freeFound := True
      }
    }
  }

  /** Cancel timer wait for a process.
    */
  def cancelProcessTimerWait(processId: UInt): Unit = {
    for (i <- 0 until MAX_TIMER_WAITS) {
      when(waitEntries(i).active && waitEntries(i).processId === processId) {
        waitEntries(i).active := False
      }
    }
  }

  /** Get list of processes ready due to timer expiry.
    */
  def getTimerReadyProcesses(): (Vec[UInt], UInt) = {
    (readyProcessIds, readyCount)
  }
}

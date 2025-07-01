package transputer.plugins.schedule

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.pipeline.ForkLink
import spinal.core.fiber.Retainer
import spinal.core.sim._
import spinal.lib.misc.plugin._
import transputer.Global
import transputer.plugins.schedule.{SchedCmd, SchedService, ProcessState}

/** T9000 Process Scheduler Plugin implementing dual-priority process scheduling.
  *
  * Features:
  *   - Dual priority queues (high/low priority)
  *   - Process state management (RUNNING, READY, WAITING, TERMINATED)
  *   - Timer-based scheduling integration
  *   - Queue persistence for SAVEH/SAVEL operations
  *   - Process context switching
  *   - T9000-compliant process lifecycle management
  */
class SchedulerPlugin extends FiberPlugin with SchedService {
  val version = "SchedulerPlugin v0.2"
  private var cmdReg: Flow[SchedCmd] = null
  private var currentProcReg: UInt = null // Currently running process
  private var nextReg: UInt = null // Next process to run
  private var hiFrontReg, hiBackReg, loFrontReg, loBackReg: UInt = null
  private var termReq: Bool = null // Termination request
  private var yieldReq: Bool = null // Yield request (STOPP)
  private var processCountReg: UInt = null // Total process count

  // Process state tracking (simplified - could be expanded to full state table)
  private var runningState: ProcessState.C = null
  private var schedulerActive: Bool = null

  // T9000 dual priority queues
  private var hiFifo: StreamFifo[UInt] = null // High priority process queue
  private var loFifo: StreamFifo[UInt] = null // Low priority process queue

  // Queue state persistence for SAVEH/SAVEL
  private var hiSavedFront, hiSavedBack: UInt = null
  private var loSavedFront, loSavedBack: UInt = null

  during setup new Area {
    report(L"Initializing $version")
    println(s"[${SchedulerPlugin.this.getDisplayName()}] setup start")
    println(s"[${SchedulerPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${SchedulerPlugin.this.getDisplayName()}] build start")

    // Create all hardware in build phase where component context is available
    cmdReg = Flow(SchedCmd())

    hiFifo = StreamFifo(UInt(Global.ADDR_BITS bits), Global.SCHED_QUEUE_DEPTH)
    loFifo = StreamFifo(UInt(Global.ADDR_BITS bits), Global.SCHED_QUEUE_DEPTH)
    // FIFO push signals will be conditionally assigned below

    // Process tracking registers
    currentProcReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    nextReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    processCountReg = Reg(UInt(16 bits)) init 0

    // Queue state registers
    hiFrontReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    hiBackReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    loFrontReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    loBackReg = Reg(UInt(Global.ADDR_BITS bits)) init 0

    // Queue persistence for SAVEH/SAVEL
    hiSavedFront = Reg(UInt(Global.ADDR_BITS bits)) init 0
    hiSavedBack = Reg(UInt(Global.ADDR_BITS bits)) init 0
    loSavedFront = Reg(UInt(Global.ADDR_BITS bits)) init 0
    loSavedBack = Reg(UInt(Global.ADDR_BITS bits)) init 0

    // Control signals
    termReq = Reg(Bool()) init (False)
    yieldReq = Reg(Bool()) init (False)
    schedulerActive = Reg(Bool()) init (True)
    runningState = Reg(ProcessState()) init (ProcessState.READY)

    // Make important signals visible in simulation
    currentProcReg.simPublic()
    nextReg.simPublic()
    processCountReg.simPublic()
    schedulerActive.simPublic()

    // Pipeline for process enqueueing
    val CMD_PTR = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_HI = Payload(Bool())
    val in = CtrlLink()
    val toHi = CtrlLink()
    val toLo = CtrlLink()
    ForkLink(in.down, Seq(toHi.up, toLo.up))
    in.up.driveFrom(cmdReg) { (n, p) =>
      n(CMD_PTR) := p.ptr
      n(CMD_HI) := p.high
    }

    // High priority queue management
    hiFifo.io.push.valid := toHi.isValid && toHi(CMD_HI)
    hiFifo.io.push.payload := toHi(CMD_PTR)
    when(hiFifo.io.push.fire) {
      hiBackReg := hiBackReg + 1
      processCountReg := processCountReg + 1
    }
    toHi.haltWhen(hiFifo.io.push.valid && !hiFifo.io.push.ready)

    // Low priority queue management
    loFifo.io.push.valid := toLo.isValid && !toLo(CMD_HI)
    loFifo.io.push.payload := toLo(CMD_PTR)
    when(loFifo.io.push.fire) {
      loBackReg := loBackReg + 1
      processCountReg := processCountReg + 1
    }
    toLo.haltWhen(loFifo.io.push.valid && !loFifo.io.push.ready)

    // T9000 priority-based scheduling logic
    val hiPop = hiFifo.io.pop
    val loPop = loFifo.io.pop
    hiPop.ready := False
    loPop.ready := False

    // Context switching state machine
    val needsScheduling = termReq || yieldReq

    when(needsScheduling && schedulerActive) {
      // Always service high priority queue first (T9000 preemptive priority)
      when(hiPop.valid) {
        // Schedule high priority process
        currentProcReg := nextReg
        nextReg := hiPop.payload
        hiPop.ready := True
        hiFrontReg := hiFrontReg + 1
        runningState := ProcessState.RUNNING

        // Clear scheduling requests
        termReq := False
        yieldReq := False
      } elsewhen (loPop.valid) {
        // Schedule low priority process
        currentProcReg := nextReg
        nextReg := loPop.payload
        loPop.ready := True
        loFrontReg := loFrontReg + 1
        runningState := ProcessState.RUNNING

        // Clear scheduling requests
        termReq := False
        yieldReq := False
      } otherwise {
        // No processes available - enter idle state
        runningState := ProcessState.WAITING
        schedulerActive := False
      }
    }

    // Re-activate scheduler when new processes arrive
    when(!schedulerActive && (hiFifo.io.occupancy > 0 || loFifo.io.occupancy > 0)) {
      schedulerActive := True
    }

    // Preemption: High priority process can interrupt low priority
    when(runningState === ProcessState.RUNNING && hiPop.valid && !termReq && !yieldReq) {
      // If currently running low priority and high priority becomes available
      when(currentProcReg =/= 0) { // Only preempt if something is actually running
        yieldReq := True // Trigger context switch
      }
    }

    println(s"[${SchedulerPlugin.this.getDisplayName()}] build end")
  }

  // Service methods implemented at class level
  override def newProc: Flow[SchedCmd] = {
    if (cmdReg != null) cmdReg else Flow(SchedCmd()).setIdle()
  }
  override def currentProc: UInt = {
    if (currentProcReg != null) currentProcReg else U(0, 32 bits)
  }
  override def nextProc: UInt = {
    if (nextReg != null) nextReg else U(0, 32 bits)
  }

  override def enqueue(ptr: UInt, high: Bool): Unit = {
    if (cmdReg != null) {
      cmdReg.valid := True
      cmdReg.payload.ptr := ptr
      cmdReg.payload.high := high
    }
  }

  override def terminateCurrent(): Unit = {
    if (termReq != null && runningState != null && processCountReg != null) {
      termReq := True
      runningState := ProcessState.TERMINATED
      when(processCountReg > 0) {
        processCountReg := processCountReg - 1
      }
    }
  }

  override def yieldCurrent(): Unit = {
    if (yieldReq != null && runningState != null) {
      yieldReq := True
      runningState := ProcessState.READY
    }
  }

  override def hasReady: Bool = {
    if (hiFifo != null && loFifo != null) hiFifo.io.occupancy > 0 || loFifo.io.occupancy > 0
    else False
  }
  override def hasHighPriority: Bool = {
    if (hiFifo != null) hiFifo.io.occupancy > 0 else False
  }
  override def hasLowPriority: Bool = {
    if (loFifo != null) loFifo.io.occupancy > 0 else False
  }

  override def isAnalyzing: Bool = False // Not used in basic scheduler
  override def updateRegisters(): Unit = {} // Registers update automatically

  override def hiFront: UInt = {
    if (hiFrontReg != null) hiFrontReg else U(0, 32 bits)
  }
  override def hiBack: UInt = {
    if (hiBackReg != null) hiBackReg else U(0, 32 bits)
  }
  override def loFront: UInt = {
    if (loFrontReg != null) loFrontReg else U(0, 32 bits)
  }
  override def loBack: UInt = {
    if (loBackReg != null) loBackReg else U(0, 32 bits)
  }

  override def saveHighQueue(): Unit = {
    if (hiSavedFront != null && hiSavedBack != null && hiFrontReg != null && hiBackReg != null) {
      hiSavedFront := hiFrontReg
      hiSavedBack := hiBackReg
    }
  }

  override def saveLowQueue(): Unit = {
    if (loSavedFront != null && loSavedBack != null && loFrontReg != null && loBackReg != null) {
      loSavedFront := loFrontReg
      loSavedBack := loBackReg
    }
  }

  override def restoreHighQueue(front: UInt, back: UInt): Unit = {
    if (hiFrontReg != null && hiBackReg != null) {
      hiFrontReg := front
      hiBackReg := back
    }
  }

  override def restoreLowQueue(front: UInt, back: UInt): Unit = {
    if (loFrontReg != null && loBackReg != null) {
      loFrontReg := front
      loBackReg := back
    }
  }

  override def getProcessState(ptr: UInt): ProcessState.C = {
    // Simplified - in full implementation would lookup in process table
    val state = ProcessState()
    state := ProcessState.READY // Default state for queued processes
    if (currentProcReg != null && runningState != null) {
      when(ptr === currentProcReg) {
        state := runningState
      }
    }
    state
  }

  override def setProcessState(ptr: UInt, state: ProcessState.C): Unit = {
    if (currentProcReg != null && runningState != null) {
      when(ptr === currentProcReg) {
        runningState := state
      }
    }
  }

  override def processCount(): UInt = {
    if (processCountReg != null) processCountReg else U(0, 16 bits)
  }
}

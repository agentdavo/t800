package transputer.plugins.schedule

import spinal.core._
import spinal.lib._
import spinal.core.fiber.Retainer

/** T9000 Process scheduler command bundle.
  *
  * Encapsulates process scheduling commands including workspace pointer, priority level, and
  * process state information.
  */
case class SchedCmd() extends Bundle {
  val ptr = UInt(transputer.Global.ADDR_BITS bits) // Workspace pointer
  val high = Bool() // Priority level (true = high priority)
}

/** T9000 Process states for scheduler state machine.
  *
  * T9000 processes can be in one of these states:
  *   - RUNNING: Currently executing process
  *   - READY: Process ready to run, queued in priority queue
  *   - WAITING: Process waiting for timer, channel, or other event
  *   - TERMINATED: Process has completed execution
  */
object ProcessState extends SpinalEnum {
  val RUNNING, READY, WAITING, TERMINATED = newElement()
}

/** T9000 Scheduler Service with hybrid signal-method architecture.
  *
  * The T9000 scheduler manages processes using:
  *   - High priority queue for time-critical processes
  *   - Low priority queue for normal processes
  *   - Timer-based scheduling integration
  *   - Process state management
  *   - Queue persistence for SAVEH/SAVEL operations
  *
  * This interface uses a hybrid approach:
  *   - Direct signal access for performance-critical scheduler state
  *   - Method interfaces for complex queue operations and state management
  *   - Proper SpinalHDL signal ownership patterns
  */
trait SchedService {
  // ========================================
  // DIRECT SIGNAL ACCESS (for performance-critical operations)
  // ========================================
  // These are mutable signals owned by the SchedulerPlugin
  // They can be read directly and assigned from other plugins
  def currentProc: UInt // Direct access to currently running process workspace pointer
  def nextProc: UInt // Direct access to next scheduled process (read-only)
  def hiFront: UInt // Direct access to high priority queue front pointer
  def hiBack: UInt // Direct access to high priority queue back pointer
  def loFront: UInt // Direct access to low priority queue front pointer
  def loBack: UInt // Direct access to low priority queue back pointer
  def hasReady: Bool // Direct access to ready state
  def hasHighPriority: Bool // Direct access to high priority queue status
  def hasLowPriority: Bool // Direct access to low priority queue status
  def isAnalyzing: Bool // Direct access to analysis mode status

  // ========================================
  // METHOD INTERFACES (for complex operations with state management)
  // ========================================

  /** Flow interface for new process creation */
  def newProc: Flow[SchedCmd]

  /** Enqueue a process into appropriate priority queue */
  def enqueue(ptr: UInt, high: Bool): Unit

  /** Terminate current process and schedule next */
  def terminateCurrent(): Unit

  /** Yield current process (STOPP instruction) */
  def yieldCurrent(): Unit

  /** Save high priority queue state (SAVEH) */
  def saveHighQueue(): Unit

  /** Save low priority queue state (SAVEL) */
  def saveLowQueue(): Unit

  /** Restore high priority queue state */
  def restoreHighQueue(front: UInt, back: UInt): Unit

  /** Restore low priority queue state */
  def restoreLowQueue(front: UInt, back: UInt): Unit

  /** Get current process state */
  def getProcessState(ptr: UInt): ProcessState.C

  /** Set process state */
  def setProcessState(ptr: UInt, state: ProcessState.C): Unit

  /** Process count for debugging */
  def processCount(): UInt

  // Signal update triggers (called when direct signal assignment occurs)
  def updateRegisters(): Unit // Sync signals back to register file
}

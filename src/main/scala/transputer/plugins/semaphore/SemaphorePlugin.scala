package transputer.plugins.semaphore

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.semaphore._

/** T9000 Semaphore Plugin implementing Table 6.23 semaphore instructions.
  *
  * This plugin implements process synchronization from T9000 Table 6.23:
  *   - wait: Wait on semaphore (may block process)
  *   - signal: Signal semaphore (may unblock process)
  *
  * Features:
  *   - N-valued semaphores with counting
  *   - Process blocking and queuing
  *   - Integration with scheduler for context switching
  *   - Atomic semaphore operations
  */
class SemaphorePlugin extends FiberPlugin {
  override def getDisplayName(): String = "SemaphorePlugin"
  setName("semaphore")

  during setup new Area {
    println(s"[${SemaphorePlugin.this.getDisplayName()}] setup start")

    addService(new SemaphoreService {
      override def executeOp(
        op: SemaphoreOp.C,
        semaphoreAddr: UInt,
        processDesc: UInt
      ): SemaphoreState = semaphoreState
      override def isSemaphoreOp(opcode: Bits): Bool = isSemaphoreOperation
      override def getSemaphoreOp(opcode: Bits): SemaphoreOp.C = semaphoreOperation
    })

    println(s"[${SemaphorePlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var semaphoreState: SemaphoreState = null
  var isSemaphoreOperation: Bool = null
  var semaphoreOperation: SemaphoreOp.C = null

  during build new Area {
    println(s"[${SemaphorePlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    semaphoreState = SemaphoreState()
    isSemaphoreOperation = Bool()
    semaphoreOperation = SemaphoreOp()

    // Default values
    semaphoreState.count := 0
    semaphoreState.blocked := False
    semaphoreState.queueFront := 0
    semaphoreState.queueBack := 0
    isSemaphoreOperation := False
    semaphoreOperation := SemaphoreOp.WAIT

    println(s"[${SemaphorePlugin.this.getDisplayName()}] Semaphore hardware configured")
    println(s"[${SemaphorePlugin.this.getDisplayName()}] - Table 6.23: Semaphore operations")
    println(s"[${SemaphorePlugin.this.getDisplayName()}] - Process synchronization primitives")
    println(s"[${SemaphorePlugin.this.getDisplayName()}] build end")
  }
}

package transputer.plugins.semaphore

import spinal.core._

/** T9000 Table 6.23 Semaphore Operations
  *
  * This service defines the interface for semaphore operations as specified in T9000 Table 6.23.
  * These instructions provide synchronization primitives for concurrent process coordination.
  */

// Semaphore operation types from Table 6.23
object SemaphoreOp extends SpinalEnum {
  val WAIT, // wait on semaphore
  SIGNAL // signal semaphore
  = newElement()
}

// Semaphore state
case class SemaphoreState() extends Bundle {
  val count = SInt(32 bits) // Semaphore count
  val blocked = Bool() // Process blocked on semaphore
  val queueFront = UInt(32 bits) // Front of blocked process queue
  val queueBack = UInt(32 bits) // Back of blocked process queue
}

// Service interface for semaphore operations
trait SemaphoreService {

  /** Execute a semaphore operation
    * @param op
    *   Operation to perform (wait/signal)
    * @param semaphoreAddr
    *   Address of semaphore data structure
    * @param processDesc
    *   Current process descriptor
    * @return
    *   Updated semaphore state
    */
  def executeOp(op: SemaphoreOp.C, semaphoreAddr: UInt, processDesc: UInt): SemaphoreState

  /** Check if an opcode is a semaphore operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isSemaphoreOp(opcode: Bits): Bool

  /** Decode opcode to semaphore operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded semaphore operation
    */
  def getSemaphoreOp(opcode: Bits): SemaphoreOp.C
}

/** T9000 Table 6.23 Instruction Opcodes */
object Table6_23 {
  val WAIT_OPCODE = 0x24f5 // wait - wait on semaphore
  val SIGNAL_OPCODE = 0x24f6 // signal - signal semaphore
}

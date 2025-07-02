package transputer.plugins.resources

import spinal.core._

/** T9000 Table 6.22 Resource Channel Operations
  *
  * This service defines the interface for resource channel operations as specified in T9000 Table
  * 6.22. These instructions manage resource allocation and synchronization through special resource
  * channels.
  */

// Resource operation types from Table 6.22
object ResourceOp extends SpinalEnum {
  val GRANT, // Grant resource
  ENBG, // Enable grant
  DISG, // Disable grant
  MKRC, // Mark resource channel
  UNMKRC, // Unmark resource channel
  IRDSQ, // Insert at front of RDS queue
  ERDSQ, // Empty RDS queue
  STRESPTR, // Store resource pointer
  LDRESPTR // Load resource pointer
  = newElement()
}

// Resource state
case class ResourceState() extends Bundle {
  val grantEnabled = Bool() // Grant mechanism enabled
  val resourceCount = UInt(8 bits) // Number of available resources
  val queueLength = UInt(8 bits) // RDS queue length
  val resourcePtr = UInt(32 bits) // Resource data structure pointer
}

// Resource result
case class ResourceResult() extends Bundle {
  val granted = Bool() // Resource was granted
  val resourceId = UInt(8 bits) // Granted resource ID
  val queueEmpty = Bool() // RDS queue is empty
  val error = Bool() // Resource error
}

// Service interface for resource operations
trait ResourceService {

  /** Execute a resource operation
    * @param op
    *   Operation to perform
    * @param channelId
    *   Resource channel ID
    * @param value
    *   Operation value
    * @return
    *   Resource result
    */
  def executeOp(op: ResourceOp.C, channelId: UInt, value: UInt): ResourceResult

  /** Check if an opcode is a resource operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isResourceOp(opcode: Bits): Bool

  /** Decode opcode to resource operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded resource operation
    */
  def getResourceOp(opcode: Bits): ResourceOp.C

  /** Get current resource state
    * @return
    *   Current resource state
    */
  def getState(): ResourceState
}

/** T9000 Table 6.22 Instruction Opcodes */
object Table6_22 {
  // Negative prefix opcodes
  val GRANT_OPCODE = 0x11f // grant - grant resource (hardware: -0x1F)
  val ENBG_OPCODE = 0x11e // enbg - enable grant (hardware: -0x1E)
  val DISG_OPCODE = 0x11d // disg - disable grant (hardware: -0x1D)
  val MKRC_OPCODE = 0x124 // mkrc - mark resource channel (hardware: -0x24)
  val UNMKRC_OPCODE = 0x123 // unmkrc - unmark resource channel (hardware: -0x23)
  val IRDSQ_OPCODE = 0x125 // irdsq - insert at front of RDS queue (hardware: -0x25)
  val ERDSQ_OPCODE = 0x126 // erdsq - empty RDS queue (hardware: -0x26)
  val STRESPTR_OPCODE = 0x127 // stresptr - store resource pointer (hardware: -0x27)
  val LDRESPTR_OPCODE = 0x128 // ldresptr - load resource pointer (hardware: -0x28)
}

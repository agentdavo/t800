package transputer.plugins.blockmove

import spinal.core._

/** T9000 Table 6.12 Block Move Operations
  *
  * This service defines the interface for block move operations as specified in T9000 Table 6.12.
  * These instructions provide efficient memory-to-memory transfers including 1D and 2D block moves.
  */

// Block move operation types from Table 6.12
object BlockMoveOp extends SpinalEnum {
  val MOVE, // 1D block move
  MOVE2DINIT, // 2D block move initialize
  MOVE2DALL, // 2D block move all
  MOVE2DNONZERO, // 2D block move non-zero
  MOVE2DZERO // 2D block move zero
  = newElement()
}

// Block move parameters
case class BlockMoveParams() extends Bundle {
  val sourceAddr = UInt(32 bits) // Source address
  val destAddr = UInt(32 bits) // Destination address
  val length = UInt(32 bits) // Length in bytes
  val srcStride = UInt(32 bits) // Source stride (2D)
  val destStride = UInt(32 bits) // Destination stride (2D)
  val rowLength = UInt(32 bits) // Row length (2D)
  val rowCount = UInt(32 bits) // Number of rows (2D)
}

// Block move result
case class BlockMoveResult() extends Bundle {
  val completed = Bool() // Operation completed
  val bytesTransferred = UInt(32 bits) // Bytes transferred
  val error = Bool() // Transfer error
  val cycles = UInt(16 bits) // Cycles taken
}

// Service interface for block move operations
trait BlockMoveService {

  /** Execute a block move operation
    * @param op
    *   Operation to perform
    * @param params
    *   Block move parameters
    * @return
    *   Block move result
    */
  def executeOp(op: BlockMoveOp.C, params: BlockMoveParams): BlockMoveResult

  /** Check if an opcode is a block move operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isBlockMoveOp(opcode: Bits): Bool

  /** Decode opcode to block move operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded block move operation
    */
  def getBlockMoveOp(opcode: Bits): BlockMoveOp.C

  /** Check if block move unit is busy
    * @return
    *   True if operation in progress
    */
  def isBusy(): Bool
}

/** T9000 Table 6.12 Instruction Opcodes */
object Table6_12 {
  val MOVE_OPCODE = 0x4a // move - block move
  val MOVE2DINIT_OPCODE = 0x21f5 // move2dinit - initialize 2D move
  val MOVE2DALL_OPCODE = 0x26fb // move2dall - 2D move all
  val MOVE2DNONZERO_OPCODE = 0x26fc // move2dnonzero - 2D move non-zero
  val MOVE2DZERO_OPCODE = 0x26fd // move2dzero - 2D move zero
}

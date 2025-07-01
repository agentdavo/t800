package transputer.plugins.indexing

import spinal.core._

/** T9000 Table 6.13 Indexing Operations
  *
  * This service defines the interface for array indexing and memory operations as specified in
  * T9000 Table 6.13. These instructions provide efficient array access with automatic bounds
  * checking and address calculation.
  */

// Indexing operation types from Table 6.13
object IndexingOp extends SpinalEnum {
  val BSUB, // byte subscript
  WSUB, // word subscript
  LB, // load byte
  SB, // store byte
  LSX, // load 16-bit and sign extend
  SS, // store 16-bit
  LDL, // load local
  STL, // store local
  LDNL, // load non-local
  STNL, // store non-local
  LDLP, // load local pointer
  LDNLP, // load non-local pointer
  LDPI, // load pointer to instruction
  GAJW, // general adjust workspace
  EQC, // equals constant
  STLB, // store local byte
  LDLB, // load local byte
  STNLB, // store non-local byte
  LDNLB // load non-local byte
  = newElement()
}

// Memory access size enumeration
object AccessSize extends SpinalEnum {
  val BYTE, WORD16, WORD32 = newElement()
}

// Indexing result with memory access information
case class IndexingResult() extends Bundle {
  val address = UInt(32 bits) // Calculated memory address
  val data = UInt(32 bits) // Data for store operations
  val accessSize = AccessSize() // Size of memory access
  val isLoad = Bool() // True for load, false for store
  val stackPush = Bool() // Push result to stack
  val stackPop = Bool() // Pop operand from stack
  val bounds_error = Bool() // Bounds checking error
}

// Service interface for indexing operations
trait IndexingService {

  /** Execute an indexing operation
    * @param op
    *   Operation to perform
    * @param baseAddress
    *   Base address for indexing
    * @param index
    *   Array index
    * @param data
    *   Data to store (for store operations)
    * @return
    *   Indexing result with memory access details
    */
  def executeOp(op: IndexingOp.C, baseAddress: UInt, index: UInt, data: UInt): IndexingResult

  /** Check if an opcode is an indexing operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isIndexingOp(opcode: Bits): Bool

  /** Decode opcode to indexing operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded indexing operation
    */
  def getIndexingOp(opcode: Bits): IndexingOp.C

  /** Calculate array address with bounds checking
    * @param baseAddr
    *   Base address of array
    * @param index
    *   Element index
    * @param elementSize
    *   Size of each element in bytes
    * @param arrayLength
    *   Length of array (for bounds checking)
    * @return
    *   Calculated address or bounds error
    */
  def calculateAddress(
    baseAddr: UInt,
    index: UInt,
    elementSize: UInt,
    arrayLength: UInt
  ): (UInt, Bool)
}

/** T9000 Table 6.13 Instruction Opcodes
  *
  * Memory codes and mnemonics from the T9000 specification:
  */
object Table6_13 {
  // Array subscript operations
  val BSUB_OPCODE = 0x21fc // bsub - byte subscript
  val WSUB_OPCODE = 0x25fa // wsub - word subscript

  // Byte operations
  val LB_OPCODE = 0x21f0 // lb - load byte
  val SB_OPCODE = 0x23f5 // sb - store byte
  val STLB_OPCODE = 0x21f3 // stlb - store local byte
  val LDLB_OPCODE = 0x21f2 // ldlb - load local byte
  val STNLB_OPCODE = 0x24f3 // stnlb - store non-local byte
  val LDNLB_OPCODE = 0x24f2 // ldnlb - load non-local byte

  // 16-bit operations
  val LS_OPCODE = 0x22f4 // ls - load 16-bit
  val SS_OPCODE = 0x22f5 // ss - store 16-bit

  // Local memory operations (primary instructions)
  val LDL_BASE = 0x10 // ldl 0-15 (4-bit immediate)
  val STL_BASE = 0x20 // stl 0-15 (4-bit immediate)
  val LDLP_BASE = 0x50 // ldlp 0-15 (4-bit immediate)

  // Non-local memory operations (primary instructions)
  val LDNL_BASE = 0x30 // ldnl 0-15 (4-bit immediate)
  val STNL_BASE = 0x40 // stnl 0-15 (4-bit immediate)
  val LDNLP_BASE = 0x60 // ldnlp 0-15 (4-bit immediate)

  // Extended addressing
  val GAJW_OPCODE = 0x23fc // gajw - general adjust workspace
  val EQC_OPCODE = 0xc0 // eqc - equals constant (primary)
}

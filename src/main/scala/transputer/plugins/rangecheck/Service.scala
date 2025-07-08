package transputer.plugins.rangecheck

import spinal.core._

/** T9000 Table 6.14 Range Checking and Conversion Operations
  *
  * This service defines the interface for range checking and type conversion operations as
  * specified in T9000 Table 6.14. These instructions provide safe array bounds checking and data
  * type conversions.
  */

// Range check operation types from Table 6.14
object RangeCheckOp extends SpinalEnum {
  val CIR, // check in range
  CB, // check byte
  CS, // check 16-bit
  CWORD, // check word
  XSWORD, // extend sign word
  CCNT1, // check count from 1
  CJ, // conditional jump
  CALL, // call subroutine
  CSNGL, // check single
  CDBL // check double
  = newElement()
}

// Range check result
case class RangeCheckResult() extends Bundle {
  val inRange = Bool() // Value is within range
  val converted = UInt(32 bits) // Converted value
  val rangeError = Bool() // Range check failed
  val jumpTaken = Bool() // Conditional jump taken
  val newIptr = UInt(32 bits) // New instruction pointer
}

// Service interface for range checking operations
trait RangeCheckService {

  /** Execute a range check operation
    * @param op
    *   Operation to perform
    * @param value
    *   Value to check/convert
    * @param lowerBound
    *   Lower bound for range check
    * @param upperBound
    *   Upper bound for range check
    * @return
    *   Range check result
    */
  def executeOp(
    op: RangeCheckOp.C,
    value: UInt,
    lowerBound: UInt,
    upperBound: UInt
  ): RangeCheckResult

  /** Check if an opcode is a range check operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isRangeCheckOp(opcode: Bits): Bool

  /** Decode opcode to range check operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded range check operation
    */
  def getRangeCheckOp(opcode: Bits): RangeCheckOp.C
}

/** T9000 Table 6.14 Instruction Opcodes */
object Table6_14 {
  // Secondary opcodes (accessed via OPR 0xF)
  val CIR_OPCODE = 0xc7 // cir - check in range
  val CB_OPCODE = 0xba // cb - check byte
  val CS_OPCODE = 0xfa // cs - check 16-bit
  val CWORD_OPCODE = 0x56 // cword - check word
  val XSWORD_OPCODE = 0xf8 // xsword - extend sign word
  val CCNT1_OPCODE = 0x4d // ccnt1 - check count from 1 (was CCNT)
  val CSNGL_OPCODE = 0x4c // csngl - check single
  val CDBL_OPCODE = 0xff // cdbl - check double (not in standard list, using placeholder)

  // Primary opcodes (direct 4-bit opcodes)
  val CJ_OPCODE = 0xa // cj - conditional jump (primary)
  val CALL_OPCODE = 0x9 // call - call subroutine (primary)
}

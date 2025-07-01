package transputer.plugins.longarith

import spinal.core._

/** T9000 Table 6.10 Long Arithmetic Operations
  *
  * This service defines the interface for 64-bit long arithmetic operations as specified in T9000
  * Table 6.10. These instructions provide extended precision arithmetic using the dual-word stack
  * model.
  */

// Long arithmetic operation types from Table 6.10
object LongArithOp extends SpinalEnum {
  val LADD, // long add
  LSUB, // long subtract
  LMUL, // long multiply
  LDIV, // long divide
  LSHL, // long shift left
  LSHR, // long shift right
  LSUM, // long sum
  LDIFF, // long difference
  MINT, // minimum integer
  BINT, // Boolean integer
  XSWORD, // extend sign word
  RESCHEDULE, // reschedule
  SLMUL, // signed long multiply
  SULMUL, // signed/unsigned long multiply
  XDBLE, // extend double
  XWORD, // extend word
  NORMALISE, // normalize (find leading bit)
  PROD // multiply step
  = newElement()
}

// Long arithmetic precision
object LongPrecision extends SpinalEnum {
  val WORD32, WORD64 = newElement()
}

// Long arithmetic result with status
case class LongArithResult() extends Bundle {
  val resultLow = UInt(32 bits) // Lower 32 bits of result
  val resultHigh = UInt(32 bits) // Upper 32 bits of result
  val overflow = Bool() // Arithmetic overflow
  val underflow = Bool() // Arithmetic underflow
  val zero = Bool() // Result is zero
  val negative = Bool() // Result is negative
  val carry = Bool() // Carry from operation
  val precision = LongPrecision() // Result precision
  val stackOperation = Bits(2 bits) // Stack manipulation needed
}

// Service interface for long arithmetic operations
trait LongArithService {

  /** Execute a long arithmetic operation
    * @param op
    *   Operation to perform
    * @param operandALow
    *   Low 32 bits of first operand
    * @param operandAHigh
    *   High 32 bits of first operand
    * @param operandBLow
    *   Low 32 bits of second operand
    * @param operandBHigh
    *   High 32 bits of second operand
    * @return
    *   Long arithmetic result with status
    */
  def executeOp(
    op: LongArithOp.C,
    operandALow: UInt,
    operandAHigh: UInt,
    operandBLow: UInt,
    operandBHigh: UInt
  ): LongArithResult

  /** Check if an opcode is a long arithmetic operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isLongArithOp(opcode: Bits): Bool

  /** Decode opcode to long arithmetic operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded long arithmetic operation
    */
  def getLongArithOp(opcode: Bits): LongArithOp.C

  /** Check if operation is busy (multi-cycle)
    * @return
    *   True if long arithmetic unit is executing
    */
  def isBusy(): Bool

  /** Get cycle count for operation
    * @param op
    *   Operation type
    * @return
    *   Number of cycles required
    */
  def getCycleCount(op: LongArithOp.C): UInt
}

/** T9000 Table 6.10 Instruction Opcodes
  *
  * Memory codes and mnemonics from the T9000 specification:
  */
object Table6_10 {
  // Basic long arithmetic
  val LADD_OPCODE = 0x26f0 // ladd - long add
  val LSUB_OPCODE = 0x26f1 // lsub - long subtract
  val LMUL_OPCODE = 0x26f2 // lmul - long multiply
  val LDIV_OPCODE = 0x21fe // ldiv - long divide

  // Long shifts
  val LSHL_OPCODE = 0x26f3 // lshl - long shift left
  val LSHR_OPCODE = 0x26f4 // lshr - long shift right

  // Extended operations
  val LSUM_OPCODE = 0x26f5 // lsum - long sum
  val LDIFF_OPCODE = 0x26f6 // ldiff - long difference

  // Constants and conversions
  val MINT_OPCODE = 0x24f2 // mint - minimum integer
  val BINT_OPCODE = 0x25f1 // bint - Boolean to integer
  val XSWORD_OPCODE = 0x26ff // xsword - extend sign word
  val XDBLE_OPCODE = 0x27f5 // xdble - extend double
  val XWORD_OPCODE = 0x27f6 // xword - extend word

  // Specialized operations
  val RESCHEDULE_OPCODE = 0x22f0 // reschedule
  val SLMUL_OPCODE = 0x26f7 // slmul - signed long multiply
  val SULMUL_OPCODE = 0x26f8 // sulmul - signed/unsigned long multiply
  val NORMALISE_OPCODE = 0x25fb // normalise - normalize
  val PROD_OPCODE = 0xf8 // prod - multiply step
}

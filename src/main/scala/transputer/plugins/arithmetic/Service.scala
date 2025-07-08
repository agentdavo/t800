package transputer.plugins.arithmetic

import spinal.core._

/** T9000 Table 6.9 Arithmetic and Logical Operations
  *
  * This service defines the interface for basic arithmetic and logical operations as specified in
  * T9000 Table 6.9. All operations work with the 3-register evaluation stack (Areg, Breg, Creg).
  */

// ALU operation types from Table 6.9
object AluOp extends SpinalEnum {
  val ADD, SUB, MUL, DIV, REM, // Arithmetic operations
  AND, OR, XOR, NOT, // Logical operations
  SHL, SHR, // Shift operations
  GT, GTU, // Comparison operations
  DIFF, SUM, PROD, // Extended arithmetic
  REV, DUP, // Stack operations
  FMUL = newElement() // Fractional multiply
}

// ALU result with status flags
case class AluResult() extends Bundle {
  val result = UInt(32 bits)
  val overflow = Bool() // Arithmetic overflow
  val underflow = Bool() // Arithmetic underflow
  val zero = Bool() // Result is zero
  val negative = Bool() // Result is negative
  val carry = Bool() // Carry/borrow flag
}

// Service interface for arithmetic operations
trait AluService {

  /** Execute an arithmetic/logical operation
    * @param op
    *   Operation to perform
    * @param operandA
    *   First operand (top of stack)
    * @param operandB
    *   Second operand (second on stack)
    * @return
    *   Result with status flags
    */
  def executeOp(op: AluOp.C, operandA: UInt, operandB: UInt): AluResult

  /** Check if an opcode is an arithmetic/logical operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isAluOp(opcode: Bits): Bool

  /** Decode opcode to ALU operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded ALU operation
    */
  def getAluOp(opcode: Bits): AluOp.C

  /** Check if ALU is busy (for multi-cycle operations)
    * @return
    *   True if ALU is currently executing
    */
  def isBusy(): Bool
}

/** T9000 Table 6.9 Instruction Opcodes
  *
  * Memory codes and mnemonics from the T9000 specification:
  */
object Table6_9 {
  // Logical operations
  val AND_OPCODE = 0x24f6 // and
  val OR_OPCODE = 0x24fb // or
  val XOR_OPCODE = 0x23f3 // xor
  val NOT_OPCODE = 0x23f2 // not
  val SHL_OPCODE = 0x24f1 // shl
  val SHR_OPCODE = 0x24f0 // shr

  // Arithmetic operations
  val ADD_OPCODE = 0xf5 // add
  val SUB_OPCODE = 0xfc // sub
  val MUL_OPCODE = 0x25f3 // mul
  val FMUL_OPCODE = 0x27f2 // fmul (fractional multiply)
  val DIV_OPCODE = 0x22fc // div
  val REM_OPCODE = 0x21ff // rem

  // Comparison operations
  val GT_OPCODE = 0xf9 // gt
  val GTU_OPCODE = 0x25f5 // gtu (greater than unsigned)

  // Extended arithmetic
  val DIFF_OPCODE = 0xf4 // diff
  val SUM_OPCODE = 0x25f2 // sum
  val PROD_OPCODE = 0xf8 // prod
}

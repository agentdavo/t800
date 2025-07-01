package transputer.plugins.controlflow

import spinal.core._

/** T9000 Table 6.11 Control Flow Operations
  *
  * This service defines the interface for control flow operations as specified in T9000 Table 6.11.
  * These instructions manage function calls, returns, workspace adjustment, and loop control.
  */

// Control flow operation types from Table 6.11
object ControlFlowOp extends SpinalEnum {
  val RET, // return
  LDPI, // load pointer to instruction
  GAJW, // general adjust workspace
  GCALL, // general call
  LEND, // loop end
  ENDP, // end process
  DISS, // disconnect stack
  STHF, // store high priority front pointer
  STLF, // store low priority front pointer
  STHB, // store high priority back pointer
  STLB, // store low priority back pointer
  SAVEL, // save low priority queue
  SAVEH, // save high priority queue
  WCNT, // word count
  SHR, // shift right
  SHL, // shift left
  NORM, // normalize
  LDIV, // long divide
  LDIVSTEP, // long divide step
  UNPACK_SNS, // unpack single length real to SNS format
  POSTNORMSN, // post normalize correction SN
  ROUNDSN, // round single length to SN
  LDINF // load infinity
  = newElement()
}

// Control flow result with branch information
case class ControlFlowResult() extends Bundle {
  val newIptr = UInt(32 bits) // New instruction pointer
  val newWptr = UInt(32 bits) // New workspace pointer (for ajw/call)
  val takeJump = Bool() // Whether to take the jump/branch
  val returnValue = UInt(32 bits) // Return value for stack
  val stackOperation = Bits(2 bits) // 00=none, 01=push, 10=pop, 11=replace_top
}

// Service interface for control flow operations
trait ControlFlowService {

  /** Execute a control flow operation
    * @param op
    *   Operation to perform
    * @param operandA
    *   First operand (top of stack)
    * @param operandB
    *   Second operand (second on stack)
    * @param currentIptr
    *   Current instruction pointer
    * @param currentWptr
    *   Current workspace pointer
    * @return
    *   Control flow result with new PC/workspace
    */
  def executeOp(
    op: ControlFlowOp.C,
    operandA: UInt,
    operandB: UInt,
    currentIptr: UInt,
    currentWptr: UInt
  ): ControlFlowResult

  /** Check if an opcode is a control flow operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isControlFlowOp(opcode: Bits): Bool

  /** Decode opcode to control flow operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded control flow operation
    */
  def getControlFlowOp(opcode: Bits): ControlFlowOp.C
}

/** T9000 Table 6.11 Instruction Opcodes
  *
  * Memory codes and mnemonics from the T9000 specification:
  */
object Table6_11 {
  // Control flow instructions
  val RET_OPCODE = 0x20f0 // ret
  val LDPI_OPCODE = 0x21f7 // ldpi
  val GAJW_OPCODE = 0x23fc // gajw
  val GCALL_OPCODE = 0x26f6 // gcall
  val LEND_OPCODE = 0x21f1 // lend
  val ENDP_OPCODE = 0xf3 // endp
  val DISS_OPCODE = 0x22f1 // diss

  // Stack/queue management
  val STHF_OPCODE = 0x21f8 // sthf
  val STLF_OPCODE = 0x21f9 // stlf
  val STHB_OPCODE = 0x21fa // sthb
  val STLB_OPCODE = 0x21fb // stlb
  val SAVEL_OPCODE = 0x22f8 // savel
  val SAVEH_OPCODE = 0x22f9 // saveh

  // Utility operations
  val WCNT_OPCODE = 0x23f1 // wcnt
  val SHR_OPCODE = 0x24f0 // shr
  val SHL_OPCODE = 0x24f1 // shl
  val NORM_OPCODE = 0x25f8 // norm

  // Long arithmetic support
  val LDIV_OPCODE = 0x21fe // ldiv
  val LDIVSTEP_OPCODE = 0x21f6 // ldivstep

  // Floating-point support
  val UNPACK_SNS_OPCODE = 0x63f1 // unpacksns
  val POSTNORMSN_OPCODE = 0x62f3 // postnormsn
  val ROUNDSN_OPCODE = 0x66f1 // roundsn
  val LDINF_OPCODE = 0x21f4 // ldinf
}

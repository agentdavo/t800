package transputer.plugins.interrupts

import spinal.core._

/** T9000 Table 6.27 Interrupt Operations
  *
  * This service defines the interface for interrupt handling operations as specified in T9000 Table
  * 6.27. These instructions manage hardware interrupts and shadow register sets.
  */

// Interrupt operation types from Table 6.27
object InterruptOp extends SpinalEnum {
  val INTDIS, // Disable interrupts
  INTENB, // Enable interrupts
  LDTRAPPED, // Load trapped process
  STTRAPPED, // Store trapped process
  LDSHADOW, // Load shadow registers
  STSHADOW, // Store shadow registers
  RESTART, // Restart from interrupt
  CAUSEERROR // Cause error (testing)
  = newElement()
}

// Interrupt state
case class InterruptState() extends Bundle {
  val enabled = Bool() // Interrupts enabled
  val pending = Bits(8 bits) // Pending interrupt mask
  val inInterrupt = Bool() // Currently in interrupt handler
  val shadowActive = Bool() // Shadow registers active
  val savedStatus = UInt(32 bits) // Saved status register
}

// Interrupt result
case class InterruptResult() extends Bundle {
  val taken = Bool() // Interrupt taken
  val vector = UInt(8 bits) // Interrupt vector
  val shadowSwitched = Bool() // Shadow registers switched
  val error = Bool() // Interrupt error
}

// Service interface for interrupt operations
trait InterruptService {

  /** Execute an interrupt operation
    * @param op
    *   Operation to perform
    * @param value
    *   Operation value (vector, mask, etc.)
    * @return
    *   Interrupt result
    */
  def executeOp(op: InterruptOp.C, value: UInt): InterruptResult

  /** Check if an opcode is an interrupt operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isInterruptOp(opcode: Bits): Bool

  /** Decode opcode to interrupt operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded interrupt operation
    */
  def getInterruptOp(opcode: Bits): InterruptOp.C

  /** Get current interrupt state
    * @return
    *   Current interrupt state
    */
  def getState(): InterruptState

  /** Request interrupt
    * @param vector
    *   Interrupt vector number
    */
  def requestInterrupt(vector: UInt): Unit
}

/** T9000 Table 6.27 Instruction Opcodes */
object Table6_27 {
  // Secondary opcodes (via OPR 0xF)
  val INTDIS_OPCODE = 0xc4 // intdis - disable interrupts
  val INTENB_OPCODE = 0xc5 // intenb - enable interrupts
  val LDTRAPPED_OPCODE = 0xca // ldtrapped - load trapped process
  val STTRAPPED_OPCODE = 0xcb // sttrapped - store trapped process

  // Negative prefix opcodes
  val LDSHADOW_OPCODE = 0x104 // ldshadow - load shadow registers (hardware: -0x04)
  val STSHADOW_OPCODE = 0x103 // stshadow - store shadow registers (hardware: -0x03)
  val RESTART_OPCODE = 0x120 // restart - restart from interrupt (hardware: -0x20)
  val CAUSEERROR_OPCODE = 0x121 // causeerror - cause error (hardware: -0x21)
}

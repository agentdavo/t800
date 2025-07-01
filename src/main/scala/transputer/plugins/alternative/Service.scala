package transputer.plugins.alternative

import spinal.core._

/** T9000 Table 6.24 Alternative (ALT) Operations
  *
  * This service defines the interface for alternative construct operations as specified in T9000
  * Table 6.24. These instructions implement the transputer's ALT construct for non-deterministic
  * choice.
  */

// ALT operation types from Table 6.24
object AltOp extends SpinalEnum {
  val ALT, // start alternative
  ALTWT, // alternative wait
  ALTEND, // end alternative
  ENBC, // enable channel
  DISC, // disable channel
  ENBT, // enable timer
  DIST, // disable timer
  TALT, // timer alternative
  TALTWT // timer alternative wait
  = newElement()
}

// ALT state
case class AltState() extends Bundle {
  val enabled = Bool() // ALT construct enabled
  val waiting = Bool() // Waiting for guard
  val selectedGuard = UInt(8 bits) // Selected guard index
  val guardCount = UInt(8 bits) // Number of guards
  val timerEnabled = Bool() // Timer guard enabled
}

// Service interface for ALT operations
trait AltService {

  /** Execute an ALT operation
    * @param op
    *   Operation to perform
    * @param guardIndex
    *   Guard index
    * @param channelAddr
    *   Channel address
    * @return
    *   ALT state
    */
  def executeOp(op: AltOp.C, guardIndex: UInt, channelAddr: UInt): AltState

  /** Check if an opcode is an ALT operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isAltOp(opcode: Bits): Bool

  /** Decode opcode to ALT operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded ALT operation
    */
  def getAltOp(opcode: Bits): AltOp.C
}

/** T9000 Table 6.24 Instruction Opcodes */
object Table6_24 {
  val ALT_OPCODE = 0x43 // alt - start alternative
  val ALTWT_OPCODE = 0x44 // altwt - alternative wait
  val ALTEND_OPCODE = 0x45 // altend - end alternative
  val ENBC_OPCODE = 0x48 // enbc - enable channel
  val DISC_OPCODE = 0x49 // disc - disable channel
  val ENBT_OPCODE = 0x47 // enbt - enable timer
  val DIST_OPCODE = 0x4c // dist - disable timer
  val TALT_OPCODE = 0x24f4 // talt - timer alternative
  val TALTWT_OPCODE = 0x21ec // taltwt - timer alternative wait
}

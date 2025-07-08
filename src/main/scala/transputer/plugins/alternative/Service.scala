package transputer.plugins.alternative

import spinal.core._

/** T9000 Table 6.24 Alternative (ALT) Operations
  *
  * This service defines the interface for alternative/select operations as specified in T9000 Table
  * 6.24. These instructions implement the ALT construct for non-deterministic choice between
  * multiple channel communications.
  */

// Alternative operation types from Table 6.24
object AltOp extends SpinalEnum {
  val ALT, // Start alternative
  ALTWT, // Alternative wait
  ALTEND, // End alternative
  ENBC, // Enable channel
  DISC, // Disable channel
  DISS, // Disable skip
  ENBS, // Enable skip
  ENBT, // Enable timer
  DIST, // Disable timer
  TALT, // Timer alternative
  TALTWT // Timer alternative wait
  = newElement()
}

// Alternative state
case class AltState() extends Bundle {
  val inAlt = Bool() // Currently in ALT construct
  val waiting = Bool() // Waiting for channel ready
  val selectedChannel = UInt(8 bits) // Which channel was selected
  val skipEnabled = Bool() // Skip guard enabled
  val timerEnabled = Bool() // Timer guard enabled
}

// Alternative result
case class AltResult() extends Bundle {
  val ready = Bool() // A channel is ready
  val channelId = UInt(8 bits) // Ready channel ID
  val isTimer = Bool() // Timer expired
  val skipTaken = Bool() // Skip guard taken
  val error = Bool() // Alternative error
}

// Service interface for alternative operations
trait AlternativeService {

  /** Execute an alternative operation
    * @param op
    *   Operation to perform
    * @param channelMask
    *   Bit mask of enabled channels
    * @param timerValue
    *   Timer value for timed alternatives
    * @return
    *   Alternative result
    */
  def executeOp(op: AltOp.C, channelMask: Bits, timerValue: UInt): AltResult

  /** Check if an opcode is an alternative operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isAltOp(opcode: Bits): Bool

  /** Decode opcode to alternative operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded alternative operation
    */
  def getAltOp(opcode: Bits): AltOp.C

  /** Get current alternative state
    * @return
    *   Current ALT state
    */
  def getState(): AltState
}

/** T9000 Table 6.24 Instruction Opcodes */
object Table6_24 {
  // Secondary opcodes (via OPR 0xF)
  val ALT_OPCODE = 0x43 // alt - start alternative
  val ALTWT_OPCODE = 0x44 // altwt - alternative wait
  val ALTEND_OPCODE = 0x45 // altend - end alternative
  val ENBC_OPCODE = 0x48 // enbc - enable channel
  val DISC_OPCODE = 0x2f // disc - disable channel
  val DISS_OPCODE = 0x30 // diss - disable skip
  val ENBS_OPCODE = 0x49 // enbs - enable skip
  val ENBT_OPCODE = 0x47 // enbt - enable timer
  val DIST_OPCODE = 0x2e // dist - disable timer
  val TALT_OPCODE = 0x4e // talt - timer alternative
  val TALTWT_OPCODE = 0x51 // taltwt - timer alternative wait
}

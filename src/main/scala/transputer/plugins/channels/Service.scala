package transputer.plugins.channels

import spinal.core._

/** T9000 Table 6.21 Channel Operations
  *
  * This service defines the interface for channel management operations as specified in T9000 Table
  * 6.21. These instructions control virtual channels and communication links.
  */

// Channel operation types from Table 6.21
object ChannelOp extends SpinalEnum {
  val CHANTYPE, // Get channel type
  INITVLCB, // Initialize virtual link control block
  SETCHMODE, // Set channel mode
  SETHDR, // Set header
  WRITEHDR, // Write header
  READHDR, // Read header
  SWAPBFR, // Swap buffer
  UNMKRC, // Unmark resource channel
  MKRC // Mark resource channel
  = newElement()
}

// Channel types
object ChannelType extends SpinalEnum {
  val PHYSICAL, // Physical link
  VIRTUAL, // Virtual channel
  RESOURCE, // Resource channel
  INVALID // Invalid channel
  = newElement()
}

// Channel mode flags
case class ChannelMode() extends Bundle {
  val enabled = Bool() // Channel enabled
  val priority = UInt(2 bits) // Channel priority
  val buffered = Bool() // Buffered mode
  val packetized = Bool() // Packet mode
}

// Channel result
case class ChannelResult() extends Bundle {
  val channelType = ChannelType() // Channel type
  val mode = ChannelMode() // Channel mode
  val header = UInt(32 bits) // Channel header
  val status = UInt(32 bits) // Channel status
  val error = Bool() // Operation error
}

// Service interface for channel operations
trait ChannelService {

  /** Execute a channel operation
    * @param op
    *   Operation to perform
    * @param channelId
    *   Channel identifier
    * @param value
    *   Operation value (mode, header, etc.)
    * @return
    *   Channel result
    */
  def executeOp(op: ChannelOp.C, channelId: UInt, value: UInt): ChannelResult

  /** Check if an opcode is a channel operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isChannelOp(opcode: Bits): Bool

  /** Decode opcode to channel operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded channel operation
    */
  def getChannelOp(opcode: Bits): ChannelOp.C
}

/** T9000 Table 6.21 Instruction Opcodes */
object Table6_21 {
  // Secondary opcodes (via OPR 0xF)
  val CHANTYPE_OPCODE = 0xc9 // chantype - get channel type

  // Negative prefix opcodes
  val INITVLCB_OPCODE = 0x11a // initvlcb - initialize VLCB (hardware: -0x1A)
  val SETCHMODE_OPCODE = 0x119 // setchmode - set channel mode (hardware: -0x19)
  val SETHDR_OPCODE = 0x118 // sethdr - set header (hardware: -0x18)
  val WRITEHDR_OPCODE = 0x11b // writehdr - write header (hardware: -0x1B)
  val READHDR_OPCODE = 0x11c // readhdr - read header (hardware: -0x1C)
  val SWAPBFR_OPCODE = 0x117 // swapbfr - swap buffer (hardware: -0x17)
  val UNMKRC_OPCODE = 0x123 // unmkrc - unmark resource channel (hardware: -0x23)
  val MKRC_OPCODE = 0x124 // mkrc - mark resource channel (hardware: -0x24)
}

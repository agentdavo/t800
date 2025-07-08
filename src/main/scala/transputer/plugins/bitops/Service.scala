package transputer.plugins.bitops

import spinal.core._

/** T9000 Table 6.16 Bit Operations and CRC
  *
  * This service defines the interface for bit manipulation and CRC operations as specified in T9000
  * Table 6.16. These instructions provide efficient bit-level operations and checksum calculations.
  */

// Bit operation types from Table 6.16
object BitOp extends SpinalEnum {
  val CRCWORD, // CRC word
  CRCBYTE, // CRC byte
  BITCNT, // count bits
  BITREVWORD, // reverse bits in word
  BITREVNBITS // reverse n bits
  = newElement()
}

// Bit operation result
case class BitOpResult() extends Bundle {
  val result = UInt(32 bits) // Operation result
  val bitCount = UInt(6 bits) // Number of bits set
  val crcValue = UInt(32 bits) // CRC checksum
  val completed = Bool() // Operation completed
}

// Service interface for bit operations
trait BitOpService {

  /** Execute a bit operation
    * @param op
    *   Operation to perform
    * @param data
    *   Input data
    * @param polynomial
    *   CRC polynomial (for CRC operations)
    * @return
    *   Bit operation result
    */
  def executeOp(op: BitOp.C, data: UInt, polynomial: UInt): BitOpResult

  /** Check if an opcode is a bit operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isBitOp(opcode: Bits): Bool

  /** Decode opcode to bit operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded bit operation
    */
  def getBitOp(opcode: Bits): BitOp.C
}

/** T9000 Table 6.16 Instruction Opcodes */
object Table6_16 {
  val CRCWORD_OPCODE = 0x25f9 // crcword - CRC word
  val CRCBYTE_OPCODE = 0x25fa // crcbyte - CRC byte
  val BITCNT_OPCODE = 0x25fb // bitcnt - count bits
  val BITREVWORD_OPCODE = 0x25fc // bitrevword - reverse bits in word
  val BITREVNBITS_OPCODE = 0x25fd // bitrevnbits - reverse n bits
}

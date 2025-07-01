package transputer.plugins.io

import spinal.core._

/** T9000 Tables 6.19-6.20 Input/Output Operations
  *
  * This service defines the interface for channel I/O operations as specified in T9000 Tables
  * 6.19-6.20. These instructions provide communication primitives for transputer-to-transputer
  * communication.
  */

// I/O operation types from Tables 6.19-6.20
object IOOp extends SpinalEnum {
  val IN, // channel input
  OUT, // channel output
  OUTWORD, // output word
  OUTBYTE, // output byte
  VIN, // vector input
  VOUT, // vector output
  TIN, // timer input
  TALT, // timer alternative
  TALTWT, // timer alternative wait
  ENBS, // enable skip
  DISS, // disable skip
  RESETCH // reset channel
  = newElement()
}

// I/O result
case class IOResult() extends Bundle {
  val completed = Bool() // Operation completed
  val dataTransferred = UInt(32 bits) // Data transferred
  val channelReady = Bool() // Channel ready for next operation
  val error = Bool() // Transfer error
}

// Service interface for I/O operations
trait IOService {

  /** Execute an I/O operation
    * @param op
    *   Operation to perform
    * @param channelAddr
    *   Channel address
    * @param data
    *   Data to transfer
    * @param length
    *   Transfer length
    * @return
    *   I/O result
    */
  def executeOp(op: IOOp.C, channelAddr: UInt, data: UInt, length: UInt): IOResult

  /** Check if an opcode is an I/O operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isIOOp(opcode: Bits): Bool

  /** Decode opcode to I/O operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded I/O operation
    */
  def getIOOp(opcode: Bits): IOOp.C
}

/** T9000 Tables 6.19-6.20 Instruction Opcodes */
object Table6_19_20 {
  val IN_OPCODE = 0x07 // in - channel input
  val OUT_OPCODE = 0x0b // out - channel output
  val OUTWORD_OPCODE = 0x21fd // outword - output word
  val OUTBYTE_OPCODE = 0x21fc // outbyte - output byte
  val VIN_OPCODE = 0x24f7 // vin - vector input
  val VOUT_OPCODE = 0x24f8 // vout - vector output
  val TIN_OPCODE = 0x21eb // tin - timer input
  val TALT_OPCODE = 0x24f4 // talt - timer alternative
  val TALTWT_OPCODE = 0x21ec // taltwt - timer alternative wait
  val ENBS_OPCODE = 0x21f4 // enbs - enable skip
  val DISS_OPCODE = 0x22f1 // diss - disable skip
  val RESETCH_OPCODE = 0x22f2 // resetch - reset channel
}

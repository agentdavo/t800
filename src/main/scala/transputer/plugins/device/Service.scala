package transputer.plugins.device

import spinal.core._

/** T9000 Table 6.15 Device Access Operations
  *
  * This service defines the interface for device access operations as specified in T9000 Table
  * 6.15. These instructions provide direct hardware access for memory-mapped I/O devices.
  */

// Device operation types from Table 6.15
object DeviceOp extends SpinalEnum {
  val DEVLB, // device load byte
  DEVLS, // device load 16-bit
  DEVLW, // device load word
  DEVSB, // device store byte
  DEVSS, // device store 16-bit
  DEVSW // device store word
  = newElement()
}

// Device access result
case class DeviceResult() extends Bundle {
  val data = UInt(32 bits) // Data read from device
  val completed = Bool() // Access completed
  val error = Bool() // Device error
  val busError = Bool() // Bus error
}

// Service interface for device operations
trait DeviceService {

  /** Execute a device access operation
    * @param op
    *   Operation to perform
    * @param deviceAddr
    *   Device address
    * @param data
    *   Data to write (for store operations)
    * @return
    *   Device access result
    */
  def executeOp(op: DeviceOp.C, deviceAddr: UInt, data: UInt): DeviceResult

  /** Check if an opcode is a device operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isDeviceOp(opcode: Bits): Bool

  /** Decode opcode to device operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded device operation
    */
  def getDeviceOp(opcode: Bits): DeviceOp.C
}

/** T9000 Table 6.15 Instruction Opcodes */
object Table6_15 {
  val DEVLB_OPCODE = 0x22f6 // devlb - device load byte
  val DEVLS_OPCODE = 0x22f7 // devls - device load 16-bit
  val DEVLW_OPCODE = 0x22f8 // devlw - device load word
  val DEVSB_OPCODE = 0x22f9 // devsb - device store byte
  val DEVSS_OPCODE = 0x22fa // devss - device store 16-bit
  val DEVSW_OPCODE = 0x22fb // devsw - device store word
}

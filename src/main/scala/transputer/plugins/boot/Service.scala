package transputer.plugins.boot

import spinal.core._
import spinal.lib._

/** Service interface for boot ROM functionality.
  *
  * Provides access to boot ROM configuration and memory mapping.
  */
trait BootRomService {

  /** Check if boot ROM is enabled */
  def isEnabled: Bool

  /** Get boot ROM start address */
  def startAddress: UInt

  /** Get boot ROM size in bytes */
  def size: Int

  /** Check if an address is within boot ROM range */
  def isBootRomAddress(address: UInt): Bool

  /** Get boot ROM bus interface (if available) */
  def bootRomBus: Option[SparseMemBus]
}

/** Sparse memory bus for boot ROM access */
case class SparseMemBus(addressWidth: Int, dataWidth: Int) extends Bundle with IMasterSlave {
  val enable = Bool()
  val write = Bool()
  val address = UInt(addressWidth bits)
  val writeData = Bits(dataWidth bits)
  val writeMask = Bits(dataWidth / 8 bits)
  val readData = Bits(dataWidth bits)

  override def asMaster(): Unit = {
    out(enable, write, address, writeData, writeMask)
    in(readData)
  }
}

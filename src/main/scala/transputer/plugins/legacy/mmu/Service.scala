package transputer.plugins.legacy.mmu

import spinal.core._
import spinal.lib._
import transputer.Global

case class TrapHandlerService() extends Bundle {
  val trapAddr = Bits(Global.ADDR_BITS bits)
  val trapType = Bits(4 bits)
  val trapEnable = Bool()
  val trapHandlerAddr = Bits(Global.ADDR_BITS bits)
  def setTrap(addr: Bits, typ: Bits): Unit = {
    trapAddr := addr
    trapType := typ
    trapEnable := True
  }
  def clearTrap(): Unit = trapEnable := False
}

trait ConfigAccessService {
  def addr: Bits
  def data: Bits
  def writeEnable: Bool
  def isValid: Bool
  def read(addr: Bits, width: Int): Bits
  def write(addr: Bits, data: Bits, width: Int): Unit
}

trait AddressTranslationService {
  def translate(addr: Bits): Bits
}

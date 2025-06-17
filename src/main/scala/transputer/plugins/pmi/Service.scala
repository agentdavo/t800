package transputer.plugins.pmi

import spinal.core._

case class PmiAccessSrv() extends Bundle {
  val addr = Bits(32 bits)
  val dataIn = Bits(64 bits)
  val dataOut = Bits(64 bits)
  val writeEnable = Bool()
  val isValid = Bool()
}

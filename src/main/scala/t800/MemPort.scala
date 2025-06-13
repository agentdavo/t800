package t800

import spinal.core._

/** Generic memory port command definitions. */
case class MemReadCmd() extends Bundle {
  val addr = UInt(Global.ADDR_BITS() bits)
}

case class MemWriteCmd() extends Bundle {
  val addr = UInt(Global.ADDR_BITS() bits)
  val data = Bits(Global.WORD_BITS() bits)
}

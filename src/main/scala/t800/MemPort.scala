package t800

import spinal.core._

/** Generic memory port command definitions. */
case class MemReadCmd() extends Bundle {
  val addr = UInt(TConsts.AddrBits bits)
}

case class MemWriteCmd() extends Bundle {
  val addr = UInt(TConsts.AddrBits bits)
  val data = Bits(TConsts.WordBits bits)
}

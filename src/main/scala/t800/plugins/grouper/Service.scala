package t800.plugins.grouper

import spinal.core._
import spinal.lib._
import t800.Global

case class GroupedInstructions() extends Bundle {
  val instructions = Vec(Bits(Global.OPCODE_BITS bits), 8)
  val count = UInt(4 bits)
}

trait GroupedInstrSrv {
  def groups: Flow[GroupedInstructions]
}

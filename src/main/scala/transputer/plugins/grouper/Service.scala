package transputer.plugins.grouper

import spinal.core._
import spinal.lib._
import transputer.Global

case class GroupedInstructions() extends Bundle {
  val instructions = Vec(Bits(Global.OPCODE_BITS bits), 8)
  val count = UInt(4 bits)
}

trait GroupedInstrService {
  def groups: Flow[GroupedInstructions]
}

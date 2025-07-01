package transputer.plugins.grouper

import spinal.core._
import spinal.lib._
import transputer.Global

case class GroupedInstructions() extends Bundle {
  val instructions = Vec(Bits(8 bits), 8) // T9000 uses 8-bit opcodes
  val count = UInt(4 bits) // Number of valid instructions (0-8)
}

/** Service interface for instruction grouping, consuming from FetchPlugin and providing to
  * PrimaryInstrPlugin. This interface follows the T9000-inspired pipeline: FetchPlugin ->
  * GrouperPlugin -> PrimaryInstrPlugin
  */
trait GroupedInstrService {
  def groups: Stream[GroupedInstructions] // Changed from Flow to Stream for proper backpressure
}

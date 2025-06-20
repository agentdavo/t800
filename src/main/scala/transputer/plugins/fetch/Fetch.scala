package transputer.plugins.fetch

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import spinal.lib.misc.database.Database
import transputer.Global

object Fetch extends AreaObject {
  object DBKeys {
    val FETCH_PC = Database.blocking[Bits]()
    val FETCH_OPCODES = Database.blocking[Vec[Bits]]()
  }

  val FETCH_PC = Payload(Bits(Global.ADDR_BITS bits))
  val FETCH_OPCODES = Payload(Vec(Bits(8 bits), 8))
}

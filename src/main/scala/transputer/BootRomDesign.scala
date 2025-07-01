package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.database.Database
import transputer.plugins.fetch.InstrFetchService

/** Simple design that maps a small boot ROM at 0x80000000 to the system bus. */
class BootRomDesign(
  romFile: String = "bootrom.hex",
  romSize: BigInt = 0x1000
) extends Component {
  val db = Transputer.defaultDatabase()
  // Provide baseline configuration for plugins
  db(Global.OPCODE_BITS) = 8
  // Instantiate core with minimal plugins
  val core = Database(db).on(Transputer(Transputer.unitPlugins()))

  val rom = BmbOnChipRam(
    p = Transputer.systemBusParam,
    size = romSize,
    hexOffset = BigInt(0x80000000L),
    hexInit = romFile
  )

  core.systemBus >> rom.io.bus
}

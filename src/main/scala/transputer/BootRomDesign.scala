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

  /** Instruction fetch bus connected to the boot ROM */
  val fetchArea = new Area {
    val fetchSrv   = core.host[InstrFetchService]
    val fetchParam =
      BmbDownSizerBridge
        .outputParameterFrom(Transputer.systemBusParam.access, 64)
        .toBmbParameter()

    val downSizer = BmbDownSizerBridge(
      inputParameter = Transputer.systemBusParam,
      outputParameter = fetchParam
    )

    val fetchBus = Bmb(fetchParam)

    fetchBus >> downSizer.io.input
    downSizer.io.output >> rom.io.bus

    val addrReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    fetchSrv.cmd.ready := fetchBus.cmd.ready
    when(fetchSrv.cmd.fire) { addrReg := fetchSrv.cmd.address }

    fetchBus.cmd.valid := fetchSrv.cmd.valid
    fetchBus.cmd.last := True
    fetchBus.cmd.opcode := Bmb.Cmd.Opcode.READ
    fetchBus.cmd.address := addrReg
    fetchBus.cmd.length := 0
    fetchBus.cmd.source := 0
    fetchBus.cmd.context := 0
    when(fetchBus.cmd.fire) { addrReg := addrReg + 1 }

    fetchBus.rsp.ready := True
    fetchSrv.rsp.valid := fetchBus.rsp.valid
    fetchSrv.rsp.payload := fetchBus.rsp.data
  }
}

package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.database.Database
import spinal.core.fiber._
import transputer.plugins.fetch.InstrFetchService

/** Simple design that maps a small boot ROM at 0x80000000 to the system bus. */
class BootRomDesign(
    romFile: String = "bootrom.hex",
    romSize: BigInt = 0x1000
) extends Component {
  val db = Transputer.defaultDatabase()
  // Provide baseline configuration for plugins
  db(Global.OPCODE_BITS) = 8
  // Instantiate core with minimal plugins via TransputerUnit
  private val unit = new TransputerUnit(db)
  val core = unit.core

  val rom = BmbOnChipRam(
    p = Transputer.systemBusParam,
    size = romSize,
    hexOffset = BigInt(0x80000000L),
    hexInit = romFile
  )

  // ROM used only for instruction fetch in this design

  /** Instruction fetch bus connected to the boot ROM */
  val fetchArea = Fiber.build(core.host.rework(new Area {
    val fetchSrv   = core.host[InstrFetchService]
    val fetchParam =
      BmbDownSizerBridge
        .outputParameterFrom(Transputer.systemBusParam.access, 64)
        .toBmbParameter()

    val upSizer = BmbUpSizerBridge(
      inputParameter = fetchParam,
      outputParameter = Transputer.systemBusParam
    )

    val fetchBus = Bmb(fetchParam)

    fetchBus >> upSizer.io.input
    upSizer.io.output >> rom.io.bus

    val addrReg = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val active  = Reg(Bool()) init False
    when(!active && fetchSrv.cmd.valid) {
      addrReg := fetchSrv.cmd.address
      active  := True
    } elsewhen (fetchBus.cmd.fire) {
      addrReg := addrReg + 1
    }

    fetchBus.cmd.valid := fetchSrv.cmd.valid
    fetchBus.cmd.last := True
    fetchBus.cmd.opcode := Bmb.Cmd.Opcode.READ
    fetchBus.cmd.address := addrReg
    fetchBus.cmd.length := 0
    fetchBus.cmd.source := 0
    fetchBus.cmd.context := 0

    fetchBus.rsp.ready := True
    fetchSrv.rsp.valid := fetchBus.rsp.valid
    fetchSrv.rsp.payload := fetchBus.rsp.data
  }))
}

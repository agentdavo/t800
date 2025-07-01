package transputer.plugins.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import spinal.lib.bus.bmb._
import transputer.{MemReadCmd, Global}
import transputer.plugins.SystemBusService
import transputer.plugins.bus.BusMasterService

/** T9000-inspired instruction fetch plugin that interfaces directly with systemBus. Fetches
  * instruction streams and provides them to the GrouperPlugin via InstrFetchService.
  */
class FetchPlugin extends FiberPlugin {
  override def getDisplayName(): String = "FetchPlugin"
  setName("fetch")

  private var fetchBus: Bmb = null
  private var instrStream: Stream[Bits] = null
  private var pcReg: UInt = null

  during setup new Area {
    // Create instruction stream for downstream plugins (GrouperPlugin)
    instrStream = Stream(Bits(8 bits)) // 8-bit opcodes per T9000 spec
    instrStream.setIdle()

    addService(new InstrFetchService {
      override def cmd: Stream[MemReadCmd] = Stream(MemReadCmd()).setIdle() // Legacy interface
      override def rsp: Stream[Bits] = FetchPlugin.this.instrStream
      override def instrStream: Stream[Bits] = FetchPlugin.this.instrStream
    })
  }

  during build new Area {
    val systemBusService = Plugin[SystemBusService]

    // Create fetch bus parameters for 64-bit instruction fetching
    val fetchBusParam = BmbParameter(
      addressWidth = Global.AddrBitsValue,
      dataWidth = 64,       // 64-bit data for efficient instruction fetching
      sourceWidth = 1,      // Single source for fetch stage
      contextWidth = 0,     // Base context width (will be extended by upsizer)
      lengthWidth = 3       // Support up to 8-byte transfers
    )

    // Create dedicated fetch bus and upsize to system bus
    fetchBus = Bmb(fetchBusParam)
    val outputParam = BmbUpSizerBridge.outputParameterFrom(
      fetchBusParam.access, 
      systemBusService.bus.p.access.dataWidth
    )
    val upSizer = BmbUpSizerBridge(
      inputParameter = fetchBusParam,
      outputParameter = fetchBusParam.copy(access = outputParam)
    )
    fetchBus >> upSizer.io.input
    // TODO: Connect through proper arbiter instead of direct connection
    // upSizer.io.output >> systemBusService.bus
    
    // Register with bus master service for arbitration
    val busMasterService = host.get[BusMasterService]
    if (busMasterService.isDefined) {
      busMasterService.get.addMaster("fetch", upSizer.io.output)
    } else {
      // Fallback: create properly terminated output to avoid bus conflicts
      upSizer.io.output.cmd.ready := True
      upSizer.io.output.rsp.valid := False
      upSizer.io.output.rsp.payload.data := 0
      upSizer.io.output.rsp.payload.last := True
      upSizer.io.output.rsp.payload.source := 0
    }

    // Program counter register
    pcReg = Reg(UInt(Global.AddrBitsValue bits)) init Global.ResetIptr

    // Instruction fetch state machine
    val fetchArea = new Area {
      val readCmd = Stream(Bits(64 bits)) // Fetch bus data width
      val pendingFetch = Reg(Bool()) init False

      // Generate fetch commands
      fetchBus.cmd.valid := instrStream.ready && !pendingFetch
      fetchBus.cmd.address := pcReg
      fetchBus.cmd.opcode := Bmb.Cmd.Opcode.READ
      fetchBus.cmd.length := 7 // Fetch 8 bytes (64 bits)
      fetchBus.cmd.last := True
      fetchBus.cmd.source := 0
      fetchBus.cmd.context := B(0, 0 bits) // 0-bit context (will be extended by upsizer)

      when(fetchBus.cmd.fire) {
        pendingFetch := True
        pcReg := pcReg + 8 // Advance PC by 8 bytes (64 bits)
      }

      // Handle fetch responses
      fetchBus.rsp.ready := instrStream.ready

      when(fetchBus.rsp.fire) {
        pendingFetch := False
      }

      // Extract individual 8-bit opcodes from 64-bit fetch data
      val opcodeExtractor = new Area {
        val fetchData = fetchBus.rsp.data
        val opcodeIndex = Reg(UInt(3 bits)) init 0 // 3 bits for 8 elements

        instrStream.valid := fetchBus.rsp.valid
        instrStream.payload := fetchData.subdivideIn(8 bits)(opcodeIndex)

        when(instrStream.fire) {
          opcodeIndex := opcodeIndex + 1
          when(opcodeIndex === 7) { // 8 elements (0-7)
            opcodeIndex := 0
          }
        }
      }
    }
  }
}

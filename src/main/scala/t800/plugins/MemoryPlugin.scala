package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
import spinal.core.fiber.{Retainer, Lock}
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline._
import t800.{MemReadCmd, MemWriteCmd, TConsts, Global}

/** Simple on-chip memory for instructions. */
class MemoryPlugin(romInit: Seq[BigInt] = Seq.fill(TConsts.RomWords)(BigInt(0)))
    extends FiberPlugin
    with PipelineService {
  private var rom: Mem[Bits] = null
  private var ram: Mem[Bits] = null
  private var instrCmdReg: Flow[MemReadCmd] = null
  private var instrRspReg: Flow[Bits] = null
  private var dataRdCmdReg: Flow[MemReadCmd] = null
  private var dataRdRspReg: Flow[Bits] = null
  private var dataWrCmdReg: Flow[MemWriteCmd] = null
  private var linkRdCmdReg: Flow[MemReadCmd] = null
  private var linkRdRspReg: Flow[Bits] = null
  private var linkWrCmdReg: Flow[MemWriteCmd] = null
  private var exeRdCmd: Flow[MemReadCmd] = null
  private var exeWrCmd: Flow[MemWriteCmd] = null
  private var chanRdCmd: Flow[MemReadCmd] = null
  private var chanWrCmd: Flow[MemWriteCmd] = null
  private val retain = Retainer()
  private var pipelineLinks: Seq[pipeline.Link] = Seq()

  override def getLinks(): Seq[pipeline.Link] = pipelineLinks

  during setup new Area {
    println(s"[${MemoryPlugin.this.getDisplayName()}] setup start")
    rom = Mem(Bits(Global.WORD_BITS bits), Global.ROM_WORDS)
    rom.initBigInt(romInit)
    ram = Mem(Bits(Global.WORD_BITS bits), Global.RAM_WORDS)
    rom.simPublic()
    ram.simPublic()
    instrCmdReg = Flow(MemReadCmd())
    instrRspReg = Flow(Bits(Global.WORD_BITS bits))
    dataRdCmdReg = Flow(MemReadCmd())
    dataRdRspReg = Flow(Bits(Global.WORD_BITS bits))
    dataWrCmdReg = Flow(MemWriteCmd())
    linkRdCmdReg = Flow(MemReadCmd())
    linkRdRspReg = Flow(Bits(Global.WORD_BITS bits))
    linkWrCmdReg = Flow(MemWriteCmd())
    exeRdCmd = Flow(MemReadCmd())
    exeWrCmd = Flow(MemWriteCmd())
    chanRdCmd = Flow(MemReadCmd())
    chanWrCmd = Flow(MemWriteCmd())
    dataRdCmdReg.valid := False
    dataRdCmdReg.payload.addr := U(0)
    dataWrCmdReg.valid := False
    dataWrCmdReg.payload.addr := U(0)
    dataWrCmdReg.payload.data := B(0, Global.WORD_BITS bits)
    addService(new InstrFetchSrv {
      override def cmd = instrCmdReg
      override def rsp = instrRspReg
    })
    addService(new DataBusSrv {
      override def rdCmd = dataRdCmdReg
      override def rdRsp = dataRdRspReg
      override def wrCmd = dataWrCmdReg
    })
    addService(new LinkBusSrv {
      override def rdCmd = linkRdCmdReg
      override def rdRsp = linkRdRspReg
      override def wrCmd = linkWrCmdReg
    })
    addService(new LinkBusArbiterSrv {
      override def exeRd: Flow[MemReadCmd] = exeRdCmd
      override def exeWr: Flow[MemWriteCmd] = exeWrCmd
      override def chanRd: Flow[MemReadCmd] = chanRdCmd
      override def chanWr: Flow[MemWriteCmd] = chanWrCmd
    })
    addService(new MemAccessSrv {
      override def rom: Mem[Bits] = MemoryPlugin.this.rom
      override def ram: Mem[Bits] = MemoryPlugin.this.ram
    })
    retain()
    println(s"[${MemoryPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${MemoryPlugin.this.getDisplayName()}] build start")
    retain.await()
    val busArb = new t800.LinkBusArbiter
    busArb.io.exeRd << exeRdCmd
    busArb.io.exeWr << exeWrCmd
    busArb.io.chanRd << chanRdCmd
    busArb.io.chanWr << chanWrCmd

    // Stage the arbiter results before hitting RAM
    val rdA = CtrlLink()
    val rdB = CtrlLink()
    val wrA = CtrlLink()
    val wrB = CtrlLink()
    val rdBuf = S2MLink(rdA.down, rdB.up)
    val wrBuf = S2MLink(wrA.down, wrB.up)
    rdA.up.driveFrom(busArb.io.rdOut)((n, p) => n(Global.MEM_ADDR) := p.addr)
    wrA.up.driveFrom(busArb.io.wrOut) { (n, p) =>
      n(Global.MEM_ADDR) := p.addr
      n(Global.MEM_DATA) := p.data
    }
    val stagedRd = rdB.down.toFlow { n =>
      val cmd = MemReadCmd()
      cmd.addr := n(Global.MEM_ADDR)
      cmd
    }
    val stagedWr = wrB.down.toFlow { n =>
      val cmd = MemWriteCmd()
      cmd.addr := n(Global.MEM_ADDR)
      cmd.data := n(Global.MEM_DATA)
      cmd
    }
    linkRdCmdReg << stagedRd
    linkWrCmdReg << stagedWr

    // Instruction read pipeline
    val iCmd = CtrlLink()
    val iDo = CtrlLink()
    val iStage = StageLink(iCmd.down, iDo.up)
    iCmd.up.driveFrom(instrCmdReg)((n, p) => n(Global.MEM_ADDR) := p.addr)
    instrRspReg.payload := rom.readSync(
      iDo(Global.MEM_ADDR)(log2Up(Global.ROM_WORDS) - 1 downto 0)
    )
    instrRspReg.valid := iDo.isValid

    // Data read pipeline with skid buffer
    val dCmdA = CtrlLink()
    val dCmdB = CtrlLink()
    val dStage = S2MLink(dCmdA.down, dCmdB.up)
    dCmdA.up.driveFrom(dataRdCmdReg)((n, p) => n(Global.MEM_ADDR) := p.addr)
    dataRdRspReg.payload := ram.readSync(
      dCmdB(Global.MEM_ADDR)(log2Up(Global.RAM_WORDS) - 1 downto 0)
    )
    dataRdRspReg.valid := dCmdB.isValid

    // Link read pipeline
    val lCmd = CtrlLink()
    val lDo = CtrlLink()
    val lStage = StageLink(lCmd.down, lDo.up)
    lCmd.up.driveFrom(linkRdCmdReg)((n, p) => n(Global.MEM_ADDR) := p.addr)
    linkRdRspReg.payload := ram.readSync(
      lDo(Global.MEM_ADDR)(log2Up(Global.RAM_WORDS) - 1 downto 0)
    )
    linkRdRspReg.valid := lDo.isValid

    // Data write pipeline with skid buffer
    val dwCmdA = CtrlLink()
    val dwCmdB = CtrlLink()
    val dwStage = S2MLink(dwCmdA.down, dwCmdB.up)
    dwCmdA.up.driveFrom(dataWrCmdReg) { (n, p) =>
      n(Global.MEM_ADDR) := p.addr
      n(Global.MEM_DATA) := p.data
    }
    when(dwCmdB.isValid) {
      ram.write(
        dwCmdB(Global.MEM_ADDR)(log2Up(Global.RAM_WORDS) - 1 downto 0),
        dwCmdB(Global.MEM_DATA)
      )
    }

    // Link write pipeline
    val lwCmd = CtrlLink()
    val lwDo = CtrlLink()
    val lwStage = StageLink(lwCmd.down, lwDo.up)
    lwCmd.up.driveFrom(linkWrCmdReg) { (n, p) =>
      n(Global.MEM_ADDR) := p.addr
      n(Global.MEM_DATA) := p.data
    }
    when(lwDo.isValid) {
      ram.write(
        lwDo(Global.MEM_ADDR)(log2Up(Global.RAM_WORDS) - 1 downto 0),
        lwDo(Global.MEM_DATA)
      )
    }

    pipelineLinks = Seq(
      rdBuf,
      wrBuf,
      iStage,
      dStage,
      lStage,
      dwStage,
      lwStage
    )
    println(s"[${MemoryPlugin.this.getDisplayName()}] build end")
  }
}

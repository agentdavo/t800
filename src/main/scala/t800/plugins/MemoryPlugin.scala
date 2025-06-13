package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import t800.{MemReadCmd, MemWriteCmd, TConsts, Global}

/** Simple on-chip memory for instructions. */
class MemoryPlugin(romInit: Seq[BigInt] = Seq.fill(TConsts.RomWords)(BigInt(0)))
    extends FiberPlugin {
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

  override def setup(): Unit = {
    rom = Mem(Bits(Global.WORD_BITS() bits), Global.ROM_WORDS())
    rom.initBigInt(romInit)
    ram = Mem(Bits(Global.WORD_BITS() bits), Global.RAM_WORDS())
    rom.simPublic()
    ram.simPublic()
    instrCmdReg = Flow(MemReadCmd())
    instrRspReg = Flow(Bits(Global.WORD_BITS() bits))
    dataRdCmdReg = Flow(MemReadCmd())
    dataRdRspReg = Flow(Bits(Global.WORD_BITS() bits))
    dataWrCmdReg = Flow(MemWriteCmd())
    linkRdCmdReg = Flow(MemReadCmd())
    linkRdRspReg = Flow(Bits(Global.WORD_BITS() bits))
    linkWrCmdReg = Flow(MemWriteCmd())
    exeRdCmd = Flow(MemReadCmd())
    exeWrCmd = Flow(MemWriteCmd())
    chanRdCmd = Flow(MemReadCmd())
    chanWrCmd = Flow(MemWriteCmd())
    dataRdCmdReg.valid := False
    dataRdCmdReg.payload.addr := U(0)
    dataWrCmdReg.valid := False
    dataWrCmdReg.payload.addr := U(0)
    dataWrCmdReg.payload.data := B(0, Global.WORD_BITS() bits)
    addService(new InstrBusSrv {
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
  }

  override def build(): Unit = {
    val busArb = new t800.LinkBusArbiter
    busArb.io.exeRd << exeRdCmd
    busArb.io.exeWr << exeWrCmd
    busArb.io.chanRd << chanRdCmd
    busArb.io.chanWr << chanWrCmd
    linkRdCmdReg.valid := busArb.io.rdOut.valid
    linkRdCmdReg.payload := busArb.io.rdOut.payload
    linkWrCmdReg.valid := busArb.io.wrOut.valid
    linkWrCmdReg.payload := busArb.io.wrOut.payload
    instrRspReg.payload := rom.readSync(
      instrCmdReg.payload.addr(log2Up(Global.ROM_WORDS()) - 1 downto 0)
    )
    instrRspReg.valid := instrCmdReg.valid
    dataRdRspReg.payload := ram.readSync(
      dataRdCmdReg.payload.addr(log2Up(Global.RAM_WORDS()) - 1 downto 0)
    )
    dataRdRspReg.valid := dataRdCmdReg.valid
    linkRdRspReg.payload := ram.readSync(
      linkRdCmdReg.payload.addr(log2Up(Global.RAM_WORDS()) - 1 downto 0)
    )
    linkRdRspReg.valid := linkRdCmdReg.valid
    when(dataWrCmdReg.valid) {
      ram.write(
        dataWrCmdReg.payload.addr(log2Up(Global.RAM_WORDS()) - 1 downto 0),
        dataWrCmdReg.payload.data
      )
    }
    when(linkWrCmdReg.valid) {
      ram.write(
        linkWrCmdReg.payload.addr(log2Up(Global.RAM_WORDS()) - 1 downto 0),
        linkWrCmdReg.payload.data
      )
    }
  }
}

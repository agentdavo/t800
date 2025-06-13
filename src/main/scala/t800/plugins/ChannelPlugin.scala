package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
import spinal.core.fiber.Retainer
import spinal.lib.misc.pipeline._
import t800.{MemReadCmd, MemWriteCmd, TConsts, Global}
import t800.plugins.{LinkBusSrv, LinkBusArbiterSrv}

/** Provides transputer link interfaces with simple FIFO synchronizers. */
class ChannelPlugin extends FiberPlugin {
  private var pins: ChannelPins = null
  private var rxVec: Vec[Stream[Bits]] = null
  private var txVec: Vec[Stream[Bits]] = null
  private var cmdStream: Stream[ChannelTxCmd] = null
  private var busyVec: Vec[Bool] = null
  private var memTx: Vec[Stream[Bits]] = null

  private val retain = Retainer()

  during setup new Area {
    pins = ChannelPins(Global.LINK_COUNT)
    rxVec = Vec.fill(Global.LINK_COUNT)(Stream(Bits(Global.WORD_BITS bits)))
    txVec = Vec.fill(Global.LINK_COUNT)(Stream(Bits(Global.WORD_BITS bits)))
    rxVec.foreach(_.setIdle())
    txVec.foreach(_.setIdle())
    rxVec.foreach(_.ready := False)
    cmdStream = Stream(ChannelTxCmd()).setIdle()
    busyVec = Vec.fill(Global.LINK_COUNT)(Reg(Bool()) init False)
    addService(new ChannelSrv {
      override def txReady(link: UInt): Bool = txVec(link).ready
      override def push(link: UInt, data: Bits): Bool = {
        txVec(link).valid := True
        txVec(link).payload := data
        txVec(link).ready
      }
      override def rxValid(link: UInt): Bool = rxVec(link).valid
      override def rxPayload(link: UInt): Bits = rxVec(link).payload
      override def rxAck(link: UInt): Unit = { rxVec(link).ready := True }
    })
    addService(new ChannelPinsSrv { def pins = ChannelPlugin.this.pins })
    addService(new ChannelDmaSrv { def cmd = ChannelPlugin.this.cmdStream })
    retain()
  }

  during build new Area {
    retain.await()
    implicit val h: PluginHost = host
    val mem = Plugin[LinkBusSrv]
    val arb = Plugin[LinkBusArbiterSrv]

    arb.chanRd.valid := False
    arb.chanWr.valid := False
    arb.chanWr.payload.addr := U(0)
    arb.chanWr.payload.data := B(0, TConsts.WordBits bits)

    memTx = Vec.fill(Global.LINK_COUNT)(Stream(Bits(Global.WORD_BITS bits)))
    memTx.foreach(_.setIdle())

    for (i <- 0 until Global.LINK_COUNT) {
      rxVec(i) << pins.in(i)
      val txArb = StreamArbiterFactory.lowerFirst.onArgs(txVec(i), memTx(i))
      pins.out(i) << txArb
    }

    val CMD_ADDR = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_LEN = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_LINK = Payload(UInt(log2Up(Global.LINK_COUNT) bits))

    val dmaCmd = CtrlLink()
    val dmaDo = CtrlLink()
    val dmaStage = StageLink(dmaCmd.down, dmaDo.up)
    dmaCmd.up.driveFrom(cmdStream) { (n, p) =>
      n(CMD_ADDR) := p.addr
      n(CMD_LEN) := p.length
      n(CMD_LINK) := p.link
    }
    dmaCmd.haltWhen(busyVec.reduce(_ || _))

    val ptr = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val remaining = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val linkIdx = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0
    val haveByte = Reg(Bool()) init False
    val byteReg = Reg(Bits(8 bits)) init 0

    when(dmaCmd.down.isFiring) {
      ptr := dmaCmd(CMD_ADDR)
      remaining := dmaCmd(CMD_LEN)
      linkIdx := dmaCmd(CMD_LINK)
      busyVec.foreach(_ := False)
      busyVec(dmaCmd(CMD_LINK)) := True
    }

    arb.chanRd.valid := dmaDo.isValid && !haveByte
    arb.chanRd.payload.addr := ptr
    dmaDo.haltWhen(!haveByte && !mem.rdRsp.valid)
    when(dmaDo.isValid && !haveByte && mem.rdRsp.valid) {
      val shift = ptr(1 downto 0) * 8
      byteReg := (mem.rdRsp.payload >> shift)(7 downto 0)
      haveByte := True
      ptr := ptr + 1
      remaining := remaining - 1
    }

    memTx(linkIdx).valid := dmaDo.isValid && haveByte
    memTx(linkIdx).payload := byteReg.resized
    dmaDo.haltWhen(haveByte && !memTx(linkIdx).ready)
    when(dmaDo.isValid && haveByte && memTx(linkIdx).ready) {
      haveByte := False
      when(remaining === 0) {
        busyVec(linkIdx) := False
        dmaDo.throwIt(usingReady = true)
      }
    }

    Builder(dmaStage)
  }
}

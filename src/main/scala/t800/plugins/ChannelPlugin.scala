package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
import spinal.core.fiber.Retainer
import spinal.lib.misc.{pipeline => miscPipeline}
import miscPipeline._
import t800.{MemReadCmd, MemWriteCmd, TConsts, Global}
import t800.plugins.{LinkBusSrv, LinkBusArbiterSrv}

object DmaState extends SpinalEnum {
  val Idle, Read, Push, Complete = newElement()
}

/** Provides transputer link interfaces with simple FIFO synchronizers. */
class ChannelPlugin extends FiberPlugin with PipelineService {
  private var pins: ChannelPins = null
  private var rxVec: Vec[Stream[Bits]] = null
  private var txVec: Vec[Stream[Bits]] = null
  private var cmdStream: Stream[ChannelTxCmd] = null
  private var stateVec: Vec[DmaState.C] = null
  private var memTx: Vec[Stream[Bits]] = null

  private val retain = Retainer()
  private var pipelineLinks: Seq[miscPipeline.Link] = Seq()
  override def getLinks(): Seq[miscPipeline.Link] = pipelineLinks

  during setup new Area {
    println(s"[${ChannelPlugin.this.getDisplayName()}] setup start")
    pins = ChannelPins(Global.LINK_COUNT)
    rxVec = Vec.fill(Global.LINK_COUNT)(Stream(Bits(Global.WORD_BITS bits)))
    txVec = Vec.fill(Global.LINK_COUNT)(Stream(Bits(Global.WORD_BITS bits)))
    rxVec.foreach(_.setIdle())
    txVec.foreach(_.setIdle())
    rxVec.foreach(_.ready := False)
    cmdStream = Stream(ChannelTxCmd()).setIdle()
    stateVec = Vec.fill(Global.LINK_COUNT)(Reg(DmaState()) init DmaState.Idle)
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
    println(s"[${ChannelPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${ChannelPlugin.this.getDisplayName()}] build start")
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
    val CMD_STRIDE = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_ROWS = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_TWO_D = Payload(Bool())

    val dmaCmd = CtrlLink()
    val dmaDo = CtrlLink()
    val dmaStage = StageLink(dmaCmd.down, dmaDo.up)
    dmaCmd.up.driveFrom(cmdStream) { (n, p) =>
      n(CMD_ADDR) := p.addr
      n(CMD_LEN) := p.length
      n(CMD_LINK) := p.link
      n(CMD_STRIDE) := p.stride
      n(CMD_ROWS) := p.rows
      n(CMD_TWO_D) := p.twoD
    }
    dmaCmd.haltWhen(stateVec.map(_ =/= DmaState.Idle).reduce(_ || _))

    val ptr = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val remaining = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val rowBytes = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val rows = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val stride = Reg(UInt(Global.ADDR_BITS bits)) init 0
    val is2d = Reg(Bool()) init False
    val linkIdx = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0
    val byteReg = Reg(Bits(8 bits)) init 0

    when(dmaCmd.down.isFiring) {
      ptr := dmaCmd(CMD_ADDR)
      remaining := dmaCmd(CMD_LEN)
      rowBytes := dmaCmd(CMD_LEN)
      rows := Mux(dmaCmd(CMD_TWO_D), dmaCmd(CMD_ROWS), U(1))
      stride := dmaCmd(CMD_STRIDE)
      is2d := dmaCmd(CMD_TWO_D)
      linkIdx := dmaCmd(CMD_LINK)
      stateVec.foreach(_ := DmaState.Idle)
      stateVec(dmaCmd(CMD_LINK)) := DmaState.Read
    }

    val state = stateVec(linkIdx)

    arb.chanRd.valid := False
    memTx(linkIdx).valid := False

    switch(state) {
      is(DmaState.Read) {
        arb.chanRd.valid := dmaDo.isValid
        arb.chanRd.payload.addr := ptr
        dmaDo.haltWhen(!mem.rdRsp.valid)
        when(dmaDo.isValid && mem.rdRsp.valid) {
          val shift = ptr(1 downto 0) * 8
          byteReg := (mem.rdRsp.payload >> shift)(7 downto 0)
          ptr := ptr + 1
          remaining := remaining - 1
          stateVec(linkIdx) := DmaState.Push
        }
      }
      is(DmaState.Push) {
        memTx(linkIdx).valid := dmaDo.isValid
        memTx(linkIdx).payload := byteReg.resized
        dmaDo.haltWhen(!memTx(linkIdx).ready)
        when(dmaDo.isValid && memTx(linkIdx).ready) {
          when(remaining === 0) {
            when(rows === 1) {
              stateVec(linkIdx) := DmaState.Complete
            } otherwise {
              rows := rows - 1
              ptr := ptr + stride
              remaining := rowBytes
              stateVec(linkIdx) := DmaState.Read
            }
          } otherwise {
            stateVec(linkIdx) := DmaState.Read
          }
        }
      }
      is(DmaState.Complete) {
        when(dmaDo.isValid) {
          stateVec(linkIdx) := DmaState.Idle
          dmaDo.throwIt(usingReady = true)
        }
      }
      default {}
    }

    pipelineLinks = Seq(dmaStage)
    println(s"[${ChannelPlugin.this.getDisplayName()}] build end")
  }
}

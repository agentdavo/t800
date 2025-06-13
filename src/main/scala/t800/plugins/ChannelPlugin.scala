package t800.plugins

import spinal.core._
import spinal.lib._
import t800.TConsts

/** Provides transputer link interfaces with simple FIFO synchronizers. */
class ChannelPlugin extends FiberPlugin {
  private var pins: ChannelPins = null
  private var rxVec: Vec[Stream[Bits]] = null
  private var txVec: Vec[Stream[Bits]] = null
  private var cmdStream: Stream[ChannelTxCmd] = null
  private var busyVec: Vec[Bool] = null
  private var memTx: Vec[Stream[Bits]] = null

  override def setup(): Unit = {
    pins = ChannelPins(TConsts.LinkCount)
    rxVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    txVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    rxVec.foreach(_.ready := False)
    cmdStream = Stream(ChannelTxCmd())
    busyVec = Vec.fill(TConsts.LinkCount)(Reg(Bool()) init False)
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
  }

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val mem = Plugin[LinkBusSrv]

    memTx = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    memTx.foreach(_.setIdle())

    for (i <- 0 until TConsts.LinkCount) {
      rxVec(i) << pins.in(i)
      val arb = StreamArbiterFactory.lowerFirst.onArgs(txVec(i), memTx(i))
      pins.out(i) << arb
    }

    cmdStream.ready := !busyVec.reduce(_ || _)
    val ptr = Reg(UInt(TConsts.AddrBits bits)) init 0
    val remaining = Reg(UInt(TConsts.AddrBits bits)) init 0
    val linkIdx = Reg(UInt(log2Up(TConsts.LinkCount) bits)) init 0
    val haveByte = Reg(Bool()) init False
    val byteReg = Reg(Bits(8 bits)) init 0

    when(!busyVec.reduce(_ || _) && cmdStream.valid) {
      ptr := cmdStream.payload.addr
      remaining := cmdStream.payload.length
      linkIdx := cmdStream.payload.link
      busyVec.foreach(_ := False)
      busyVec(cmdStream.payload.link) := True
    }

    when(busyVec.reduce(_ || _)) {
      when(!haveByte) {
        mem.rdCmd.valid := True
        when(mem.rdRsp.valid) {
          val shift = ptr(1 downto 0) * 8
          byteReg := (mem.rdRsp.payload >> shift)(7 downto 0)
          haveByte := True
          ptr := ptr + 1
          remaining := remaining - 1
        }
      } otherwise {
        memTx(linkIdx).valid := True
        memTx(linkIdx).payload := byteReg.resized
        when(memTx(linkIdx).ready) {
          haveByte := False
          when(remaining === 0) {
            busyVec(linkIdx) := False
          }
        }
      }
    }
  }
}

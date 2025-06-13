package t800.plugins

import spinal.core._
import spinal.lib._
import t800.{MemReadCmd, MemWriteCmd, TConsts}
import t800.plugins.{LinkBusSrv, LinkBusArbiterSrv}

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
    txVec.foreach(_.setIdle())
    cmdStream = Stream(ChannelTxCmd())
    busyVec = Vec.fill(TConsts.LinkCount)(Reg(Bool()) init False)
    addService(new ChannelSrv {
      override def rx: Vec[Stream[Bits]] = rxVec
      override def tx: Vec[Stream[Bits]] = txVec
      override def txCmd: Stream[ChannelTxCmd] = cmdStream
      override def busy: Vec[Bool] = busyVec
    })
    addService(new ChannelPinsSrv { def pins = ChannelPlugin.this.pins })
  }

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val mem = Plugin[LinkBusSrv]
    val arb = Plugin[LinkBusArbiterSrv]

    arb.chanRd.valid := False
    arb.chanRd.payload.addr := U(0)
    arb.chanWr.valid := False
    arb.chanWr.payload.addr := U(0)
    arb.chanWr.payload.data := B(0, TConsts.WordBits bits)

    memTx = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    memTx.foreach(_.setIdle())

    for (i <- 0 until TConsts.LinkCount) {
      rxVec(i) << pins.in(i)
      val txArb = StreamArbiterFactory.lowerFirst.onArgs(txVec(i), memTx(i))
      pins.out(i) << txArb
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
        arb.chanRd.valid := True
        arb.chanRd.payload.addr := ptr
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

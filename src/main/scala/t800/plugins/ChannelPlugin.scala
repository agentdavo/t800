package t800.plugins

import spinal.core._
import spinal.lib._
import t800.TConsts

/** Provides transputer link interfaces with simple FIFO synchronizers. */
class ChannelPlugin extends FiberPlugin {
  private var pins: ChannelPins = null
  private var rxVec: Vec[Stream[Bits]] = null
  private var txVec: Vec[Stream[Bits]] = null

  override def setup(): Unit = {
    pins = ChannelPins(TConsts.LinkCount)
    rxVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    txVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    rxVec.foreach(_.setIdle())
    txVec.foreach(_.setIdle())
    rxVec.foreach(_.ready := False)
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
    for (i <- 0 until TConsts.LinkCount) {
      val inFifo = StreamFifo(Bits(TConsts.WordBits bits), 2)
      val outFifo = StreamFifo(Bits(TConsts.WordBits bits), 2)
      inFifo.io.push << pins.in(i)
      rxVec(i) << inFifo.io.pop
      outFifo.io.push << txVec(i)
      pins.out(i) << outFifo.io.pop
    }
  }
}

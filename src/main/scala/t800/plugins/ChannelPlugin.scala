package t800.plugins

import spinal.core._
import spinal.lib._
import t800.{MemReadCmd, MemWriteCmd, TConsts}

/** Provides transputer link interfaces with simple FIFO synchronizers. */
class ChannelPlugin extends FiberPlugin {
  private var pins: ChannelPins = null
  private var rxVec: Vec[Stream[Bits]] = null
  private var txVec: Vec[Stream[Bits]] = null

  override def setup(): Unit = {
    pins = ChannelPins(TConsts.LinkCount)
    rxVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    txVec = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    addService(new ChannelSrv {
      override def rx: Vec[Stream[Bits]] = rxVec
      override def tx: Vec[Stream[Bits]] = txVec
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

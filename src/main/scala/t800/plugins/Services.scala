package t800.plugins

import spinal.core._
import spinal.lib._

trait LinkPins
trait DebugPins extends Area
trait ExtMemPins extends Area

trait StackSrv {
  val A: UInt
  val B: UInt
  val C: UInt
  val O: UInt
  val WPtr: UInt
  val IPtr: UInt
  def read(offset: SInt): UInt
  def write(offset: SInt, data: UInt): Unit
}

trait FpuSrv {
  def pipe: Flow[FpCmd]
  def rsp: Flow[UInt]
}

case class SchedCmd() extends Bundle {
  val ptr = UInt(t800.TConsts.AddrBits bits)
  val high = Bool()
}

trait SchedSrv {
  def newProc: Flow[SchedCmd]
  def nextProc: UInt
}

trait TimerSrv {
  def hi: UInt
  def lo: UInt
  def set(value: UInt): Unit
}

trait InstrBusSrv {
  def cmd: Flow[t800.MemReadCmd]
  def rsp: Flow[Bits]
}

trait DataBusSrv {
  def rdCmd: Flow[t800.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[t800.MemWriteCmd]
}

trait LinkBusSrv {
  def rdCmd: Flow[t800.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[t800.MemWriteCmd]
}

trait MemAccessSrv {
  def rom: Mem[Bits]
  def ram: Mem[Bits]
}

case class ChannelPins(count: Int) extends Bundle with LinkPins {
  val in = Vec(slave Stream (Bits(t800.TConsts.WordBits bits)), count)
  val out = Vec(master Stream (Bits(t800.TConsts.WordBits bits)), count)
}

trait ChannelSrv {

  /** Return true when the transmit FIFO can accept a new word. */
  def txReady(link: UInt): Bool

  /** Drive a transmit request. The returned value mirrors txReady. */
  def push(link: UInt, data: Bits): Bool

  /** True when a word is available on the receive FIFO. */
  def rxValid(link: UInt): Bool

  /** Current word from the receive FIFO. */
  def rxPayload(link: UInt): Bits

  /** Acknowledge the current word when done. */
  def rxAck(link: UInt): Unit
}

trait ChannelPinsSrv {
  def pins: ChannelPins
}

package transputer.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.Bmb
import _root_.transputer.Global

trait LinkPins
trait DebugPins extends Area
trait ExtMemPins extends Area

trait ConfigAccessService {
  def addr: Bits
  def data: Bits
  def writeEnable: Bool
  def isValid: Bool
  def read(addr: Bits, width: Int): Bits
  def write(addr: Bits, data: Bits, width: Int): Unit
}

trait AddressTranslationService {
  def translate(addr: Bits): Bits
}

trait SystemBusService {
  def bus: Bmb
}

trait DataBusService {
  def rdCmd: Flow[_root_.transputer.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[_root_.transputer.MemWriteCmd]
}

trait LinkBusService {
  def rdCmd: Flow[_root_.transputer.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[_root_.transputer.MemWriteCmd]
}

trait LinkBusArbiterService {
  def exeRd: Flow[_root_.transputer.MemReadCmd]
  def exeWr: Flow[_root_.transputer.MemWriteCmd]
  def chanRd: Flow[_root_.transputer.MemReadCmd]
  def chanWr: Flow[_root_.transputer.MemWriteCmd]
}

case class ChannelTxCmd() extends Bundle {
  val link = UInt(2 bits)
  val addr = UInt(Global.ADDR_BITS bits)
  val length = UInt(Global.ADDR_BITS bits)
  val stride = UInt(Global.ADDR_BITS bits)
  val rows = UInt(Global.ADDR_BITS bits)
  val twoD = Bool()
}

case class ChannelPins(count: Int) extends Bundle with LinkPins {
  val in = Vec(slave Stream (Bits(Global.WORD_BITS bits)), count)
  val out = Vec(master Stream (Bits(Global.WORD_BITS bits)), count)
}

trait ChannelService {
  def txReady(link: UInt): Bool
  def push(link: UInt, data: Bits): Bool
  def rxValid(link: UInt): Bool
  def rxPayload(link: UInt): Bits
  def rxAck(link: UInt): Unit
}

trait ChannelPinsService {
  def pins: ChannelPins
}

trait ChannelDmaService {
  def cmd: Stream[ChannelTxCmd]
}

trait MemAccessService {
  def rom: Mem[UInt]
  def ram: Mem[UInt]
}

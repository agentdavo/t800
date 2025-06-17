package transputer.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.Bmb
import transputer.Global

trait LinkPins
trait DebugPins extends Area
trait ExtMemPins extends Area

trait ConfigAccessSrv {
  def addr: Bits
  def data: Bits
  def writeEnable: Bool
  def isValid: Bool
  def read(addr: Bits, width: Int): Bits
  def write(addr: Bits, data: Bits, width: Int): Unit
}

trait AddressTranslationSrv {
  def translate(addr: Bits): Bits
}

trait SystemBusSrv {
  def bus: Bmb
}

trait DataBusSrv {
  def rdCmd: Flow[transputer.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[transputer.MemWriteCmd]
}

trait LinkBusSrv {
  def rdCmd: Flow[transputer.MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[transputer.MemWriteCmd]
}

trait LinkBusArbiterSrv {
  def exeRd: Flow[transputer.MemReadCmd]
  def exeWr: Flow[transputer.MemWriteCmd]
  def chanRd: Flow[transputer.MemReadCmd]
  def chanWr: Flow[transputer.MemWriteCmd]
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

trait ChannelSrv {
  def txReady(link: UInt): Bool
  def push(link: UInt, data: Bits): Bool
  def rxValid(link: UInt): Bool
  def rxPayload(link: UInt): Bits
  def rxAck(link: UInt): Unit
}

trait ChannelPinsSrv {
  def pins: ChannelPins
}

trait ChannelDmaSrv {
  def cmd: Stream[ChannelTxCmd]
}

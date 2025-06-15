package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline

/** Service exposing pipeline links so plugins can build their logic. */
trait PipelineService {
  def getLinks(): Seq[pipeline.Link]
}

trait AddressTranslationSrv { def translate(addr: Bits): Bits }
trait LinkPins
trait DebugPins extends Area
trait ExtMemPins extends Area

/** Service for stack operations, integrated with WorkspaceCachePlugin. */
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

/** Service for data bus operations, integrated with MainCachePlugin and PmiPlugin. */
trait DataBusSrv {
  def rdCmd: Flow[t800.MemReadCmd] // BMB-based read command
  def rdRsp: Flow[Bits] // Response from MainCachePlugin or PmiPlugin
  def wrCmd: Flow[t800.MemWriteCmd] // BMB-based write command
}

/** Service for link bus operations, integrated with MainCachePlugin and LinksPlugin. */
trait LinkBusSrv {
  def rdCmd: Flow[t800.MemReadCmd] // BMB-based read command
  def rdRsp: Flow[Bits] // Response from MainCachePlugin
  def wrCmd: Flow[t800.MemWriteCmd] // BMB-based write command
}

/** Service for link bus arbitration, integrated with LinksPlugin. */
trait LinkBusArbiterSrv {
  def exeRd: Flow[t800.MemReadCmd]
  def exeWr: Flow[t800.MemWriteCmd]
  def chanRd: Flow[t800.MemReadCmd]
  def chanWr: Flow[t800.MemWriteCmd]
}

/** Service for memory access, removed direct Mem access, integrated with BMB plugins. */

/** Service for trap handling, integrated with MemoryManagementPlugin. */
case class TrapHandlerSrv() extends Bundle {
  val trapAddr = Bits(32 bits) // Address where trap occurred
  val trapType = Bits(4 bits) // Type of trap (e.g., 0010: stack extension, 0100: privileged instr)
  val trapEnable = Bool() // Enable trap handling for current process
  val trapHandlerAddr = Bits(32 bits) // Address of trap handler (set by SchedulerPlugin)
  def setTrap(addr: Bits, typ: Bits): Unit = {
    trapAddr := addr
    trapType := typ
    trapEnable := True
  }
  def clearTrap(): Unit = trapEnable := False
}

/** Service for channel transmit/receive operations, integrated with ChannelPlugin. */
case class ChannelTxCmd() extends Bundle {
  val link = UInt(2 bits)
  val addr = UInt(t800.Global.ADDR_BITS bits)
  val length = UInt(t800.Global.ADDR_BITS bits)
  val stride = UInt(t800.Global.ADDR_BITS bits)
  val rows = UInt(t800.Global.ADDR_BITS bits)
  val twoD = Bool()
}

case class ChannelPins(count: Int) extends Bundle with LinkPins {
  val in = Vec(slave Stream (Bits(t800.Global.WORD_BITS bits)), count)
  val out = Vec(master Stream (Bits(t800.Global.WORD_BITS bits)), count)
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

trait ChannelDmaSrv {
  def cmd: Stream[ChannelTxCmd]
}

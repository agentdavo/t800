package transputer.plugins.legacy.vcp

import spinal.core._
import spinal.lib._

// Base channel service for transputer communication
trait ChannelService {
  def txReady(link: UInt): Bool
  def push(link: UInt, data: Bits): Bool
  def rxValid(link: UInt): Bool
  def rxPayload(link: UInt): Bits
  def rxAck(link: UInt): Unit
}

// Memory access commands for VCP VLCB management
case class MemReadCmd() extends Bundle {
  val addr = UInt(32 bits)
  val size = UInt(3 bits) // 1, 2, 4, 8, 16, 32 bytes
}

case class MemWriteCmd() extends Bundle {
  val addr = UInt(32 bits)
  val data = Bits(32 bits)
  val size = UInt(3 bits)
}

// Link bus service for memory-mapped VCP access
trait LinkBusService {
  def rdCmd: Flow[MemReadCmd]
  def rdRsp: Flow[Bits]
  def wrCmd: Flow[MemWriteCmd]
}

/** Enhanced VCP service with T9000 compliance and hybrid signal-method architecture.
  *
  * This interface uses a hybrid approach:
  *   - Direct signal access for performance-critical VCP state operations
  *   - Method interfaces for complex channel operations and VLCB management
  *   - Proper SpinalHDL signal ownership patterns
  */
trait VcpService extends ChannelService {
  // ========================================
  // DIRECT SIGNAL ACCESS (for performance-critical operations)
  // ========================================
  // These are mutable signals owned by the VcpPlugin
  // They can be read directly and assigned from other plugins
  def vcpStatus: Bits // Direct access to VCP status register
  def channelStates: Vec[Bits] // Direct access to array of channel state registers
  def linkReady: Vec[Bool] // Direct access to per-link ready signals
  def linkError: Vec[Bool] // Direct access to per-link error flags
  def vlcbBaseAddr: Vec[UInt] // Direct access to VLCB base addresses

  // ========================================
  // METHOD INTERFACES (for complex operations with state management)
  // ========================================

  def scheduleInput(channel: Int): Unit
  def scheduleOutput(channel: Int): Unit
  def getChannelState(channel: Int): Bits
  def setVcpCommand(cmd: Bits): Unit
  def getVcpStatus(): Bits
  def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit
  def receiveAck(channel: Int): Bool
  def enqueueMessage(channel: Int, data: Bits): Unit

  // T9000-specific VCP operations
  def initializeVlcb(channel: Int, baseAddr: UInt): Unit = {}
  def updateChannelHeader(channel: Int, header: Bits): Unit = {}
  def handleLinkError(link: Int, errorType: Bits): Unit = {}
  def performLinkReset(link: Int): Unit = {}

  // Signal update triggers (called when direct signal assignment occurs)
  def updateRegisters(): Unit // Sync signals back to register file
}

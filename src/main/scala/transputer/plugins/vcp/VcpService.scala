package transputer.plugins.vcp

import spinal.core._
import spinal.lib._

/** T9000 Virtual Channel Processor (VCP) Service
  *
  * Manages DS-Link virtual channels and packet routing per T9000 specification Chapter 12. Provides
  * high-level interface for virtual channel communication.
  */
trait VcpService {
  // VCP Control and Status
  def enable(): Unit
  def disable(): Unit
  def reset(): Unit
  def isEnabled: Bool
  def isReady: Bool
  def getStatus: Bits

  // Virtual Channel Management
  def openChannel(virtualChannel: UInt, physicalLink: UInt): Bool
  def closeChannel(virtualChannel: UInt): Bool
  def isChannelOpen(virtualChannel: UInt): Bool
  def getChannelStatus(virtualChannel: UInt): Bits

  // Packet Transmission
  def sendPacket(virtualChannel: UInt, data: Bits, isEnd: Bool): Bool
  def canSendPacket(virtualChannel: UInt): Bool
  def getTransmitCredit(virtualChannel: UInt): UInt

  // Packet Reception
  def receivePacket(virtualChannel: UInt): Option[Bits]
  def hasReceiveData(virtualChannel: UInt): Bool
  def getReceiveBufferLevel(virtualChannel: UInt): UInt

  // Physical Link Interface
  def getLinkStatus(physicalLink: UInt): Bits
  def isLinkReady(physicalLink: UInt): Bool
  def hasLinkError(physicalLink: UInt): Bool
  def clearLinkError(physicalLink: UInt): Unit

  // Header Area Management (T9000 Section 12.4)
  def setHeaderAreaBase(address: UInt): Unit
  def setHeaderAreaLimit(address: UInt): Unit
  def getHeaderAreaBase: UInt
  def getHeaderAreaLimit: UInt

  // Virtual Channel Configuration
  def setChannelPriority(virtualChannel: UInt, priority: UInt): Unit
  def setChannelBufferSize(virtualChannel: UInt, size: UInt): Unit
  def setChannelTimeout(virtualChannel: UInt, timeout: UInt): Unit

  // Flow Control
  def sendAcknowledge(virtualChannel: UInt): Unit
  def requestCredit(virtualChannel: UInt, amount: UInt): Bool
  def getFlowControlState(virtualChannel: UInt): Bits

  // Error Handling
  def hasChannelError(virtualChannel: UInt): Bool
  def getErrorStatus(virtualChannel: UInt): Bits
  def clearChannelError(virtualChannel: UInt): Unit
  def clearAllErrors(): Unit

  // Statistics and Monitoring
  def getPacketsSent(virtualChannel: UInt): UInt
  def getPacketsReceived(virtualChannel: UInt): UInt
  def getBytesTransferred(virtualChannel: UInt): UInt
  def getErrorCount(virtualChannel: UInt): UInt
}

/** DS-Link Hardware Service Low-level interface to physical DS-Link hardware
  */
trait DsLinkService {
  // Physical Link Control
  def enableLink(link: UInt): Unit
  def disableLink(link: UInt): Unit
  def resetLink(link: UInt): Unit
  def isLinkActive(link: UInt): Bool
  def isLinkConnected(link: UInt): Bool

  // Physical Layer Status
  def getSignalStrength(link: UInt): UInt
  def getClockRecoveryStatus(link: UInt): Bool
  def getSerializerStatus(link: UInt): Bool
  def getDeserializerStatus(link: UInt): Bool

  // Data Transmission
  def transmitData(link: UInt, data: Bits): Bool
  def isTransmitReady(link: UInt): Bool
  def getTransmitBufferLevel(link: UInt): UInt

  // Data Reception
  def receiveData(link: UInt): Option[Bits]
  def isReceiveDataAvailable(link: UInt): Bool
  def getReceiveBufferLevel(link: UInt): UInt

  // Error Detection
  def hasParityError(link: UInt): Bool
  def hasFramingError(link: UInt): Bool
  def hasOverrunError(link: UInt): Bool
  def clearErrors(link: UInt): Unit

  // Configuration
  def setLinkSpeed(link: UInt, speed: UInt): Unit
  def setLinkMode(link: UInt, mode: UInt): Unit
  def getLinkConfiguration(link: UInt): Bits
}

/** Virtual Channel Descriptor
  */
case class VirtualChannelDescriptor() extends Bundle {
  val channelId = UInt(8 bits)
  val physicalLink = UInt(3 bits)
  val priority = UInt(3 bits)
  val bufferSize = UInt(10 bits)
  val timeout = UInt(16 bits)
  val isOpen = Bool()
  val hasError = Bool()
  val reserved = Bits(6 bits)
}

/** DS-Link Packet Header (T9000 format)
  */
case class DsLinkPacketHeader() extends Bundle {
  val destinationChannel = UInt(8 bits)
  val sourceChannel = UInt(8 bits)
  val packetType = UInt(4 bits)
  val priority = UInt(3 bits)
  val isEnd = Bool()
  val length = UInt(8 bits)
  val reserved = Bits(8 bits)
}

/** VCP Packet Buffer Entry
  */
case class VcpPacketEntry() extends Bundle {
  val header = DsLinkPacketHeader()
  val data = Bits(256 bits) // 32 bytes max per packet
  val valid = Bool()
  val bytesUsed = UInt(6 bits)
}

/** VCP Status Register Layout
  */
object VcpStatus {
  def ENABLED = 0
  def READY = 1
  def PROCESSING = 2
  def ERROR = 3
  def LINK0_READY = 4
  def LINK1_READY = 5
  def LINK2_READY = 6
  def LINK3_READY = 7
  def HEADER_ERROR = 8
  def PACKET_ERROR = 9
  def BUFFER_OVERFLOW = 10
  def TIMEOUT_ERROR = 11
  def VCP_STATE_LSB = 12 // 3-bit state field
  def VCP_STATE_MSB = 14
  def RESERVED_LSB = 15
  def RESERVED_MSB = 31
}

/** VCP Channel States
  */
object VcpChannelState extends SpinalEnum {
  val CLOSED, OPENING, OPEN, CLOSING, ERROR = newElement()
}

/** VCP Packet Types
  */
object VcpPacketType extends SpinalEnum {
  val DATA, ACKNOWLEDGE, CREDIT_REQUEST, CHANNEL_OPEN, CHANNEL_CLOSE, ERROR_REPORT = newElement()
}

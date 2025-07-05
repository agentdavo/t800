package transputer.plugins.vcp

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin

/** Simplified T9000 VCP Plugin for compilation testing This is a minimal implementation to resolve
  * compilation issues
  */
class VcpPluginSimple(
  val maxVirtualChannels: Int = 256,
  val physicalLinkCount: Int = 4,
  val channelBufferSize: Int = 1024,
  val packetBufferDepth: Int = 16,
  val enableFlowControl: Boolean = true
) extends FiberPlugin {

  override def getDisplayName(): String = "VcpPluginSimple"
  setName("vcpSimple")

  during setup new Area {
    println(s"[VcpPluginSimple] Setup starting...")
    require(maxVirtualChannels > 0 && maxVirtualChannels <= 256, "Virtual channels must be 1-256")
    require(physicalLinkCount > 0 && physicalLinkCount <= 4, "Physical links must be 1-4")
    println(s"[VcpPluginSimple] Setup complete")
  }

  during build new Area {
    println(s"[VcpPluginSimple] Build starting...")

    // Simplified VCP state
    val vcpEnabled = RegInit(False)
    val vcpReady = RegInit(True)
    val vcpStatus = Reg(Bits(32 bits)) init 0

    // Simplified channel management
    val channelOpen = Vec(RegInit(False), maxVirtualChannels)
    val channelError = Vec(RegInit(False), maxVirtualChannels)

    // Simplified link status
    val linkReady = Vec(RegInit(True), physicalLinkCount)
    val linkError = Vec(RegInit(False), physicalLinkCount)

    // Update status register
    vcpStatus := Cat(
      B"16'h0000", // Reserved
      linkError.reduce(_ || _), // Link errors
      linkReady.reduce(_ && _), // All links ready
      channelError.reduce(_ || _), // Channel errors
      vcpReady, // Ready
      vcpEnabled // Enabled
    )

    // Simplified VCP service
    addService(new VcpService {
      override def enable(): Unit = vcpEnabled := True
      override def disable(): Unit = vcpEnabled := False
      override def reset(): Unit = {
        vcpEnabled := False
        channelOpen.foreach(_ := False)
        channelError.foreach(_ := False)
        linkError.foreach(_ := False)
      }
      override def isEnabled: Bool = vcpEnabled
      override def isReady: Bool = vcpReady
      override def getStatus: Bits = vcpStatus

      override def openChannel(virtualChannel: UInt, physicalLink: UInt): Bool = {
        val channelValid = virtualChannel < maxVirtualChannels
        val linkValid = physicalLink < physicalLinkCount
        when(channelValid && linkValid) {
          // Simplified channel opening
          for (i <- 0 until maxVirtualChannels) {
            when(virtualChannel === i) {
              channelOpen(i) := True
              channelError(i) := False
            }
          }
        }
        channelValid && linkValid
      }

      override def closeChannel(virtualChannel: UInt): Bool = {
        val channelValid = virtualChannel < maxVirtualChannels
        when(channelValid) {
          for (i <- 0 until maxVirtualChannels) {
            when(virtualChannel === i) {
              channelOpen(i) := False
            }
          }
        }
        channelValid
      }

      override def isChannelOpen(virtualChannel: UInt): Bool = {
        val result = RegInit(False)
        for (i <- 0 until maxVirtualChannels) {
          when(virtualChannel === i) {
            result := channelOpen(i)
          }
        }
        result
      }

      override def getChannelStatus(virtualChannel: UInt): Bits = {
        val result = Reg(Bits(32 bits)) init 0
        for (i <- 0 until maxVirtualChannels) {
          when(virtualChannel === i) {
            result := Cat(channelError(i), channelOpen(i), B"30'h0")
          }
        }
        result
      }

      override def sendPacket(virtualChannel: UInt, data: Bits, isEnd: Bool): Bool = True
      override def canSendPacket(virtualChannel: UInt): Bool = True
      override def getTransmitCredit(virtualChannel: UInt): UInt = U(8)
      override def receivePacket(virtualChannel: UInt): Option[Bits] = None
      override def hasReceiveData(virtualChannel: UInt): Bool = False
      override def getReceiveBufferLevel(virtualChannel: UInt): UInt = U(0)

      override def getLinkStatus(physicalLink: UInt): Bits = {
        val result = Reg(Bits(32 bits)) init 0
        for (i <- 0 until physicalLinkCount) {
          when(physicalLink === i) {
            result := Cat(linkError(i), linkReady(i), B"30'h0")
          }
        }
        result
      }

      override def isLinkReady(physicalLink: UInt): Bool = {
        val result = RegInit(True)
        for (i <- 0 until physicalLinkCount) {
          when(physicalLink === i) {
            result := linkReady(i)
          }
        }
        result
      }

      override def hasLinkError(physicalLink: UInt): Bool = {
        val result = RegInit(False)
        for (i <- 0 until physicalLinkCount) {
          when(physicalLink === i) {
            result := linkError(i)
          }
        }
        result
      }

      override def clearLinkError(physicalLink: UInt): Unit = {
        for (i <- 0 until physicalLinkCount) {
          when(physicalLink === i) {
            linkError(i) := False
          }
        }
      }

      // Header area management
      override def setHeaderAreaBase(address: UInt): Unit = {}
      override def setHeaderAreaLimit(address: UInt): Unit = {}
      override def getHeaderAreaBase: UInt = U(0x80000000L)
      override def getHeaderAreaLimit: UInt = U(0x80001000L)

      // Channel configuration
      override def setChannelPriority(virtualChannel: UInt, priority: UInt): Unit = {}
      override def setChannelBufferSize(virtualChannel: UInt, size: UInt): Unit = {}
      override def setChannelTimeout(virtualChannel: UInt, timeout: UInt): Unit = {}

      // Flow control (simplified)
      override def sendAcknowledge(virtualChannel: UInt): Unit = {}
      override def requestCredit(virtualChannel: UInt, amount: UInt): Bool = True
      override def getFlowControlState(virtualChannel: UInt): Bits = B"16'h0808"

      // Error handling
      override def hasChannelError(virtualChannel: UInt): Bool = {
        val result = RegInit(False)
        for (i <- 0 until maxVirtualChannels) {
          when(virtualChannel === i) {
            result := channelError(i)
          }
        }
        result
      }

      override def getErrorStatus(virtualChannel: UInt): Bits = B"16'h0000"
      override def clearChannelError(virtualChannel: UInt): Unit = {
        for (i <- 0 until maxVirtualChannels) {
          when(virtualChannel === i) {
            channelError(i) := False
          }
        }
      }

      override def clearAllErrors(): Unit = {
        channelError.foreach(_ := False)
        linkError.foreach(_ := False)
      }

      // Statistics (simplified)
      override def getPacketsSent(virtualChannel: UInt): UInt = U(0)
      override def getPacketsReceived(virtualChannel: UInt): UInt = U(0)
      override def getBytesTransferred(virtualChannel: UInt): UInt = U(0)
      override def getErrorCount(virtualChannel: UInt): UInt = U(0)
    })

    println(s"[VcpPluginSimple] Build complete")
  }
}

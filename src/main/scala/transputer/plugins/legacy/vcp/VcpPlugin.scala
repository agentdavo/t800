package transputer.plugins.legacy.vcp

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.bus.bmb._
import transputer.Global._
import transputer.plugins.legacy.vcp.{VcpService, MemReadCmd, MemWriteCmd}

/** T9000 Virtual Channel Processor Plugin Implements DS-Link communication and packet routing Based
  * on T9000 specification chapters 12.4 and 12.5
  */
class VcpPlugin extends FiberPlugin {
  override def getDisplayName(): String = "VcpPlugin"
  setName("vcp")

  during setup new Area {
    println(s"[${VcpPlugin.this.getDisplayName()}] setup start")
    println(s"[${VcpPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${VcpPlugin.this.getDisplayName()}] build start")

    // VCP Configuration constants (from T9000 spec)
    val LINK_COUNT = 4
    val MAX_VIRTUAL_CHANNELS = 256
    val PACKET_BUFFER_SIZE = 1024
    val HEADER_REGION_SIZE = 256

    // VCP Control and Status Registers
    val vcpCommandReg = Reg(Bits(8 bits)) init 0
    val vcpStatusReg = Reg(Bits(8 bits)) init 0
    val vcpRunning = Reg(Bool()) init False
    val vcpReady = Reg(Bool()) init True

    // Link Configuration Registers (4 physical links)
    val linkModeRegs = Vec(Reg(Bits(8 bits)) init 0, LINK_COUNT)
    val linkMinHeaderRegs = Vec(Reg(Bits(16 bits)) init 0, LINK_COUNT)
    val linkMaxHeaderRegs = Vec(Reg(Bits(16 bits)) init B"16'hFFFF", LINK_COUNT)

    // Header Area Configuration (T9000 spec section 12.4)
    val hdrAreaBaseReg = Reg(UInt(32 bits)) init InternalMemStart
    val hdrAreaLimitReg = Reg(UInt(32 bits)) init (InternalMemStart + HEADER_REGION_SIZE)

    // Error Status Registers
    val linkErrorRegs = Vec(Reg(Bool()) init False, LINK_COUNT)
    val headerErrorRegs = Vec(Reg(Bool()) init False, LINK_COUNT)

    // Virtual Channel Buffers and Control
    val virtualChannelValid = Vec(Reg(Bool()) init False, MAX_VIRTUAL_CHANNELS)
    val channelReadyFlags = Vec(Reg(Bool()) init False, MAX_VIRTUAL_CHANNELS)
    val channelErrorFlags = Vec(Reg(Bool()) init False, MAX_VIRTUAL_CHANNELS)

    // Create flows inside logic area (simplified implementation)
    val memReadFlow = Flow(MemReadCmd())
    val memWriteFlow = Flow(MemWriteCmd())
    val memResponseFlow = Flow(Bits(32 bits))

    // VCP State Machine
    val vcpState = RegInit(U"2'b00") // 00=Reset, 01=Stopped, 10=Running, 11=Error

    // Command Processing
    when(vcpCommandReg(0)) { // Reset command
      vcpState := U"2'b00"
      vcpRunning := False
      vcpReady := True
      // Clear all channel states
      for (i <- 0 until MAX_VIRTUAL_CHANNELS) {
        virtualChannelValid(i) := False
        channelReadyFlags(i) := False
        channelErrorFlags(i) := False
      }
      // Clear link errors
      for (i <- 0 until LINK_COUNT) {
        linkErrorRegs(i) := False
        headerErrorRegs(i) := False
      }
      vcpCommandReg := 0
    }.elsewhen(vcpCommandReg(1)) { // Start command
      when(vcpState === U"2'b01") { // Only start from stopped state
        vcpState := U"2'b10"
        vcpRunning := True
        vcpReady := True
      }
      vcpCommandReg := 0
    }.elsewhen(vcpCommandReg(2)) { // Stop command
      when(vcpState === U"2'b10") { // Only stop from running state
        vcpState := U"2'b01"
        vcpRunning := False
        vcpReady := True
      }
      vcpCommandReg := 0
    }

    // Update status register
    vcpStatusReg := Cat(
      vcpState, // bits 1:0 - VCP state
      vcpRunning, // bit 2 - running flag
      vcpReady, // bit 3 - ready flag
      B"4'h0" // bits 7:4 - reserved
    )

    // Packet Header Validation Logic
    def validateHeader(link: Int, header: Bits): Bool = {
      val headerValue = header.asUInt
      val minHeader = linkMinHeaderRegs(link).asUInt
      val maxHeader = linkMaxHeaderRegs(link).asUInt
      val valid = (headerValue >= minHeader) && (headerValue <= maxHeader)

      when(!valid) {
        headerErrorRegs(link) := True
        when(!linkModeRegs(link)(1)) { // LocalizeError not set
          // Report error to control unit
        }
      }
      valid
    }

    // Virtual Channel Packet Processing (simplified implementation)
    // In a full implementation, this would handle packet routing through FIFOs
    // For now, just maintain basic VCP state

    // Memory interface handling
    memReadFlow.valid := False
    memReadFlow.payload := MemReadCmd().getZero

    memWriteFlow.valid := False
    memWriteFlow.payload := MemWriteCmd().getZero

    memResponseFlow.valid := False
    memResponseFlow.payload := 0

    // Note: registers are automatically visible in simulation

    // Service Implementation - implement existing VcpService interface
    addService(new VcpService {
      // Direct signal access (hybrid signal-method architecture)
      override def vcpStatus: Bits = vcpStatusReg
      override def channelStates: Vec[Bits] = Vec(channelReadyFlags.map(_.asBits))
      override def linkReady: Vec[Bool] = Vec(linkErrorRegs.map(!_))
      override def linkError: Vec[Bool] = Vec(linkErrorRegs)
      override def vlcbBaseAddr: Vec[UInt] = Vec.fill(LINK_COUNT)(hdrAreaBaseReg)

      // ChannelService interface methods
      override def txReady(link: UInt): Bool = vcpRunning && !linkErrorRegs(0) // Simplified
      override def push(link: UInt, data: Bits): Bool = vcpRunning
      override def rxValid(link: UInt): Bool = vcpRunning
      override def rxPayload(link: UInt): Bits = B"32'h0"
      override def rxAck(link: UInt): Unit = {}

      // VCP-specific method interfaces
      override def scheduleInput(channel: Int): Unit = {
        if (channel < MAX_VIRTUAL_CHANNELS) {
          channelReadyFlags(channel) := True
        }
      }

      override def scheduleOutput(channel: Int): Unit = {
        if (channel < MAX_VIRTUAL_CHANNELS) {
          channelReadyFlags(channel) := True
        }
      }

      override def getChannelState(channel: Int): Bits = {
        if (channel < MAX_VIRTUAL_CHANNELS) channelReadyFlags(channel).asBits else B"0"
      }

      override def setVcpCommand(cmd: Bits): Unit = {
        vcpCommandReg := cmd
      }

      override def getVcpStatus(): Bits = vcpStatusReg

      override def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit = {
        if (channel < MAX_VIRTUAL_CHANNELS) {
          when(vcpRunning) {
            // Simplified implementation - route to physical links
          }
        }
      }

      override def receiveAck(channel: Int): Bool = {
        if (channel < MAX_VIRTUAL_CHANNELS) channelReadyFlags(channel) else False
      }

      override def enqueueMessage(channel: Int, data: Bits): Unit = {
        if (channel < MAX_VIRTUAL_CHANNELS) {
          when(vcpRunning) {
            // Simplified implementation - would queue to channel buffer
          }
        }
      }

      override def updateRegisters(): Unit = {
        // Registers update automatically in SpinalHDL
      }
    })

    println(s"[${VcpPlugin.this.getDisplayName()}] build end")
  }
}

package t800.plugins

import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbDownSizerBridge, BmbOnChipRamMultiPort, BmbQueue}
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, SpiXdrParameter}
import t800.Global

/** Service for virtual channel management, extending ChannelSrv with VCP-specific functionality. */
trait VcpSrv extends ChannelSrv {
  def scheduleInput(channel: Int): Unit
  def scheduleOutput(channel: Int): Unit
  def getChannelState(channel: Int): Bits // e.g., INRI, OUTRO, SPEOPM, SPACK
  def setVcpCommand(cmd: Bits): Unit
  def getVcpStatus(): Bits
  def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit
  def receiveAck(channel: Int): Bool
  def enqueueMessage(channel: Int, data: Bits): Unit
}

/** Plugin for T9000 Virtual Channel Processor (VCP) with four SpiXdrMasterCtrl links. */
class VcpPlugin extends FiberPlugin {
  val version = "VcpPlugin v4.0"
  report(L"Initializing $version")

  object DBKeys {
    val VCP_CHANNEL_STATE = Database.blocking[Vec[Bits]]()
    val VCP_STATUS = Database.blocking[Bits]()
    val VCP_VLCB_BASE = Database.blocking[Bits]()
  }

  lazy val VCP_CHANNEL_STATE = Payload(Vec(Bits(8 bits), 2 * Global.LINK_COUNT.toInt)) // INRI, OUTRO, SPEOPM, SPACK
  lazy val VCP_STATUS = Payload(Bits(32 bits)) // VCPStatus register
  lazy val VCP_VLCB_BASE = Payload(Bits(32 bits)) // Base for VLCB memory

  lazy val srv = during setup new Area {
    val service = new VcpSrv {
      def txReady(link: UInt): Bool = links(link).spiCtrl.io.tx.ready
      def push(link: UInt, data: Bits): Bool = links(link).spiCtrl.io.tx.push(data)
      def rxValid(link: UInt): Bool = links(link).spiCtrl.io.rx.valid
      def rxPayload(link: UInt): Bits = links(link).spiCtrl.io.rx.payload
      def rxAck(link: UInt): Unit = links(link).spiCtrl.io.rx.ack()
      def scheduleInput(channel: Int): Unit = channels(channel).scheduleInput()
      def scheduleOutput(channel: Int): Unit = channels(channel).scheduleOutput()
      def getChannelState(channel: Int): Bits = channels(channel).getState()
      def setVcpCommand(cmd: Bits): Unit = vcpCommand := cmd
      def getVcpStatus(): Bits = vcpStatus
      def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit = channels(channel).sendPacket(data, isEnd)
      def receiveAck(channel: Int): Bool = channels(channel).receiveAck()
      def enqueueMessage(channel: Int, data: Bits): Unit = channels(channel).enqueueMessage(data)
    }
    addService(service)
    addService(new LinkBusSrv {
      def rdCmd = Flow(MemReadCmd())
      def rdRsp = Flow(Bits(32 bits))
      def wrCmd = Flow(MemWriteCmd())
    })
  }

  during setup {
    buildBefore(retains(
      host[SchedulerPlugin].lock,
      host[LinksPlugin].lock,
      host[MemoryManagementPlugin].lock
    ).lock)
  }

  def connectToSystemBus(bus: Bmb): Unit = {
    val downSizer = BmbDownSizerBridge(
      inputParameter = bus.p,
      outputParameter = BmbDownSizerBridge.outputParameterFrom(bus.p.access, 32).toBmbParameter()
    )
    bus >> downSizer.io.input
    val vcpBmb = downSizer.io.output
    vcpBmb >> vcpMemory.io.ctrl
    for (link <- links) {
      vcpBmb >> link.spiCtrl.io.ctrl
    }
  }

  def connectToDsLinks(links: Vec[Bmb]): Unit = {
    // No direct connection; SpiXdrMasterCtrl handles DS Links internally
  }

  lazy val logic = during build new Area {
    val pipeline = host.find[StageCtrlPipeline]
    val decode = pipeline.ctrl(2)
    val memory = pipeline.ctrl(4)

    // VCP BMB and memory parameters
    val vcpBmbParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 32,
        lengthWidth = 4,
        sourceWidth = 1,
        contextWidth = 0
      )
    )
    val vcpMemory = BmbOnChipRamMultiPort(
      portsParameter = Seq(vcpBmbParam),
      size = 2048 // 2 KB for VLCBs and headers
    )

    // VCP configuration registers
    val configSrv = host.find[Global.ConfigAccessSrv]
    val vcpCommand = Reg(Bits(32 bits)) init(0) // #0804
    val vcpStatus = Reg(Bits(32 bits)) init(0) // #0802
    val hdrAreaBase = Reg(Bits(32 bits)) init(Global.InternalMemStart) // #0901
    val link0HdrOffset = Reg(Bits(16 bits)) init(0) // #0808
    val link1HdrOffset = Reg(Bits(16 bits)) init(0) // #080C
    val link2HdrOffset = Reg(Bits(16 bits)) init(0) // #0810
    val link3HdrOffset = Reg(Bits(16 bits)) init(0) // #0814
    val link0MinHeader = Reg(Bits(16 bits)) init(0) // #0807
    val link0MaxHeader = Reg(Bits(16 bits)) init(U"16'hFFFF") // #0806
    val link1MinHeader = Reg(Bits(16 bits)) init(0) // #080B
    val link1MaxHeader = Reg(Bits(16 bits)) init(U"16'hFFFF") // #080A
    val link2MinHeader = Reg(Bits(16 bits)) init(0) // #080F
    val link2MaxHeader = Reg(Bits(16 bits)) init(U"16'hFFFF") // #080E
    val link3MinHeader = Reg(Bits(16 bits)) init(0) // #0813
    val link3MaxHeader = Reg(Bits(16 bits)) init(U"16'hFFFF") // #0812
    val link0Mode = Reg(Bits(3 bits)) init(0) // #0805
    val link1Mode = Reg(Bits(3 bits)) init(0) // #0809
    val link2Mode = Reg(Bits(3 bits)) init(0) // #080D
    val link3Mode = Reg(Bits(3 bits)) init(0) // #0811

    // Spawn four SpiXdrMasterCtrl links with bit-level protocol
    val spiParams = SpiXdrMasterCtrl.Parameters(
      dataWidth = 8,
      timerWidth = 12,
      spi = SpiXdrParameter(dataWidth = 8, ioRate = 2, ssWidth = 4)
    ).addFullDuplex(id = 0, rate = 1, ddr = true, dataWidth = 8)
      .addHalfDuplex(id = 1, rate = 1, ddr = false, spiWidth = 8, dataWidth = 8)

    val linkParams = SpiXdrMasterCtrl.MemoryMappingParameters(
      ctrl = spiParams,
      cmdFifoDepth = 32,
      rspFifoDepth = 32,
      xip = null
    )

    val links = for (i <- 0 until 4) yield new Area {
      val spiCtrl = SpiXdrMasterCtrl(linkParams, vcpBmbParam)
      val linkState = RegInit(U"001") // Reset state
      val buffer = Reg(Bits(32 bits)) init(0) // Packet buffer
      val packetHeader = Reg(Bits(16 bits)) init(0) // Header storage
      val errorFlag = Reg(Bool()) init(False)
      val dataLine = Reg(Bits(1 bits)) init(0) // Bit-level data
      val strobeLine = Reg(Bits(1 bits)) init(0) // Bit-level strobe

      // Link protocol state machine
      val packetState = RegInit(U"00") // 00: Idle, 01: Sending SP, 10: Receiving, 11: Ack
      val packetCount = Reg(UInt(8 bits)) init(0)
      val isEnd = Reg(Bool()) init(False)
      val tokenType = Reg(Bits(2 bits)) init(B"00") // 00: Data, 01: EOP, 10: EOM
      val parityBit = Reg(Bool()) init(False)

      // Bit-level protocol simulation
      val bitClock = Counter(8) // 8-bit token cycle
      when(bitClock.willOverflow) {
        bitClock.clear()
        when(packetState === U"01" || packetState === U"10") {
          val dataToken = Cat(parityBit, B"0", buffer(7 downto 0)) // Data token
          val controlToken = Cat(parityBit, B"1", tokenType) // Control token
          spiCtrl.io.tx.push(Mux(tokenType === B"00", dataToken, controlToken))
          dataLine := dataToken(0) // Simulate data edge
          strobeLine := ~strobeLine // Toggle strobe on no data edge
        }
      }

      // State transitions based on VCPCommand
      when(vcpCommand(0)) {
        linkState := U"001" // Reset
        RegNext(True, init = False, when = vcpCommand(0), for 256 cycles) // Hold for 256 cycles
      }
      when(vcpCommand(1) && linkState === U"001") { linkState := U"010" } // Start
      when(vcpCommand(2) && linkState === U"010") { linkState := U"100" } // Stop
      when(linkState === U"100") { // Stopping
        // Deactivate channels
      }
      when(linkState === U"101") { // Discarding
        when(srv.rxValid(U(i))) { srv.rxAck(U(i)) } // Discard packets
      }

      // Packet handling with token-level protocol
      when(packetState === U"00" && srv.txReady(U(i)) && linkState === U"010") {
        packetState := U"01" // Sending SP
        tokenType := B"00" // Data token for SP
        parityBit := ~^B"00000000" // Parity for SP
        buffer := B"00000000" // Placeholder SP
        packetCount := 1
      }
      when(packetState === U"01" && srv.txReady(U(i))) {
        when(packetCount <= 32) { // Up to 32 bytes
          spiCtrl.io.tx.push(buffer)
          packetCount := packetCount + 1
        } otherwise {
          tokenType := isEnd ? B"10" : B"01" // EOM or EOP
          parityBit := ~^(isEnd ? B"00000010" : B"00000001") // Parity for EOM/EOP
          packetState := U"10" // Receiving
        }
      }
      when(packetState === U"10" && srv.rxValid(U(i))) {
        val received = srv.rxPayload(U(i))
        parityBit := received(9)
        tokenType := received(8 downto 7)
        packetHeader := received(15 downto 0) // Store header
        val headerValid = packetHeader >= link0MinHeader && packetHeader <= link0MaxHeader
        when(!headerValid || tokenType === B"11") { // Invalid token
          errorFlag := True
          vcpStatus(0) := True // Error
          vcpStatus(2 downto 1) := B"00" // Header out of range
        }
        buffer := Cat(B"0", received(7 downto 0)) // Store data
        packetState := U"11" // Ack
        srv.rxAck(U(i))
      }
      when(packetState === U"11" && srv.txReady(U(i))) {
        spiCtrl.io.tx.push(Cat(parityBit, B"1", B"01")) // Ack token
        packetState := U"00"
        packetCount := 0
        isEnd := False
        when(errorFlag && !link0Mode(1)) { // Report error if not localized
          vcpStatus(5 downto 4) := U(i, 2 bits)
          vcpStatus(31 downto 16) := packetHeader
        }
        errorFlag := False
      }
    }

    // Virtual link buffers (one per channel for first packet)
    val virtualBuffers = for (i <- 0 until 2 * Global.LINK_COUNT.toInt) yield new Area {
      val buffer = Reg(Bits(32 bits)) init(0)
      val valid = Reg(Bool()) init(False)
    }

    // Channel state machines (8 channels, 2 per link)
    val channels = for (i <- 0 until 2 * Global.LINK_COUNT.toInt) yield new Area {
      val inri = Reg(UInt(2 bits)) init(0) // #in - #run(in)
      val outro = Reg(UInt(2 bits)) init(0) // #out - #run(out)
      val speopm = Reg(UInt(2 bits)) init(0) // #sp - (#cop + #com)
      val spack = Reg(UInt(2 bits)) init(0) // #sp - #ack
      val linkIdx = i / 2
      val vlcbAddr = Reg(Bits(32 bits)) init(0)
      val messageQueue = Stream(Bits(32 bits))

      // Safety conditions
      assert(inri <= 1, "INRI must be <= 1")
      assert(outro <= 1, "OUTRO must be <= 1")
      assert(speopm <= 1, "SPEOPM must be <= 1")
      assert(spack <= 1, "SPACK must be <= 1")

      def scheduleInput(): Unit = {
        when(inri === 0 && links(linkIdx).linkState === U"010" && !virtualBuffers(i).valid) {
          inri := inri + 1
          val scheduler = host.find[SchedulerPlugin]
          scheduler.srv.terminateCurrent() // Deschedule input process
        }
      }

      def scheduleOutput(): Unit = {
        when(outro === 0 && links(linkIdx).linkState === U"010" && messageQueue.valid) {
          outro := outro + 1
          val scheduler = host.find[SchedulerPlugin]
          scheduler.srv.terminateCurrent() // Deschedule output process
        }
      }

      def sendPacket(data: Bits, isEnd: Bool): Unit = {
        when(speopm === 0 && links(linkIdx).linkState === U"010" && messageQueue.valid) {
          speopm := speopm + 1
          links(linkIdx).buffer := data
          links(linkIdx).isEnd := isEnd
          links(linkIdx).packetState := U"01" // Trigger sending
          messageQueue.ready := True
        }
      }

      def receiveAck(): Bool = spack > 0 && links(linkIdx).packetState === U"11"

      def enqueueMessage(data: Bits): Unit = {
        messageQueue.payload := data
        messageQueue.valid := True
      }

      def getState(): Bits = B"0" ## spack ## speopm ## outro ## inri

      // VLCB address calculation
      val headerOffset = Mux(linkIdx === 0, link0HdrOffset,
        Mux(linkIdx === 1, link1HdrOffset,
          Mux(linkIdx === 2, link2HdrOffset, link3HdrOffset)))
      vlcbAddr := hdrAreaBase + ((U(i) - headerOffset.asUInt).resized << 5) // vlink.shift = 5

      // State updates from pipeline
      when(decode.isValid && decode(MEM_ADDR) === U(i)) {
        switch(decode(OPCODE)) {
          is(B"00000001") { scheduleInput() } // in
          is(B"00000010") { scheduleOutput() } // out
          is(B"00000011") { when(inri > 0) { inri := inri - 1 } } // run(in)
          is(B"00000100") { when(outro > 0) { outro := outro - 1 } } // run(out)
          is(B"00000101") { sendPacket(B"00000000", False) } // sp
          is(B"00000110") { when(speopm > 0) { speopm := speopm - 1 } } // cop/com
          is(B"00000111") { enqueueMessage(B"00001111") } // ack placeholder
        }
      }

      // Buffer first packet if input not ready
      when(links(linkIdx).packetState === U"10" && inri === 0) {
        virtualBuffers(i).buffer := links(linkIdx).buffer
        virtualBuffers(i).valid := True
      }
      when(inri > 0 && virtualBuffers(i).valid) {
        vcpMemory.io.ctrl.cmd.valid := True
        vcpMemory.io.ctrl.cmd.opcode := 1 // Write
        vcpMemory.io.ctrl.cmd.address := vlcbAddr
        vcpMemory.io.ctrl.cmd.data := virtualBuffers(i).buffer
        vcpMemory.io.ctrl.cmd.length := 0
        virtualBuffers(i).valid := False
      }
    }

    // Update payloads
    VCP_CHANNEL_STATE := Vec(channels.map(_.getState()))
    VCP_STATUS := vcpStatus
    VCP_VLCB_BASE := hdrAreaBase

    // Initialization and error handling
    when(vcpCommand(0)) { // Reset
      for (link <- links) { link.linkState := U"001" }
      vcpStatus := 0
      for (channel <- channels) { channel.inri := 0; channel.outro := 0; channel.speopm := 0; channel.spack := 0 }
    }
    when(vcpStatus(0)) { // Error state
      for (link <- links) { link.linkState := U"101" } // Discarding
    }
  }
}

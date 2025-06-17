package transputer.plugins.pmi

import transputer.plugins._
import transputer.plugins.cache.{MainCachePlugin, WorkspaceCachePlugin}

import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.database._
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, BmbSpiXdrMasterCtrl}
import spinal.lib.bus.bmb.{
  Bmb,
  BmbParameter,
  BmbAccessParameter,
  BmbDecoder,
  BmbArbiter,
  BmbDownSizerBridge
}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}

// The PmiPlugin implements a 64-bit DDR external memory interface with four BmbSpiXdrMasterCtrl channels
// Supporting 16 devices (ssWidth = 4), mapped to 1/4 address space via BmbDecoder.
// Uses burst transactions for high-bandwidth 16-byte DDR refills and single-beat octal SPI for configuration
// Integrated with the Memory stage, ensuring low-latency access and scalability

class PmiPlugin extends FiberPlugin {
  val version = "PmiPlugin v2.0"
  report(L"Initializing $version")

  object DBKeys {
    val PMI_ADDR = Database.blocking[Bits]()
    val PMI_DATA = Database.blocking[Bits]()
  }

  lazy val PMI_ADDR = Payload(Bits(32 bits))
  lazy val PMI_DATA = Payload(Bits(64 bits))

  lazy val srv = during setup new Area {
    val service = PmiAccessSrv()
    addService(service)
  }

  buildBefore(
    retains(
      host[MainCachePlugin].lock,
      host[WorkspaceCachePlugin].lock
    ).lock
  )

  lazy val logic = during build new Area {
    val memory = host.find[StageCtrlPipeline].ctrl(3)

    // BMB bus parameters (64-bit data, 32-bit address, burst support)
    val bmbParameter = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = 32,
        dataWidth = 64,
        lengthWidth = 4, // Supports 16-byte bursts (two 64-bit beats)
        sourceWidth = 0,
        contextWidth = 0
      ),
      sourceCount = 1
    )

    // Channel BMB parameters (32-bit data, single-beat for SPI, burst for DDR)
    val channelBmbParameter = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = 32,
        dataWidth = 32,
        lengthWidth = 4,
        sourceWidth = 0,
        contextWidth = 0
      ),
      sourceCount = 1
    )

    // Channel address mappings (bits 31:30)
    val channelMappings = Seq(
      SizeMapping(0x00000000L, 0x40000000L, B(0, 2 bits)), // Channel 0
      SizeMapping(0x40000000L, 0x40000000L, B(1, 2 bits)), // Channel 1
      SizeMapping(0x80000000L, 0x40000000L, B(2, 2 bits)), // Channel 2
      SizeMapping(0xc0000000L, 0x40000000L, B(3, 2 bits)) // Channel 3
    )

    // Decoder for channel routing
    val decoder = BmbDecoder(
      p = bmbParameter,
      mappings = channelMappings,
      capabilities = Seq.fill(4)(channelBmbParameter),
      pendingMax = 63
    )

    // Arbiter for channel responses
    val arbiter = BmbArbiter(
      inputsParameter = Seq.fill(4)(channelBmbParameter),
      outputParameter = bmbParameter,
      lowerFirstPriority = false,
      pendingInvMax = 8
    )

    // Four SpiXdrMasterCtrl channels
    val channels = for (i <- 0 until 4) yield new Area {
      val downSizer = BmbDownSizerBridge(
        inputParameter = bmbParameter,
        outputParameter =
          BmbDownSizerBridge.outputParameterFrom(bmbParameter.access, 32).toBmbParameter()
      )
      val spiCtrl = BmbSpiXdrMasterCtrl(
        p = SpiXdrMasterCtrl.MemoryMappingParameters(
          ctrl = SpiXdrMasterCtrl
            .Parameters(
              dataWidth = 8,
              timerWidth = 12,
              spi = SpiXdrParameter(
                dataWidth = 8,
                ioRate = 2,
                ssWidth = 4
              )
            )
            .addFullDuplex(0, 1, true, 8) // DDR for bursts
            .addHalfDuplex(1, 1, false, 8), // Octal SPI
          cmdFifoDepth = 32,
          rspFifoDepth = 32,
          xip = null
        ),
        ctrlParameter = channelBmbParameter
      )
      decoder.io.outputs(i) >> downSizer.io.input
      downSizer.io.output >> spiCtrl.io.ctrl
      spiCtrl.io.ctrl >> arbiter.io.inputs(i)
    }

    // Connect arbiter to pipeline
    decoder.io.input.cmd.valid := memory.isValid
    decoder.io.input.cmd.opcode := srv.writeEnable ? 1 | 0
    decoder.io.input.cmd.address := memory(PMI_ADDR)
    decoder.io.input.cmd.length := 15 // 16 bytes for bursts
    decoder.io.input.cmd.data := memory(PMI_DATA)
    arbiter.io.output.rsp.ready := True

    // External memory simulation
    val externalMem = Vec(Vec(Mem(Bits(64 bits), 4096), 4), 4) // 256 KB per device
    val strobeTimings = Vec(Reg(UInt(8 bits)) init (0), 4)
    val pageModeEns = Vec(Reg(Bool()) init (False), 4)
    val deviceSels = Vec(Reg(UInt(2 bits)) init (0), 4)

    // DDR access logic
    def read(addr: Bits): Bits = {
      srv.addr := addr
      srv.isValid := False
      val chanIdx = addr(31 downto 30).asUInt
      channels(chanIdx).spiCtrl.io.ctrl.cmd.valid := True
      channels(chanIdx).spiCtrl.io.ctrl.cmd.opcode := 0 // Read
      channels(chanIdx).spiCtrl.io.ctrl.cmd.address := addr
      channels(chanIdx).spiCtrl.io.ctrl.cmd.length := 15 // 16-byte burst
      channels(chanIdx).spiCtrl.io.ctrl.cmd.data := 0
      channels(chanIdx).spiCtrl.io.spi.ss := B(1 << deviceSels(chanIdx), 4 bits)

      when(
        channels(chanIdx).spiCtrl.io.ctrl.rsp.valid && channels(
          chanIdx
        ).spiCtrl.io.spi.sclk.write === B(1, 2 bits)
      ) {
        srv.dataOut := externalMem(chanIdx)(deviceSels(chanIdx)).readSync(addr(13 downto 3).asUInt)
        srv.isValid := channels(chanIdx).spiCtrl.io.ctrl.rsp.last // Valid on last beat
      }
      memory.haltWhen(!srv.isValid)
      srv.dataOut
    }

    def write(addr: Bits, data: Bits): Unit = {
      srv.addr := addr
      srv.dataIn := data
      val chanIdx = addr(31 downto 30).asUInt
      channels(chanIdx).spiCtrl.io.ctrl.cmd.valid := True
      channels(chanIdx).spiCtrl.io.ctrl.cmd.opcode := 1 // Write
      channels(chanIdx).spiCtrl.io.ctrl.cmd.address := addr
      channels(chanIdx).spiCtrl.io.ctrl.cmd.length := 15 // 16-byte burst
      channels(chanIdx).spiCtrl.io.ctrl.cmd.data := data(31 downto 0)
      channels(chanIdx).spiCtrl.io.spi.ss := B(1 << deviceSels(chanIdx), 4 bits)

      when(
        srv.writeEnable && channels(chanIdx).spiCtrl.io.ctrl.rsp.valid && channels(
          chanIdx
        ).spiCtrl.io.ctrl.rsp.last
      ) {
        externalMem(chanIdx)(deviceSels(chanIdx)).writeAsync(addr(13 downto 3).asUInt, data)
        srv.isValid := True
      }
    }

    // Octal SPI configuration logic (per channel)
    val spiConfigs = for (i <- 0 until 4) yield new Area {
      val configState = Reg(UInt(2 bits)) init (0)
      val configData = Reg(Bits(8 bits)) init (0)
      val bitCount = Reg(UInt(4 bits)) init (0)

      when(channels(i).spiCtrl.io.spi.ss(deviceSels(i)) === False) {
        switch(configState) {
          is(0) { // Idle
            channels(i).spiCtrl.io.ctrl.cmd.valid := True
            channels(i).spiCtrl.io.ctrl.cmd.opcode := 1 // Write
            channels(i).spiCtrl.io.ctrl.cmd.address := 0x0 // Command register
            channels(i).spiCtrl.io.ctrl.cmd.length := 0 // Single-beat
            channels(
              i
            ).spiCtrl.io.ctrl.cmd.data := (True ## False ## True ## U"8'h01" ## U"8'h00").asBits
            when(channels(i).spiCtrl.io.ctrl.rsp.valid) {
              configState := 1
              bitCount := 0
            }
          }
          is(1) { // Send config
            channels(i).spiCtrl.io.ctrl.cmd.address := 0x58 // Data register
            channels(i).spiCtrl.io.ctrl.cmd.length := 0 // Single-beat
            channels(i).spiCtrl.io.ctrl.cmd.data := configData.asBits(32 bits)
            when(channels(i).spiCtrl.io.ctrl.rsp.valid) {
              bitCount := bitCount + 1
              when(bitCount === 7) {
                configState := 2
                when(configData === B"00000001") {
                  strobeTimings(i) := channels(i).spiCtrl.io.ctrl.rsp.data(7 downto 0).asUInt
                } elsewhen (configData === B"00000010") {
                  pageModeEns(i) := channels(i).spiCtrl.io.ctrl.rsp.data(0)
                }
              }
            }
          }
          is(2) { // Complete
            configState := 0
          }
        }
      }
    }

    // Pipeline integration
    val pmiLogic = new Area {
      when(memory.isValid) {
        memory.insert(PMI_ADDR) := srv.addr
        memory.insert(PMI_DATA) := srv.dataOut
        DBKeys.PMI_ADDR.set(srv.addr)
        DBKeys.PMI_DATA.set(srv.dataOut)
      }
    }

    // Debug logging
    when(srv.isValid && debugEn) {
      val chanIdx = srv.addr(31 downto 30).asUInt
      report(
        L"PMI addr=$PMI_ADDR data=$PMI_DATA pageMode=${pageModeEns(chanIdx)} device=${deviceSels(chanIdx)} channel=$chanIdx"
      )
    }
  }
}

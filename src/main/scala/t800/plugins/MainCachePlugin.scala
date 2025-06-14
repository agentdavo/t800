import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.database_
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbOnChipRamMultiPort, BmbUnburstify, BmbArbiter, BmbDecoder, BmbDownSizerBridge}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}

// The MainCachePlugin implements four BmbOnChipRamMultiPort banks (4 KB each, 32-bit data/32-bit address, two read ports)
// accessed via 32-bit unburstified BMB interfaces, mapped using BmbDecoder
// Single-beat transactions ensure single-cycle, zero-latency access for cache hits, saving logic area
// It uses BmbArbiter for pipeline arbitration and BmbDownSizerBridge for PMI refills, supporting two simultaneous CPU reads

case class MainCacheAccessSrv() extends Bundle {
  val addr = Bits(32 bits)
  val dataOut = Bits(128 bits) // 16 bytes per line
  val isHit = Bool()
  val writeEnable = Bool()
  val writeData = Bits(128 bits)
}

class MainCachePlugin extends FiberPlugin {
  val version = "MainCachePlugin v1.6"
  report(L"Initializing $version")

  object DBKeys {
    val CACHE_ADDR = Database.blocking[Bits]()
    val CACHE_DATA = Database.blocking[Bits]()
  }

  lazy val CACHE_ADDR = Payload(Bits(32 bits))
  lazy val CACHE_DATA = Payload(Bits(128 bits))

  lazy val srv = during setup new Area {
    val service = MainCacheAccessSrv()
    addService(service)
  }

  buildBefore(retains(host[PmiPlugin].lock, host[MemoryManagementPlugin].lock, host[WorkspaceCachePlugin].lock).lock)

  lazy val logic = during build new Area {
    val fetch = host.find[StageCtrlPipeline].ctrl(0)
    val memory = host.find[StageCtrlPipeline].ctrl(4)

    // BMB bus parameters (32-bit data, 32-bit address)
    val bmbParameter = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = 32,
        dataWidth = 32,
        lengthWidth = 2, // Single-beat transactions
        sourceWidth = 0,
        contextWidth = 0
      ),
      sourceCount = 1
    )

    // Address mappings for four banks (bits 4:5)
    val bankMappings = Seq(
      SizeMapping(0x00000000L, 0x40000000L, B(0, 2 bits)), // Bank 0: addr[5:4] = 00
      SizeMapping(0x00000000L, 0x40000000L, B(1, 2 bits)), // Bank 1: addr[5:4] = 01
      SizeMapping(0x00000000L, 0x40000000L, B(2, 2 bits)), // Bank 2: addr[5:4] = 10
      SizeMapping(0x00000000L, 0x40000000L, B(3, 2 bits))  // Bank 3: addr[5:4] = 11
    )

    // Arbiter for pipeline inputs (Fetch/Memory)
    val arbiter = BmbArbiter(
      inputsParameter = Seq.fill(2)(bmbParameter),
      outputParameter = bmbParameter,
      lowerFirstPriority = false
    )

    // Decoder for bank routing
    val decoder = BmbDecoder(
      p = bmbParameter,
      mappings = bankMappings,
      capabilities = Seq.fill(4)(bmbParameter),
      pendingMax = 63
    )

    // Connect pipeline to arbiter
    arbiter.io.inputs(0).cmd.valid := fetch.isValid
    arbiter.io.inputs(0).cmd.opcode := 0 // Read
    arbiter.io.inputs(0).cmd.address := fetch(PC)
    arbiter.io.inputs(0).cmd.length := 0
    arbiter.io.inputs(0).cmd.data := 0
    arbiter.io.inputs(0).rsp.ready := True

    arbiter.io.inputs(1).cmd.valid := memory.isValid && srv.writeEnable
    arbiter.io.inputs(1).cmd.opcode := 1 // Write
    arbiter.io.inputs(1).cmd.address := memory(CACHE_ADDR)
    arbiter.io.inputs(1).cmd.length := 0
    arbiter.io.inputs(1).cmd.data := memory(CACHE_DATA)(31 downto 0)
    arbiter.io.inputs(1).rsp.ready := True

    // Down-sizer for PMI refills
    val pmiDownSizer = BmbDownSizerBridge(
      inputParameter = host[PmiPlugin].srv.p,
      outputParameter = BmbDownSizerBridge.outputParameterFrom(host[PmiPlugin].srv.p.access, 32).toBmbParameter()
    )

    // Four cache banks, each with multi-port RAM
    case class CacheBank(portId: Int) extends Area {
      // Multi-port RAM with two read ports
      val ram = BmbOnChipRamMultiPort(
        portsParameter = Seq.fill(2)(bmbParameter), // Two ports for simultaneous reads
        size = 4096 // 4 KB
      )

      // Connect decoder output to RAM ports via unburstify
      val unburstify = Seq.fill(2)(BmbUnburstify(bmbParameter))
      decoder.io.outputs(portId) >> unburstify(0).io.input // Port 0 (primary read/write)
      unburstify(0).io.output >> ram.io.buses(0)
      unburstify(1).io.input.cmd.valid := fetch.isValid && fetch(PC)(5 downto 4).asUInt === portId
      unburstify(1).io.input.cmd.opcode := 0 // Read (secondary read port)
      unburstify(1).io.input.cmd.address := fetch(PC)
      unburstify(1).io.input.cmd.length := 0
      unburstify(1).io.input.cmd.data := 0
      unburstify(1).io.input.rsp.ready := True
      unburstify(1).io.output >> ram.io.buses(1)

      // Cache metadata
      val tagMem = Mem(Bits(26 bits), 256)
      val validBits = Mem(Bool(), 256)
      val dirtyBits = Mem(Bool(), 256)
      val emptyLine = Reg(UInt(8 bits)) init(0)

      def read(addr: Bits, portIdx: Int): (Bool, Bits) = {
        val lineIdx = addr(11 downto 4).asUInt
        val tag = addr(31 downto 6)
        val isHit = validBits.readSync(lineIdx) && tagMem.readSync(lineIdx) === tag
        val ramAddr = addr(11 downto 2).asUInt
        val dataLow = ram.io.buses(portIdx).rsp.data
        val dataHigh = RegNextWhen(dataLow, ram.io.buses(portIdx).rsp.valid)
        val dataOut = dataHigh ## dataLow ## dataHigh ## dataLow

        when(isRamMode) {
          isHit := True
        } elsewhen(!isHit && !isRamMode) {
          fetch.haltWhen(True)
          pmiDownSizer.io.input.cmd.valid := True
          pmiDownSizer.io.input.cmd.opcode := 0 // Read
          pmiDownSizer.io.input.cmd.address := addr
          pmiDownSizer.io.input.cmd.length := 0
          pmiDownSizer.io.input.cmd.data := 0
          val refillData = pmiDownSizer.io.output.rsp.data
          ram.io.buses(0).cmd.opcode := 1 // Write (use primary port)
          ram.io.buses(0).cmd.address := lineIdx @@ U"2'b00"
          ram.io.buses(0).cmd.data := refillData(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (lineIdx @@ U"2'b01") @@ U"2'b00"
            ram.io.buses(0).cmd.data := refillData(63 downto 32)
          }
          tagMem.write(lineIdx, tag)
          validBits.write(lineIdx, True)
          dirtyBits.write(lineIdx, False)
          emptyLine := emptyLine + 1
        }

        (isHit, dataOut)
      }

      def write(addr: Bits, data: Bits): Unit = {
        val lineIdx = addr(11 downto 4).asUInt
        val tag = addr(31 downto 6)
        val isHit = validBits.readSync(lineIdx) && tagMem.readSync(lineIdx) === tag
        val ramAddr = addr(11 downto 2).asUInt

        when(isRamMode || isHit) {
          ram.io.buses(0).cmd.opcode := 1 // Write (use primary port)
          ram.io.buses(0).cmd.address := ramAddr @@ U"2'b00"
          ram.io.buses(0).cmd.data := data(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (ramAddr + 1) @@ U"2'b00"
            ram.io.buses(0).cmd.data := data(63 downto 32)
          }
          tagMem.write(lineIdx, tag)
          validBits.write(lineIdx, True)
          dirtyBits.write(lineIdx, True)
          if (!isRamMode) {
            pmiDownSizer.io.input.cmd.opcode := 1 // Write
            pmiDownSizer.io.input.cmd.address := addr
            pmiDownSizer.io.input.cmd.data := data(63 downto 0)
            // Write-through to Workspace Cache
            host[WorkspaceCachePlugin].write(addr, data)
          }
        } elsewhen(!isRamMode) {
          fetch.haltWhen(True)
          when(dirtyBits.readSync(emptyLine)) {
            pmiDownSizer.io.input.cmd.opcode := 1 // Write
            pmiDownSizer.io.input.cmd.address := addr
            pmiDownSizer.io.input.cmd.data := ram.io.buses(0).rsp.data ## ram.io.buses(0).rsp.data
          }
          ram.io.buses(0).cmd.opcode := 1 // Write
          ram.io.buses(0).cmd.address := emptyLine @@ U"2'b00"
          ram.io.buses(0).cmd.data := data(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (emptyLine @@ U"2'b01") @@ U"2'b00"
            ram.io.buses(0).cmd.data := data(63 downto 32)
          }
          tagMem.write(emptyLine, tag)
          validBits.write(emptyLine, True)
          dirtyBits.write(emptyLine, True)
          emptyLine := emptyLine + 1
        }
      }
    }

    val banks = for (i <- 0 until 4) yield CacheBank(i)

    val cacheMode = Reg(Bits(2 bits)) init(0) // 00: Full RAM, 01: Full Cache, 10: Half RAM/Half Cache
    val isRamMode = cacheMode === 0 || (cacheMode === 2 && fetch(PC)(13) === 0)

    val bankSel = fetch(PC)(5 downto 4).asUInt
    val bankAddr = fetch(PC)(31 downto 6) @@ fetch(PC)(3 downto 0)

    val bankHits = Vec(Bool(), 4)
    val bankDataOuts = Vec(Bits(128 bits), 4)
    for ((bank, i) <- banks.zipWithIndex) {
      when(bankSel === i) {
        val (hit, dataOut) = bank.read(bankAddr, 0) // Primary read port
        bankHits(i) := hit
        bankDataOuts(i) := dataOut
      } otherwise {
        bankHits(i) := False
        bankDataOuts(i) := 0
      }
    }

    val cacheLogic = new Area {
      when(fetch.isValid) {
        memory.insert(CACHE_ADDR) := fetch(PC)
        memory.insert(CACHE_DATA) := bankDataOuts(bankSel)
        srv.isHit := bankHits(bankSel)
      }
    }

    when(srv.isHit && debugEn) {
      report(L"MAIN_CACHE addr=$CACHE_ADDR hit=$isHit bank=$bankSel mode=$cacheMode")
    }
  }
}

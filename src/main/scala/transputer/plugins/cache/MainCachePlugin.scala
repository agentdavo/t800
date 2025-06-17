package t800.plugins.cache

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.{
  Bmb,
  BmbParameter,
  BmbAccessParameter,
  BmbOnChipRamMultiPort,
  BmbUnburstify,
  BmbArbiter,
  BmbDecoder,
  BmbDownSizerBridge
}
import t800.plugins.pmi.PmiPlugin
import t800.plugins.{AddressTranslationSrv, WorkspaceCacheSrv, SystemBusSrv, Fetch}
import t800.plugins.pipeline.PipelineStageSrv
import t800.{Global, T800}

class MainCachePlugin extends FiberPlugin {
  val version = "MainCachePlugin v1.7"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  object DBKeys {
    val CACHE_ADDR = Database.blocking[Bits]()
    val CACHE_DATA = Database.blocking[Bits]()
  }

  lazy val CACHE_ADDR = Payload(Bits(Global.ADDR_BITS bits))
  lazy val CACHE_DATA = Payload(Bits(128 bits))

  lazy val srv = during setup new Area {
    val service = new MainCacheAccessSrv {
      val addr = Reg(Bits(Global.ADDR_BITS bits)) init 0
      val dataOut = Reg(Bits(128 bits)) init 0
      val isHit = Reg(Bool()) init False
      val writeEnable = Reg(Bool()) init False
      val writeData = Reg(Bits(128 bits)) init 0
    }
    addService(service)
    addService(new MainCacheSrv {
      def read(addr: UInt): Bits = {
        service.addr := addr.asBits
        when(service.isHit) { service.dataOut } otherwise { B(0, 128 bits) }
      }
      def write(addr: UInt, data: Bits): Unit = {
        service.addr := addr.asBits
        service.writeEnable := True
        service.writeData := data
      }
    })
    val cacheIf = new CacheAccessSrv {
      override val req = Flow(CacheReq())
      override val rsp = Flow(CacheRsp())
    }
    cacheIf.req.setIdle()
    cacheIf.rsp.setIdle()
    addService(cacheIf)
    service
  }

  buildBefore(
    retains(
      host[PmiPlugin].lock,
      host[WorkspaceCachePlugin].lock
    ).lock
  )

  lazy val logic = during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    val pipe = host[PipelineStageSrv]
    val fetch = pipe.ctrl(0) // Fetch stage
    val memory = pipe.ctrl(3) // Memory stage
    val systemBus = host[SystemBusSrv].bus // 128-bit BMB system bus

    // BMB bus parameters (32-bit data, single-beat)
    val bmbParameter = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 32,
        lengthWidth = 0, // Single-beat transactions
        sourceWidth = 1,
        contextWidth = 0
      )
    )

    // Address mappings for four banks (bits 5:4)
    val bankMappings = Seq(
      SizeMapping(0x00000000L, 0x1000L), // Bank 0: addr[5:4] = 00
      SizeMapping(0x1000L, 0x1000L), // Bank 1: addr[5:4] = 01
      SizeMapping(0x2000L, 0x1000L), // Bank 2: addr[5:4] = 10
      SizeMapping(0x3000L, 0x1000L) // Bank 3: addr[5:4] = 11
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
    arbiter.io.output >> decoder.io.input

    // Down-sizer for PMI refills
    val pmiBmb = host[PmiPlugin].srv.bus
    val pmiDownSizer = BmbDownSizerBridge(
      inputParameter = T800.systemBusParam,
      outputParameter = bmbParameter
    )
    pmiDownSizer.io.output >> pmiBmb

    // Four cache banks, each with multi-port RAM
    case class CacheBank(portId: Int) extends Area {
      val ram = BmbOnChipRamMultiPort(
        portsParameter = Seq.fill(2)(bmbParameter), // Two ports for simultaneous reads
        size = 4096 // 4 KB
      )

      // Connect decoder output to RAM ports via unburstify
      val unburstify = Seq.fill(2)(BmbUnburstify(bmbParameter))
      decoder.io.outputs(portId) >> unburstify(0).io.input // Port 0 (primary read/write)
      unburstify(0).io.output >> ram.io.buses(0)
      unburstify(1).io.input.cmd.valid := fetch.isValid && fetch(Fetch.FETCH_PC)(
        5 downto 4
      ).asUInt === portId
      unburstify(1).io.input.cmd.opcode := 0 // Read (secondary read port)
      unburstify(1).io.input.cmd.address := fetch(Fetch.FETCH_PC)
      unburstify(1).io.input.cmd.length := 0
      unburstify(1).io.input.cmd.data := 0
      unburstify(1).io.input.cmd.mask := B"1111"
      unburstify(1).io.input.rsp.ready := True
      unburstify(1).io.output >> ram.io.buses(1)

      // Cache metadata
      val tagMem = Mem(Bits(26 bits), 256)
      val validBits = Mem(Bool(), 256)
      val dirtyBits = Mem(Bool(), 256)
      val emptyLine = Reg(UInt(8 bits)) init 0

      def read(addr: UInt, portIdx: Int): (Bool, Bits) = {
        val phys = host[AddressTranslationSrv].translate(addr.asBits)
        val lineIdx = phys(11 downto 4).asUInt
        val tag = phys(31 downto 6)
        val isHit = validBits.readSync(lineIdx) && tagMem.readSync(lineIdx) === tag
        val ramAddr = addr(11 downto 2).asUInt
        val dataLow = ram.io.buses(portIdx).rsp.data
        val dataHigh = RegNextWhen(dataLow, ram.io.buses(portIdx).rsp.valid)
        val dataOut = dataHigh ## dataLow ## dataHigh ## dataLow

        when(isRamMode) {
          isHit := True
        } elsewhen (!isHit && !isRamMode) {
          fetch.haltIt()
          pmiDownSizer.io.input.cmd.valid := True
          pmiDownSizer.io.input.cmd.opcode := 0 // Read
          pmiDownSizer.io.input.cmd.address := phys.asUInt
          pmiDownSizer.io.input.cmd.length := 0
          pmiDownSizer.io.input.cmd.data := 0
          pmiDownSizer.io.input.cmd.mask := B"1111"
          val refillData = pmiDownSizer.io.input.rsp.data
          ram.io.buses(0).cmd.opcode := 1 // Write
          ram.io.buses(0).cmd.address := lineIdx @@ U"00"
          ram.io.buses(0).cmd.data := refillData(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (lineIdx @@ U"01") @@ U"00"
            ram.io.buses(0).cmd.data := refillData(63 downto 32)
          }
          tagMem.write(lineIdx, tag)
          validBits.write(lineIdx, True)
          dirtyBits.write(lineIdx, False)
          emptyLine := emptyLine + 1
        }

        (isHit, dataOut)
      }

      def write(addr: UInt, data: Bits): Unit = {
        val phys = host[AddressTranslationSrv].translate(addr.asBits)
        val lineIdx = phys(11 downto 4).asUInt
        val tag = phys(31 downto 6)
        val isHit = validBits.readSync(lineIdx) && tagMem.readSync(lineIdx) === tag
        val ramAddr = addr(11 downto 2).asUInt

        when(isRamMode || isHit) {
          ram.io.buses(0).cmd.opcode := 1 // Write
          ram.io.buses(0).cmd.address := ramAddr @@ U"00"
          ram.io.buses(0).cmd.data := data(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (ramAddr + 1) @@ U"00"
            ram.io.buses(0).cmd.data := data(63 downto 32)
          }
          tagMem.write(lineIdx, tag)
          validBits.write(lineIdx, True)
          dirtyBits.write(lineIdx, True)
          if (!isRamMode) {
            pmiDownSizer.io.input.cmd.opcode := 1 // Write
            pmiDownSizer.io.input.cmd.address := phys.asUInt
            pmiDownSizer.io.input.cmd.data := data(63 downto 0)
            pmiDownSizer.io.input.cmd.mask := B"1111"
            host[WorkspaceCacheSrv].write(phys.asUInt, data(31 downto 0))
          }
        } elsewhen (!isRamMode) {
          fetch.haltIt()
          when(dirtyBits.readSync(emptyLine)) {
            pmiDownSizer.io.input.cmd.opcode := 1 // Write
            pmiDownSizer.io.input.cmd.address := tagMem.readSync(emptyLine) @@ emptyLine @@ U"00"
            pmiDownSizer.io.input.cmd.data := ram.io.buses(0).rsp.data ## ram.io.buses(0).rsp.data
            pmiDownSizer.io.input.cmd.mask := B"1111"
          }
          ram.io.buses(0).cmd.opcode := 1 // Write
          ram.io.buses(0).cmd.address := emptyLine @@ U"00"
          ram.io.buses(0).cmd.data := data(31 downto 0)
          when(ram.io.buses(0).rsp.valid) {
            ram.io.buses(0).cmd.address := (emptyLine @@ U"01") @@ U"00"
            ram.io.buses(0).cmd.data := data(63 downto 32)
          }
          tagMem.write(emptyLine, tag)
          validBits.write(emptyLine, True)
          dirtyBits.write(emptyLine, True)
          emptyLine := emptyLine + 1
        }
      }
    }

    // Connect arbiter inputs
    arbiter.io.inputs(0).cmd.valid := fetch.isValid
    arbiter.io.inputs(0).cmd.opcode := 0 // Read
    arbiter.io.inputs(0).cmd.address := fetch(Fetch.FETCH_PC)
    arbiter.io.inputs(0).cmd.length := 0
    arbiter.io.inputs(0).cmd.data := 0
    arbiter.io.inputs(0).cmd.mask := B"1111"
    arbiter.io.inputs(0).rsp.ready := True

    arbiter.io.inputs(1).cmd.valid := memory.isValid && srv.writeEnable
    arbiter.io.inputs(1).cmd.opcode := 1 // Write
    arbiter.io.inputs(1).cmd.address := srv.addr
    arbiter.io.inputs(1).cmd.length := 0
    arbiter.io.inputs(1).cmd.data := srv.writeData(31 downto 0)
    arbiter.io.inputs(1).cmd.mask := B"1111"
    arbiter.io.inputs(1).rsp.ready := True

    // Cache banks
    val banks = for (i <- 0 until 4) yield CacheBank(i)

    val cacheMode =
      Reg(Bits(2 bits)) init 0 // 00: Full RAM, 01: Full Cache, 10: Half RAM/Half Cache
    val isRamMode = cacheMode === 0 || (cacheMode === 2 && fetch(Fetch.FETCH_PC)(13) === 0)

    val bankSel = fetch(Fetch.FETCH_PC)(5 downto 4).asUInt
    val bankAddr = fetch(Fetch.FETCH_PC)

    val bankHits = Vec(Bool(), 4)
    val bankDataOuts = Vec(Bits(128 bits), 4)
    for ((bank, i) <- banks.zipWithIndex) {
      when(bankSel === i) {
        val (hit, dataOut) = bank.read(bankAddr.asUInt, 0) // Primary read port
        bankHits(i) := hit
        bankDataOuts(i) := dataOut
        when(srv.writeEnable && srv.addr(5 downto 4).asUInt === i) {
          bank.write(srv.addr.asUInt, srv.writeData)
        }
      } otherwise {
        bankHits(i) := False
        bankDataOuts(i) := 0
      }
    }

    // CacheAccessSrv integration
    cacheIf.req.ready := True
    cacheIf.rsp.valid := False
    cacheIf.rsp.payload.data := 0
    cacheIf.rsp.payload.valid := False
    when(cacheIf.req.valid) {
      val reqAddr = cacheIf.req.payload.address
      val isWrite = cacheIf.req.payload.isWrite
      val reqBank = reqAddr(5 downto 4).asUInt
      when(isWrite) {
        srv.writeEnable := True
        srv.addr := reqAddr.asBits
        srv.writeData := cacheIf.req.payload.data ## cacheIf.req.payload.data ## cacheIf.req.payload.data ## cacheIf.req.payload.data
        banks(reqBank).write(reqAddr, srv.writeData)
        cacheIf.rsp.valid := True
        cacheIf.rsp.payload.valid := True
      } otherwise {
        srv.addr := reqAddr.asBits
        val (hit, dataOut) = banks(reqBank).read(reqAddr, 0)
        when(hit) {
          cacheIf.rsp.valid := True
          cacheIf.rsp.payload.data := dataOut(31 downto 0)
          cacheIf.rsp.payload.valid := True
        } otherwise {
          fetch.haltIt()
          val mainCacheData = pmiDownSizer.io.input.rsp.data
          banks(reqBank).write(
            reqAddr,
            mainCacheData ## mainCacheData ## mainCacheData ## mainCacheData
          )
          cacheIf.rsp.valid := True
          cacheIf.rsp.payload.data := mainCacheData(31 downto 0)
          cacheIf.rsp.payload.valid := True
        }
      }
    }

    // Pipeline integration
    when(fetch.isValid) {
      memory(CACHE_ADDR) := fetch(Fetch.FETCH_PC)
      memory(CACHE_DATA) := bankDataOuts(bankSel)
      srv.isHit := bankHits(bankSel)
    }

    // Debugging
    when(srv.isHit) {
      report(L"MAIN_CACHE addr=$CACHE_ADDR hit=$isHit bank=$bankSel mode=$cacheMode")
    }

    println(s"[${this.getDisplayName()}] build end")
  }
}

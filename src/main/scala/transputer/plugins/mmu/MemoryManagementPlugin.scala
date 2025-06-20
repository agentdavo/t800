package transputer.plugins.mmu

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import transputer.plugins.pmi.PmiPlugin
import transputer.plugins.cache.{MainCachePlugin, WorkspaceCachePlugin, CacheAccessService}
import transputer.plugins.schedule.SchedulerPlugin
import transputer.plugins.{
  TrapHandlerService,
  ConfigAccessService,
  AddressTranslationService,
  Fetch
}
import transputer.plugins.registers.RegfileService
import transputer.plugins.pipeline.PipelineStageService
import transputer.plugins.registers.RegName
import transputer.Global
import transputer.Transputer

class MemoryManagementPlugin extends FiberPlugin {
  val version = "MemoryManagementPlugin v1.5"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  object DBKeys {
    val TRAP_ADDR = Database.blocking[Bits]()
    val TRAP_TYPE = Database.blocking[Bits]()
    val P_PROCESS_MODE = Database.blocking[Bool]()
    val REGION_BASE = Database.blocking[Vec[UInt]]()
    val REGION_SIZE = Database.blocking[Vec[UInt]]()
    val REGION_PERMS = Database.blocking[Vec[Bits]]()
  }

  lazy val TRAP_ADDR = Payload(Bits(Global.ADDR_BITS bits))
  lazy val TRAP_TYPE = Payload(Bits(4 bits))
  lazy val P_PROCESS_MODE = Payload(Bool())
  lazy val REGION_BASE = Payload(Vec(UInt(Global.ADDR_BITS bits), 4))
  lazy val REGION_SIZE = Payload(Vec(UInt(Global.ADDR_BITS bits), 4))
  lazy val REGION_PERMS = Payload(Vec(Bits(3 bits), 4)) // Exec, Read, Write

  lazy val srv = during setup new Area {
    val service = new TrapHandlerService {
      val trapAddr = Reg(Bits(Global.ADDR_BITS bits)) init 0
      val trapType = Reg(Bits(4 bits)) init 0
      val trapEnable = Reg(Bool()) init False
      val trapHandlerAddr = Reg(Bits(Global.ADDR_BITS bits)) init 0
    }
    addService(service)
    addService(new ConfigAccessService {
      val addr = Reg(Bits(Global.ADDR_BITS bits)) init 0
      val data = Reg(Bits(Global.WORD_BITS bits)) init 0
      val writeEnable = Reg(Bool()) init False
      val isValid = Reg(Bool()) init False
      def read(addr: Bits, width: Int): Bits = {
        this.addr := addr
        isValid := True
        data.resized(width)
      }
      def write(addr: Bits, data: Bits, width: Int): Unit = {
        this.addr := addr
        this.data := data.resized(width)
        writeEnable := True
        isValid := True
      }
    })
    service
  }

  buildBefore(
    retains(
      host[MainCachePlugin].lock,
      host[WorkspaceCachePlugin].lock,
      host[PmiPlugin].lock,
      host[SchedulerPlugin].lock,
      host[SystemControlPlugin].lock
    ).lock
  )

  lazy val logic = during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    val pipe = host[PipelineStageService]
    val regfile = host[RegfileService]
    val fetch = pipe.ctrl(0) // Fetch stage
    val decode = pipe.ctrl(2) // Decode stage
    val execute = pipe.ctrl(3) // Execute stage
    val memory = pipe.ctrl(4) // Memory stage
    val writeback = pipe.ctrl(5) // Writeback stage
    val cacheIf = host[CacheAccessService]

    // P-process mode
    val pProcessModeReg = Reg(Bool()) init False
    P_PROCESS_MODE := pProcessModeReg
    val contextSwitch = host[SchedulerPlugin].contextSwitch // Stubbed
    when(contextSwitch) { pProcessModeReg := False }

    // Region configuration
    val regionConfig = Reg(Vec(Bits(Global.ADDR_BITS bits), 4)) init 0
    val configService = host[ConfigAccessService]
    when(configService.writeEnable && configService.isValid) {
      switch(configService.addr) {
        is(Global.ConfigAddr.CACHE) {
          regionConfig(0) := configService.data.resized(Global.ADDR_BITS)
        }
        is(Global.ConfigAddr.PMI_STROBE) {
          regionConfig(1) := configService
            .read(Global.ConfigAddr.RAS_STROBE0, Global.WORD_BITS)
            .resized(Global.ADDR_BITS)
        }
      }
    }
    REGION_BASE := Vec(regionConfig.map(_.asUInt))
    REGION_SIZE := Vec.fill(4)(U(0x40000000L, Global.ADDR_BITS bits))
    REGION_PERMS := Vec(
      B"101", // Exec, Read, !Write (Code)
      B"011", // !Exec, Read, Write (Data)
      B"011", // !Exec, Read, Write (Stack)
      B"000" // !Exec, !Read, !Write (Reserved)
    )

    // Address translation
    def translateAddress(logicalAddr: Bits): Bits = {
      val regionIdx =
        OHToUInt(for (i <- 0 until 4) yield logicalAddr(31 downto 30) === B(i, 2 bits))
      val regionBase = REGION_BASE(regionIdx)
      val offset = logicalAddr(29 downto 0).asBits.resized(Global.ADDR_BITS)
      (regionBase ## offset).resized(Global.ADDR_BITS)
    }

    // Memory access validation
    val memAccessAddr = Mux(
      fetch.isValid,
      fetch(Fetch.FETCH_PC),
      decode.isValid,
      decode(CACHE_ADDR), // Use CACHE_ADDR for workspace access
      memory.isValid,
      memory(CACHE_ADDR),
      writeback.isValid,
      writeback(CACHE_ADDR),
      U(0, Global.ADDR_BITS bits)
    )
    val memAccessType = Mux(
      fetch.isValid,
      B"001", // Execute
      decode.isValid && cacheIf.req.payload.isWrite,
      B"010", // Write
      decode.isValid,
      B"100", // Read
      B"000"
    )
    cacheIf.req.valid := decode.isValid || fetch.isValid || memory.isValid
    cacheIf.req.payload.address := memAccessAddr.asUInt
    cacheIf.req.payload.data := cacheIf.rsp.payload.data
    cacheIf.req.payload.isWrite := memAccessType(1)
    val regionIdx = OHToUInt(
      for (i <- 0 until 4)
        yield REGION_BASE(i) <= memAccessAddr.asUInt && memAccessAddr.asUInt < (REGION_BASE(
          i
        ) + REGION_SIZE(i))
    )
    val isValidAccess = pProcessModeReg && MuxLookup(
      regionIdx,
      False,
      (0 until 4).map(i =>
        i -> (memAccessType(2) && REGION_PERMS(i)(2) || memAccessType(1) && REGION_PERMS(i)(
          1
        ) || memAccessType(0) && REGION_PERMS(i)(0))
      )
    )

    // Stack extension validation
    val wptr = regfile.read(RegName.WdescReg, 0).asUInt
    val wptrValid = wptr >= REGION_BASE(2) && wptr < (REGION_BASE(2) + REGION_SIZE(2))
    when(decode.isValid && !wptrValid) {
      srv.setTrap(wptr.asBits, B"0010") // Stack extension trap
      decode.haltIt()
    }

    // Privileged instruction detection
    val privilegedInstr = execute.isValid && (execute(Global.OPCODE) === B"1111" || execute(
      Global.OPCODE
    )(7 downto 4) === B"1010")
    when(pProcessModeReg && privilegedInstr) {
      srv.setTrap(execute(Fetch.FETCH_PC), B"0100") // Privileged instruction trap
      execute.haltIt()
    }

    // Trap handling
    when(!isValidAccess || srv.trapEnable) {
      when(!isValidAccess) { srv.setTrap(memAccessAddr, B"0001") } // Invalid memory access trap
      pipe.haltIt()
      DBKeys.TRAP_ADDR.set(srv.trapAddr)
      DBKeys.TRAP_TYPE.set(srv.trapType)
      when(host[SchedulerPlugin].trapHandlerEnable) { // Stubbed
        fetch(Fetch.FETCH_PC) := srv.trapHandlerAddr.asUInt
        srv.clearTrap()
      }
    }

    addService(new AddressTranslationService {
      def translate(addr: Bits): Bits = translateAddress(addr)
    })

    println(s"[${this.getDisplayName()}] build end")
  }
}

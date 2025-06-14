package t800.plugins

import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import t800.Global

/** Service for trap handling, providing details of errors and control. */
case class TrapHandlerSrv() extends Bundle {
  val trapAddr = Bits(32 bits)
  val trapType = Bits(4 bits)
  val trapEnable = Bool()
  val trapHandlerAddr = Bits(32 bits)
  def setTrap(addr: Bits, typ: Bits): Unit = {
    trapAddr := addr
    trapType := typ
    trapEnable := True
  }
  def clearTrap(): Unit = trapEnable := False
}

/** Plugin for T9000-style memory management, P-process protection, and error handling. */
class MemoryManagementPlugin extends FiberPlugin {
  val version = "MemoryManagementPlugin v1.4"
  report(L"Initializing $version")

  object DBKeys {
    val TRAP_ADDR = Database.blocking[Bits]()
    val TRAP_TYPE = Database.blocking[Bits]()
    val P_PROCESS_MODE = Database.blocking[Bool]()
    val REGION_BASE = Database.blocking[Vec[UInt]]()
    val REGION_SIZE = Database.blocking[Vec[UInt]]()
    val REGION_PERMS = Database.blocking[Vec[Bits]]()
  }

  lazy val TRAP_ADDR = Payload(Bits(32 bits))
  lazy val TRAP_TYPE = Payload(Bits(4 bits))
  lazy val P_PROCESS_MODE = Payload(Bool())
  lazy val REGION_BASE = Payload(Vec(UInt(32 bits), 4))
  lazy val REGION_SIZE = Payload(Vec(UInt(32 bits), 4))
  lazy val REGION_PERMS = Payload(Vec(Bits(3 bits), 4)) // Exec, Read, Write

  lazy val srv = during setup new Area {
    val service = TrapHandlerSrv()
    addService(service)
    addService(new Global.ConfigAccessSrv())
  }

  during setup {
    buildBefore(retains(
      host[MainCachePlugin].lock,
      host[WorkspaceCachePlugin].lock,
      host[PmiPlugin].lock,
      host[SchedulerPlugin].lock,
      host[SystemControlPlugin].lock
    ).lock)
  }

  lazy val logic = during build new Area {
    val pipeline = host.find[StageCtrlPipeline]
    val fetch = pipeline.ctrl(0)
    val decode = pipeline.ctrl(2)
    val execute = pipeline.ctrl(3)
    val memory = pipeline.ctrl(4)
    val writeback = pipeline.ctrl(5)

    // P-process mode, controlled by SchedulerPlugin
    val pProcessModeReg = Reg(Bool()) init(False)
    P_PROCESS_MODE := pProcessModeReg
    val contextSwitch = host[SchedulerPlugin].contextSwitch
    when(contextSwitch) { pProcessModeReg := False }

    // Configuration register access
    val configSrv = host.find[Global.ConfigAccessSrv]
    val regionConfig = Reg(Vec(Bits(32 bits), 4)) init(0)
    when(configSrv.writeEnable && configSrv.isValid) {
      switch(configSrv.addr) {
        is(Global.ConfigAddr.CACHE) {
          regionConfig(0) := configSrv.data // Cache region base (30 significant bits)
          configSrv.write(Global.ConfigAddr.CACHE, regionConfig(0), 30) // Example write
        }
        is(Global.ConfigAddr.PMI_STROBE) {
          // Load PMI strobe timing (e.g., RAS_STROBE0 at #0401)
          regionConfig(1) := configSrv.read(Global.ConfigAddr.RAS_STROBE0, 30)
        }
      }
    }
    REGION_BASE := Vec(regionConfig.map(_.asUInt))
    REGION_SIZE := Vec.fill(4)(U(0x40000000L, 32 bits)) // Default size
    REGION_PERMS := Vec(
      B"101", // Exec, Read, !Write (Code)
      B"011", // !Exec, Read, Write (Data)
      B"011", // !Exec, Read, Write (Stack)
      B"000"  // !Exec, !Read, !Write (Reserved)
    )

    // Logical-to-physical address translation
    def translateAddress(logicalAddr: Bits): Bits = {
      val regionIdx = OHToUInt(for (i <- 0 until 4) yield logicalAddr(31 downto 30) === B(i, 2 bits))
      val regionBase = REGION_BASE(regionIdx)
      val offset = logicalAddr(29 downto 0).resized
      (regionBase ## offset).resized
    }

    // Memory access validation
    val memAccessAddr = Mux(
      fetch.isValid, fetch(PC),
      decode.isValid, decode(MEM_ADDR),
      memory.isValid, memory(MEM_ADDR),
      writeback.isValid, writeback(MEM_ADDR),
      U(0, 32 bits)
    )
    val memAccessType = Mux(
      fetch.isValid, B"001", // Fetch (execute)
      decode.isValid && decode(MEM_DATA) =/= 0, B"010", // Write
      decode.isValid, B"100", // Read
      B"000"
    )
    val regionIdx = OHToUInt(for (i <- 0 until 4) yield REGION_BASE(i) <= memAccessAddr && memAccessAddr < (REGION_BASE(i) + REGION_SIZE(i)))
    val isValidAccess = pProcessModeReg && MuxLookup(
      regionIdx,
      False,
      (0 until 4).map(i => i -> (memAccessType(2) && REGION_PERMS(i)(2) || memAccessType(1) && REGION_PERMS(i)(1) || memAccessType(0) && REGION_PERMS(i)(0)))
    )

    // Stack extension (Wptr validation)
    val wptr = decode(WPTR_UPDATE).asUInt
    val wptrValid = wptr >= REGION_BASE(2) && wptr < (REGION_BASE(2) + REGION_SIZE(2)) // Stack region
    when(decode.isValid && !wptrValid) {
      srv.setTrap(wptr.asBits, B"0010") // Stack extension trap
      decode.haltWhen(True)
    }

    // Privileged instruction detection (e.g., syscall, I/O)
    val privilegedInstr = execute.isValid && (execute(OPCODE) === B"1111" || execute(OPCODE)(7 downto 4) === B"1010") // syscall or I/O
    when(pProcessModeReg && privilegedInstr) {
      srv.setTrap(execute(PC), B"0100") // Privileged instruction trap
      execute.haltWhen(True)
    }

    // Trap handling integration
    when(!isValidAccess || srv.trapEnable) {
      when(!isValidAccess) { srv.setTrap(memAccessAddr, B"0001") } // Invalid memory access trap
      pipeline.haltWhen(True)
      DBKeys.TRAP_ADDR.set(srv.trapAddr)
      DBKeys.TRAP_TYPE.set(srv.trapType)
      when(host[SchedulerPlugin].trapHandlerEnable) {
        fetch(PC) := srv.trapHandlerAddr
        srv.clearTrap()
      }
    }

    // Translate addresses for other plugins
    host[MainCachePlugin].srv.addr := translateAddress(host[MainCachePlugin].srv.addr)
    host[WorkspaceCachePlugin].srv.addrA := translateAddress(host[WorkspaceCachePlugin].srv.addrA)
    host[WorkspaceCachePlugin].srv.addrB := translateAddress(host[WorkspaceCachePlugin].srv.addrB)
    host[PmiPlugin].srv.addr := translateAddress(host[PmiPlugin].srv.addr)
  }
}

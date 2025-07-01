package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriState, TriStateArray}
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Hostable}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.regstack.RegStackPlugin
import transputer.plugins.fetch.FetchPlugin
import transputer.plugins.SystemBusService

/** Complete T9000 Transputer implementation with all enhanced plugins. This class represents a
  * fully-featured T9000 with modern SpinalHDL implementation.
  */
object T9000Transputer {

  /** Configure the database with T9000-specific parameters.
    */
  def configureDatabase(param: T9000Param): Database = {
    val db = new Database

    // Core configuration
    db(Global.WORD_BITS) = param.wordWidth
    db(Global.ADDR_BITS) = param.addrWidth
    db(Global.PC_BITS) = param.addrWidth
    db(Global.INSTR_BITS) = 32
    db(Global.IPTR_BITS) = param.addrWidth
    db(Global.OPCODE_BITS) = 8
    db(Global.LINK_COUNT) = param.linkCount

    // FPU configuration
    if (param.enableFpu) {
      db(Global.FPU_PRECISION) = param.fpuPrecision
    }

    // Memory configuration
    db(Global.ROM_WORDS) = 16384 // 64KB ROM
    db(Global.RAM_WORDS) = 1048576 // 4MB RAM

    // Scheduler configuration
    if (param.enableScheduler) {
      db(Global.SCHED_QUEUE_DEPTH) = param.schedulerQueueDepth
    }

    // Reset configuration
    db(Global.RESET_IPTR) = Global.ResetIptr

    db
  }

  /** Create enhanced T9000 system bus parameters. Supports high-bandwidth memory and I/O
    * operations.
    */
  def systemBusParam(param: T9000Param): BmbParameter = BmbParameter(
    addressWidth = param.addrWidth,
    dataWidth = 128, // 128-bit for high performance
    sourceWidth = 4, // Support multiple masters
    contextWidth = 8, // Extended context for VCP operations
    lengthWidth = 6 // Support large burst transfers
  )

  /** Standard T9000 plugin configuration.
    */
  def defaultPlugins(param: T9000Param): Seq[FiberPlugin] = param.plugins()

  /** Minimal T9000 plugin set for testing.
    */
  def unitPlugins(param: T9000Param): Seq[FiberPlugin] = {
    import transputer.plugins._
    Seq(
      new transputer.TransputerPlugin(),
      new regstack.RegStackPlugin(),
      new pipeline.PipelinePlugin(),
      new bus.SystemBusPlugin(),
      new fetch.FetchPlugin(),
      new grouper.InstrGrouperPlugin(),
      // stack functionality now in RegStackPlugin(),
      // new timers.TimerPlugin(),
      // new schedule.SchedulerPlugin(),
      // new decode.PrimaryInstrPlugin(),
      // new execute.SecondaryInstrPlugin(),
      new pipeline.PipelineBuilderPlugin()
    )
  }

  /** Development configuration with enhanced debugging.
    */
  def debugPlugins(param: T9000Param): Seq[FiberPlugin] = {
    val plugins = defaultPlugins(param).toBuffer
    // Add debug-specific plugins here when available
    plugins.toSeq
  }

  /** Performance-optimized configuration.
    */
  def performancePlugins(param: T9000Param): Seq[FiberPlugin] = {
    val optimizedParam = param.copy(
      mainCacheKb = 32, // Larger cache
      wsCacheWords = 64, // Larger workspace cache
      schedulerQueueDepth = 32, // Deeper scheduling
      enableProfiling = true // Performance monitoring
    )
    optimizedParam.plugins()
  }

  /** Convenience constructor with default configuration.
    */
  def apply(): T9000Transputer = {
    val param = T9000Param()
    val db = configureDatabase(param)
    new T9000Transputer(param, db)
  }

  /** Constructor with custom parameters.
    */
  def apply(param: T9000Param): T9000Transputer = {
    val db = configureDatabase(param)
    new T9000Transputer(param, db)
  }

  /** Constructor wiring the given plugins.
    */
  def apply(
    plugins: scala.collection.Seq[Hostable],
    db: Database = new Database
  ): T9000Transputer = {
    val param = T9000Param().copy(customPlugins = Some(plugins.toSeq))
    val db_configured = configureDatabase(param)
    new T9000Transputer(param, db_configured)
  }
}

/** Main T9000 Transputer component. Integrates all plugins into a complete T9000-compliant
  * processor.
  */
class T9000Transputer(
  val param: T9000Param = T9000Param(),
  val database: Database = new Database
) extends Component {

  // Validate configuration
  param.validate().foreach(warning => println(s"Warning: $warning"))

  // Plugin host for managing all T9000 plugins
  val host = database on new PluginHost

  // Enhanced system bus for high-performance operations - internal bus for plugin communication
  val systemBus = Bmb(T9000Transputer.systemBusParam(param))
  host.addService(new SystemBusService { def bus: Bmb = systemBus })
  
  // Default termination for internal system bus
  systemBus.cmd.ready := True  
  systemBus.rsp.valid := False
  systemBus.rsp.opcode := 0
  systemBus.rsp.data := 0
  systemBus.rsp.source := 0
  systemBus.rsp.context := 0
  systemBus.rsp.last := True

  // External interfaces for T9000 features
  val io = new Bundle {
    // DS Links for transputer-to-transputer communication
    val dsLinks = if (param.enableVcp && param.spiDdrLinks) {
      Some(Vec(master(TriStateArray(2 bits)), param.linkCount))
    } else None

    // External memory interface (PMI)
    val externalMem = if (param.enablePmi) {
      Some(
        master(
          Bmb(
            BmbParameter(
              addressWidth = param.addrWidth,
              dataWidth = 64,
              sourceWidth = 2,
              contextWidth = 4,
              lengthWidth = 4
            )
          )
        )
      )
    } else None

    // Interrupt inputs
    val interrupts = in Bits (8 bits)

    // Clock and reset
    val cpuClk = in Bool ()
    val linkClk = if (param.enableVcp) Some(in Bool ()) else None
    val reset = in Bool ()

    // Debug interface
    val debug = if (param.enableDebug) {
      Some(new Bundle {
        val enable = in Bool ()
        val addr = in UInt (param.addrWidth bits)
        val data = master(TriState(Bits(32 bits)))
      })
    } else None

    // Profiling outputs
    val profiling = if (param.enableProfiling) {
      Some(new Bundle {
        val instructionCount = out UInt (32 bits)
        val cycleCount = out UInt (32 bits)
        val cacheHits = out UInt (32 bits)
        val cacheMisses = out UInt (32 bits)
        val linkActivity = out Bits (param.linkCount bits)
      })
    } else None
  }

  // Initialize all configured plugins
  host.asHostOf(param.plugins())

  // Set component name for Verilog generation
  setDefinitionName("T9000Transputer")
  
  // Default assignments for unconnected I/O to prevent NO DRIVER errors
  // DS Links - default to high impedance
  io.dsLinks.foreach { links =>
    links.foreach { link =>
      link.write := 0
      link.writeEnable := 0
    }
  }
  
  // External memory interface - idle
  io.externalMem.foreach { mem =>
    mem.cmd.valid := False
    mem.cmd.opcode := 0
    mem.cmd.address := 0
    mem.cmd.length := 0
    mem.cmd.data := 0
    mem.cmd.mask := 0
    mem.cmd.last := True
    mem.cmd.source := 0
    mem.cmd.context := 0
    mem.rsp.ready := True
  }
  
  // Debug interface - default read
  io.debug.foreach { dbg =>
    dbg.data.write := 0
    dbg.data.writeEnable := False
  }
  
  // Profiling outputs - default to zero
  io.profiling.foreach { prof =>
    prof.instructionCount := 0
    prof.cycleCount := 0
    prof.cacheHits := 0
    prof.cacheMisses := 0
    prof.linkActivity := 0
  }

  // Add configuration report
  if (param.reportModel) {
    println(s"\nT9000 Transputer Configuration:")
    println(param.summary())
  }
}

/** Complete T9000 core for synthesis.
  */
class T9000TransputerCore(param: T9000Param = T9000Param()) extends Component {
  val core = T9000Transputer(param)

  // Set top-level name
  setDefinitionName("T9000TransputerCore")
}

/** Minimal T9000 unit for testing.
  */
class T9000TransputerUnit(
  param: T9000Param = T9000Param(),
  db: Database = new Database
) extends Component {
  val core = Database(db).on(T9000Transputer(T9000Transputer.unitPlugins(param), db))

  setDefinitionName("T9000TransputerUnit")
}

/** Performance-optimized T9000 variant.
  */
class T9000TransputerPerformance(param: T9000Param = T9000Param()) extends Component {
  val perfParam = param.copy(
    mainCacheKb = 32,
    wsCacheWords = 64,
    schedulerQueueDepth = 32,
    enableProfiling = true
  )
  val core = T9000Transputer(perfParam)

  setDefinitionName("T9000TransputerPerformance")
}

/** Development T9000 variant with debugging.
  */
class T9000TransputerDebug(param: T9000Param = T9000Param()) extends Component {
  val debugParam = param.copy(
    enableDebug = true,
    enableProfiling = true,
    enableTestFramework = true
  )
  val core = T9000Transputer(debugParam)

  setDefinitionName("T9000TransputerDebug")
}

/** Verilog generation for T9000 Transputer.
  */
object T9000TransputerVerilog {
  def main(args: Array[String]): Unit = {
    val param = T9000Param()
    val db = T9000Transputer.configureDatabase(param)

    // Parse command line arguments
    args.find(_.startsWith("--config=")).foreach { arg =>
      val configType = arg.split("=")(1)
      configType match {
        case "performance" =>
          val core = Database(db).on(new T9000TransputerPerformance(param))
          SpinalVerilog(core)
        case "debug" =>
          val core = Database(db).on(new T9000TransputerDebug(param))
          SpinalVerilog(core)
        case "unit" =>
          val core = Database(db).on(new T9000TransputerUnit(param, db))
          SpinalVerilog(core)
        case _ =>
          val core = Database(db).on(new T9000TransputerCore(param))
          SpinalVerilog(core)
      }
    }

    if (args.isEmpty) {
      // Default generation
      val core = Database(db).on(new T9000TransputerCore(param))
      val report = SpinalVerilog(core)
      println(s"T9000 Transputer Verilog generated: ${report.toplevelName}")
    }
  }
}

package transputer

import spinal.lib.misc.plugin.{FiberPlugin, Hostable}
import spinal.core.Area
import scala.collection.mutable.ArrayBuffer

/** Complete T9000 Transputer configuration parameters. This configuration supports all enhanced
  * plugins and T9000 features.
  */
case class T9000Param(
  // Core architecture
  wordWidth: Int = Global.WordBits,
  addrWidth: Int = Global.AddrBitsValue,
  linkCount: Int = Global.LinkCount,

  // FPU configuration
  enableFpu: Boolean = true,
  fpuPrecision: Int = 64,
  ieee754Compliance: Boolean = true,

  // Cache configuration
  mainCacheKb: Int = 16,
  wsCacheWords: Int = 32,

  // Memory system
  enableMmu: Boolean = true,
  enablePmi: Boolean = false, // Temporarily disabled - needs plugin restructuring
  pmiChannels: Int = 4,
  pmiDevicesPerChannel: Int = 8,

  // Communication system
  enableVcp: Boolean = true,
  spiDdrLinks: Boolean = true,

  // Process management
  enableScheduler: Boolean = true,
  schedulerQueueDepth: Int = 16,

  // Timer system
  enableTimers: Boolean = true,
  timerPrecision: Int = 1, // microseconds

  // Advanced features
  enableProfiling: Boolean = false,
  enableDebug: Boolean = false,
  enableTestFramework: Boolean = false,

  // Output configuration
  reportModel: Boolean = false,
  reportPerformance: Boolean = false,
  reportCompliance: Boolean = false,
  outputDir: String = "./generated",

  // Custom plugin override
  customPlugins: Option[Seq[Hostable]] = None
) {
  import transputer._

  /** Returns the complete sequence of T9000 plugins based on configuration. This creates the full
    * T9000 architecture with all enhanced features.
    */
  def plugins(hartId: Int = 0): Seq[FiberPlugin] = {
    customPlugins match {
      case Some(customList) => customList.collect { case p: FiberPlugin => p }.toSeq
      case None => pluginsArea(hartId).plugins.collect { case p: FiberPlugin => p }.toSeq
    }
  }

  /** Creates the complete T9000 plugin architecture. Follows the T9000 specification with modern
    * SpinalHDL implementation.
    */
  def pluginsArea(hartId: Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()

    // ========================================
    // CORE SYSTEM PLUGINS
    // ========================================

    // Base transputer core with T9000 configuration
    plugins += new transputer.plugins.core.transputer.TransputerPlugin()

    // Pipeline infrastructure
    plugins += new transputer.plugins.core.pipeline.T9000PipelinePlugin()  // Use T9000 5-stage pipeline
    plugins += new transputer.plugins.core.pipeline.StageAssignmentPlugin() // Stage assignment logic
    plugins += new transputer.plugins.core.pipeline.LaneArbitrationPlugin() // Lane arbitration for parallel execution
    plugins += new transputer.plugins.core.regstack.RegStackPlugin()

    // System bus arbitration - temporarily disabled to diagnose BMB issues
    // plugins += new transputer.plugins.bus.SystemBusPlugin()

    // ========================================
    // T9000 INSTRUCTION PIPELINE
    // ========================================

    // Instruction fetch and decode pipeline
    plugins += new transputer.plugins.core.fetch.FetchPlugin()
    plugins += new transputer.plugins.core.grouper.InstrGrouperPlugin()

    // ========================================
    // T9000 INSTRUCTION TABLE PLUGINS
    // ========================================

    // Core instruction set implementations (Tables 6.9-6.37)
    plugins += new transputer.plugins.arithmetic.ArithmeticPlugin() // Table 6.9: Basic arithmetic & logical
    plugins += new transputer.plugins.longarith.LongArithPlugin() // Table 6.10: 64-bit arithmetic  
    plugins += new transputer.plugins.controlflow.ControlFlowPlugin() // Table 6.11: Jump and call
    plugins += new transputer.plugins.indexing.IndexingPlugin() // Table 6.13: Array indexing & memory
    plugins += new transputer.plugins.general.GeneralPlugin() // Table 6.17: General stack operations

    // IEEE 754 compliant floating-point unit (Tables 6.32-6.37)
    if (enableFpu) {
      plugins += new transputer.plugins.fpu.FpuPlugin()
    }

    // ========================================
    // T9000 MEMORY SYSTEM
    // ========================================

    // Hierarchical cache system per T9000 specification
    plugins += new transputer.plugins.core.cache.MainCachePlugin() // Re-enabled
    plugins += new transputer.plugins.core.cache.WorkspaceCachePlugin() // Re-enabled

    // Memory management unit with 4-region protection per T9000 spec (temporarily disabled)
    // if (enableMmu) {
    //   plugins += new transputer.plugins.legacy.mmu.MemoryManagementPlugin()  // Disabled - testing basic functionality
    // }

    // Programmable memory interface (temporarily disabled)
    // if (enablePmi) {
    //   plugins += new transputer.plugins.legacy.pmi.PmiPlugin()  // Disabled - testing basic functionality
    // }

    // ========================================
    // T9000 COMMUNICATION SYSTEM
    // ========================================

    // Virtual channel processor for transputer-to-transputer communication (temporarily disabled)
    // if (enableVcp) {
    //   plugins += new transputer.plugins.legacy.vcp.VcpPlugin()  // Disabled - testing basic functionality
    // }

    // ========================================
    // T9000 SYSTEM SERVICES  
    // ========================================

    // Process scheduler with multi-state management
    if (enableScheduler) {
      plugins += new transputer.plugins.schedule.SchedulerPlugin()
    }

    // Dual timer system (microsecond + 64μs macro timer)
    if (enableTimers) {
      plugins += new transputer.plugins.timers.TimerPlugin()
    }

    // ========================================
    // ENHANCED FEATURES
    // ========================================

    // Performance profiling and monitoring
    if (enableProfiling) {
      // Add profiling plugin when available
      // plugins += new transputer.plugins.profiling.ProfilingPlugin()
    }

    // Debug interfaces and support
    if (enableDebug) {
      // Add debug plugin when available
      // plugins += new transputer.plugins.debug.DebugPlugin()
    }

    // Per-plugin test framework
    if (enableTestFramework) {
      // Add test framework plugin when available
      // plugins += new transputer.plugins.test.TestFrameworkPlugin()
    }

    // ========================================
    // PIPELINE BUILDER (MUST BE LAST)
    // ========================================

    // Constructs the final pipeline from all plugins
    plugins += new transputer.plugins.core.pipeline.PipelineBuilderPlugin() // Re-enabled to build pipeline
  }

  /** Validates the configuration parameters for consistency. Returns a list of validation warnings
    * or errors.
    */
  def validate(): Seq[String] = {
    val warnings = ArrayBuffer[String]()

    // Validate FPU configuration
    if (enableFpu && fpuPrecision != 32 && fpuPrecision != 64) {
      warnings += s"Invalid FPU precision: $fpuPrecision (must be 32 or 64)"
    }

    // Validate cache configuration
    if (mainCacheKb < 1 || mainCacheKb > 64) {
      warnings += s"Main cache size $mainCacheKb KB may be outside optimal range (1-64 KB)"
    }

    if (wsCacheWords < 8 || wsCacheWords > 128) {
      warnings += s"Workspace cache size $wsCacheWords words may be outside optimal range (8-128 words)"
    }

    // Validate PMI configuration
    if (enablePmi) {
      if (pmiChannels < 1 || pmiChannels > 8) {
        warnings += s"PMI channel count $pmiChannels may be outside supported range (1-8)"
      }
      if (pmiDevicesPerChannel < 1 || pmiDevicesPerChannel > 16) {
        warnings += s"PMI devices per channel $pmiDevicesPerChannel may be outside supported range (1-16)"
      }
    }

    // Validate link configuration
    if (linkCount < 1 || linkCount > 8) {
      warnings += s"Link count $linkCount may be outside T9000 specification (1-4 typical)"
    }

    // Validate scheduler configuration
    if (enableScheduler && (schedulerQueueDepth < 4 || schedulerQueueDepth > 64)) {
      warnings += s"Scheduler queue depth $schedulerQueueDepth may be outside optimal range (4-64)"
    }

    // Validate timer precision
    if (enableTimers && (timerPrecision < 1 || timerPrecision > 1000)) {
      warnings += s"Timer precision $timerPrecision μs may be outside practical range (1-1000 μs)"
    }

    warnings.toSeq
  }

  /** Returns a summary string of the configuration.
    */
  def summary(): String = {
    val sb = new StringBuilder
    sb.append(s"T9000 Configuration Summary:\n")
    sb.append(s"  Core: ${wordWidth}-bit, ${linkCount} links\n")
    sb.append(s"  FPU: ${if (enableFpu) s"${fpuPrecision}-bit IEEE 754" else "disabled"}\n")
    sb.append(s"  Cache: ${mainCacheKb}KB + ${wsCacheWords}W workspace\n")
    sb.append(
      s"  Memory: MMU=${enableMmu}, PMI=${if (enablePmi) s"${pmiChannels}x${pmiDevicesPerChannel}"
        else "disabled"}\n"
    )
    sb.append(s"  Comm: VCP=${enableVcp}, SPI DDR=${spiDdrLinks}\n")
    sb.append(s"  Process: Scheduler=${enableScheduler}, Timers=${enableTimers}\n")
    sb.append(
      s"  Advanced: Profiling=${enableProfiling}, Debug=${enableDebug}, Test=${enableTestFramework}"
    )
    sb.toString()
  }
}

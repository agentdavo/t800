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
  enablePmi: Boolean = false, // Temporarily disabled due to hierarchy violation
  pmiMemorySize: Long = 128 * 1024 * 1024, // 128MB external memory
  pmiTimingPreset: String = "sram", // "sram", "dram", "flash"
  pmiEnableDma: Boolean = true,
  pmiMaxBurstLength: Int = 32,

  // Communication system
  enableVcp: Boolean = true,
  vcpMaxChannels: Int = 256,
  vcpPhysicalLinks: Int = 4,
  vcpChannelBufferSize: Int = 2048,
  vcpPacketBufferDepth: Int = 32,
  vcpFlowControl: Boolean = true,

  // Clock domain configuration
  enableMultiClock: Boolean = true,
  systemClockMHz: Int = 500,
  memoryClockMHz: Int = 250,
  dsLinkClockMHz: Int = 100,
  timerClockMHz: Int = 1,
  debugClockMHz: Int = 50,
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

  // Boot ROM configuration
  enableBootRom: Boolean = false,
  bootRomHexFile: Option[String] = None,
  bootRomStartAddress: Long = 0x80000000L,
  bootRomSize: Int = 4096,

  // FPGA configuration
  fpgaTarget: Option[String] = None,
  fpgaOutputDir: String = "./fpga",

  // Custom plugin override
  customPlugins: Option[Seq[Hostable]] = None
) {
  import transputer._
  import transputer.plugins.boot.BootRomPlugin

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

    // Clock domain management (must be first)
    if (enableMultiClock) {
      plugins += new T9000ClockServicePlugin()
      // NOTE: For general use, you can use the extracted plugin instead:
      // plugins += new transputer.plugins.clock.ClockServicePlugin()
    }

    // Base transputer core with T9000 configuration
    plugins += new transputer.plugins.core.transputer.TransputerPlugin()

    // Pipeline infrastructure
    plugins += new transputer.plugins.core.pipeline.T9000PipelinePlugin() // Use T9000 5-stage pipeline
    // plugins += new transputer.plugins.core.pipeline.StageAssignmentPlugin() // Temporarily disabled - needs workspace service integration
    // plugins += new transputer.plugins.core.pipeline.LaneArbitrationPlugin() // Temporarily disabled - needs SpinalHDL Pipeline API fixes
    plugins += new transputer.plugins.core.regstack.RegStackPlugin()

    // System bus arbitration
    plugins += new transputer.plugins.bus.SystemBusPlugin()

    // Boot ROM (if enabled)
    if (enableBootRom) {
      bootRomHexFile match {
        case Some(hexFile) =>
          plugins += BootRomPlugin.withHex(hexFile, bootRomStartAddress, bootRomSize)
        case None =>
          plugins += new BootRomPlugin(bootRomStartAddress, bootRomSize)
      }
    }

    // ========================================
    // T9000 INSTRUCTION PIPELINE
    // ========================================

    // Instruction fetch and decode pipeline
    plugins += new transputer.plugins.core.fetch.FetchPlugin()
    plugins += new transputer.plugins.core.grouper.T9000GrouperOptimized() // Advanced T9000 grouper
    // plugins += new transputer.plugins.core.grouper.InstrGrouperPlugin() // Disabled - service dependency issue

    // ========================================
    // T9000 INSTRUCTION TABLE PLUGINS
    // ========================================

    // Core instruction set implementations (Tables 6.9-6.37)
    plugins += new transputer.plugins.arithmetic.ArithmeticPlugin() // Table 6.9: Basic arithmetic & logical
    plugins += new transputer.plugins.longarith.LongArithPlugin() // Table 6.10: 64-bit arithmetic
    plugins += new transputer.plugins.controlflow.ControlFlowPlugin() // Table 6.11: Jump and call
    plugins += new transputer.plugins.blockmove.BlockMovePlugin() // Table 6.12: Block move operations
    plugins += new transputer.plugins.indexing.IndexingPlugin() // Table 6.13: Array indexing & memory
    plugins += new transputer.plugins.rangecheck.RangeCheckPlugin() // Table 6.14: Range checking & conversion
    plugins += new transputer.plugins.device.DevicePlugin() // Table 6.15: Device access operations
    plugins += new transputer.plugins.general.GeneralPlugin() // Table 6.17: General stack operations
    plugins += new transputer.plugins.io.IOPlugin() // Tables 6.19-6.20: Input/output operations
    plugins += new transputer.plugins.channels.ChannelPlugin() // Table 6.21: Channel management
    plugins += new transputer.plugins.alternative.AlternativePlugin() // Table 6.24: ALT constructs
    plugins += new transputer.plugins.interrupts.InterruptPlugin() // Table 6.27: Interrupt handling
    plugins += new transputer.plugins.protection.MemoryProtectionPlugin() // Table 6.28: Trap handlers & protection
    plugins += new transputer.plugins.bitops.BitOpsPlugin() // Table 6.16: CRC and bit manipulation
    plugins += new transputer.plugins.resources.ResourcePlugin() // Table 6.22: Resource channels
    plugins += new transputer.plugins.semaphore.SemaphorePlugin() // Table 6.23: Semaphore operations
    plugins += new transputer.plugins.system.SystemPlugin() // Tables 6.29-6.30: System configuration

    // IEEE 754 compliant floating-point unit (Tables 6.32-6.37)
    if (enableFpu) {
      plugins += new transputer.plugins.fpu.FpuPlugin() // Updated for new pipeline architecture
    }

    // ========================================
    // T9000 MEMORY SYSTEM
    // ========================================

    // Hierarchical cache system per T9000 specification
    plugins += new transputer.plugins.core.cache.MainCachePlugin() // Table 6.31: Cache operations (fdca, fdcl, ica, icl)
    plugins += new transputer.plugins.core.cache.WorkspaceCachePlugin() // Workspace cache support

    // Memory management unit with 4-region protection per T9000 spec (temporarily disabled)
    // if (enableMmu) {
    //   plugins += new transputer.plugins.legacy.mmu.MemoryManagementPlugin()  // Disabled - testing basic functionality
    // }

    // Programmable memory interface - modern implementation
    if (enablePmi) {
      plugins += new transputer.plugins.pmi.PmiPlugin(
        externalMemorySize = pmiMemorySize,
        timingPreset = pmiTimingPreset,
        enableDma = pmiEnableDma,
        maxBurstLength = pmiMaxBurstLength
      )
    }

    // ========================================
    // T9000 COMMUNICATION SYSTEM
    // ========================================

    // Virtual channel processor - simplified implementation for compilation
    if (enableVcp) {
      plugins += new transputer.plugins.vcp.VcpPluginSimple(
        maxVirtualChannels = vcpMaxChannels,
        physicalLinkCount = vcpPhysicalLinks,
        channelBufferSize = vcpChannelBufferSize,
        packetBufferDepth = vcpPacketBufferDepth,
        enableFlowControl = vcpFlowControl
      )
    }

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
    // BOOT ROM SUPPORT
    // ========================================

    // Boot ROM for initial program loading
    if (enableBootRom) {
      plugins += new BootRomPlugin(
        startAddress = bootRomStartAddress,
        size = bootRomSize,
        hexFile = bootRomHexFile
      )
    }

    // ========================================
    // ENHANCED FEATURES
    // ========================================

    // Clock domain management (optional)
    // Provides multi-clock domain support for advanced configurations
    // plugins += new transputer.plugins.clock.ClocksPlugin()

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
      if (pmiMemorySize < 1024 * 1024 || pmiMemorySize > 4L * 1024 * 1024 * 1024) {
        warnings += s"PMI memory size ${pmiMemorySize / (1024 * 1024)}MB may be outside supported range (1MB-4GB)"
      }
      if (!Set("sram", "dram", "flash").contains(pmiTimingPreset)) {
        warnings += s"PMI timing preset '$pmiTimingPreset' is not supported (use sram, dram, or flash)"
      }
      if (pmiMaxBurstLength < 1 || pmiMaxBurstLength > 256) {
        warnings += s"PMI burst length $pmiMaxBurstLength may be outside supported range (1-256)"
      }
    }

    // Validate VCP configuration
    if (enableVcp) {
      if (vcpMaxChannels < 1 || vcpMaxChannels > 256) {
        warnings += s"VCP max channels $vcpMaxChannels may be outside supported range (1-256)"
      }
      if (vcpPhysicalLinks < 1 || vcpPhysicalLinks > 4) {
        warnings += s"VCP physical links $vcpPhysicalLinks may be outside T9000 specification (1-4)"
      }
      if (vcpChannelBufferSize < 256 || vcpChannelBufferSize > 8192) {
        warnings += s"VCP channel buffer size $vcpChannelBufferSize may be outside optimal range (256-8192 bytes)"
      }
    }

    // Validate clock configuration
    if (enableMultiClock) {
      if (systemClockMHz < 100 || systemClockMHz > 1000) {
        warnings += s"System clock $systemClockMHz MHz may be outside practical range (100-1000 MHz)"
      }
      if (memoryClockMHz > systemClockMHz) {
        warnings += s"Memory clock $memoryClockMHz MHz should not exceed system clock $systemClockMHz MHz"
      }
      if (dsLinkClockMHz > systemClockMHz / 2) {
        warnings += s"DS-Link clock $dsLinkClockMHz MHz may be too fast relative to system clock"
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
      s"  Memory: MMU=${enableMmu}, PMI=${if (enablePmi) s"${pmiMemorySize / (1024 * 1024)}MB ${pmiTimingPreset}"
        else "disabled"}\n"
    )
    sb.append(s"  Comm: VCP=${if (enableVcp) s"${vcpMaxChannels}ch/${vcpPhysicalLinks}links"
      else "disabled"}, SPI DDR=${spiDdrLinks}\n")
    sb.append(
      s"  Clocks: ${if (enableMultiClock) s"System=${systemClockMHz}MHz, Memory=${memoryClockMHz}MHz, DS-Link=${dsLinkClockMHz}MHz"
        else "single domain"}\n"
    )
    sb.append(s"  Process: Scheduler=${enableScheduler}, Timers=${enableTimers}\n")
    sb.append(
      s"  Advanced: Profiling=${enableProfiling}, Debug=${enableDebug}, Test=${enableTestFramework}"
    )
    sb.toString()
  }
}

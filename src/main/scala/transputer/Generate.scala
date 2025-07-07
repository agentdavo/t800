package transputer

import spinal.core._
import spinal.lib.{AnalysisUtils, LatencyAnalysis}
import spinal.lib.misc.plugin.PluginHost
import spinal.lib.bus.bmb.{Bmb, BmbCmd, BmbParameter}
import spinal.lib.blackbox.lattice.ecp5._
import scopt.OParser

/** Complete T9000 Transputer generation utility.
  *
  * This is the main generator for T9000 Transputer configurations. It supports all T9000 features
  * and can be configured for different use cases:
  *   - Full T9000 with all features: default configuration
  *   - Minimal/bare bones: use --minimal flag
  *   - Custom configurations: use command line options
  *
  * Usage: sbt "runMain transputer.Generate" # Full T9000 sbt "runMain transputer.Generate
  * --minimal" # Bare bones sbt "runMain transputer.Generate --enable-fpu true" # With specific
  * options
  */
object Generate {
  def main(args: Array[String]): Unit = {
    // For backward compatibility, support both Param and T9000Param
    var useMinimal = false
    var useT9000 = true

    // Quick scan for --minimal flag
    if (args.contains("--minimal")) {
      useMinimal = true
      useT9000 = false
    }

    if (useT9000) {
      // Use T9000 configuration (default)
      generateT9000(args)
    } else {
      // Use minimal configuration
      generateMinimal(args.filterNot(_ == "--minimal"))
    }
  }

  private def generateT9000(args: Array[String]): Unit = {
    // Check if this is an FPGA generation request
    val fpgaArgs = args.find(_.startsWith("--fpga"))
    if (fpgaArgs.isDefined) {
      generateFPGA(args)
      return
    }

    val builder = OParser.builder[T9000Param]
    val parser = {
      import builder._
      OParser.sequence(
        programName("t9000Generate"),
        head("T9000 Transputer Generator", "2.0"),

        // Core architecture options
        opt[Int]("word-width")
          .action((x, c) => c.copy(wordWidth = x))
          .text("Width of words in bits (default: 32)"),
        opt[Int]("addr-width")
          .action((x, c) => c.copy(addrWidth = x))
          .text("Address width in bits (default: 32)"),
        opt[Int]("link-count")
          .action((x, c) => c.copy(linkCount = x))
          .text("Number of DS link channels (default: 4)"),

        // FPU configuration
        opt[Boolean]("enable-fpu")
          .action((x, c) => c.copy(enableFpu = x))
          .text("Enable floating-point unit (default: true)"),
        opt[Int]("fpu-precision")
          .action((x, c) => c.copy(fpuPrecision = x))
          .text("FPU precision: 32 or 64 bits (default: 64)"),
        opt[Boolean]("ieee754-compliance")
          .action((x, c) => c.copy(ieee754Compliance = x))
          .text("Enable IEEE 754 compliance (default: true)"),

        // Cache configuration
        opt[Int]("main-cache-kb")
          .action((x, c) => c.copy(mainCacheKb = x))
          .text("Main cache size in KB (default: 16)"),
        opt[Int]("workspace-cache-words")
          .action((x, c) => c.copy(wsCacheWords = x))
          .text("Workspace cache size in words (default: 32)"),

        // Memory configuration
        opt[Boolean]("enable-mmu")
          .action((x, c) => c.copy(enableMmu = x))
          .text("Enable memory management unit (default: true)"),
        opt[Boolean]("enable-pmi")
          .action((x, c) => c.copy(enablePmi = x))
          .text("Enable programmable memory interface (default: true)"),
        opt[Long]("pmi-memory-size")
          .action((x, c) => c.copy(pmiMemorySize = x))
          .text("PMI external memory size in bytes (default: 128MB)"),
        opt[String]("pmi-timing-preset")
          .action((x, c) => c.copy(pmiTimingPreset = x))
          .text("PMI timing preset: sram, dram, or flash (default: sram)"),

        // VCP configuration
        opt[Boolean]("enable-vcp")
          .action((x, c) => c.copy(enableVcp = x))
          .text("Enable virtual channel processor (default: true)"),
        opt[Int]("vcp-max-channels")
          .action((x, c) => c.copy(vcpMaxChannels = x))
          .text("Maximum VCP channels (default: 256)"),
        opt[Boolean]("spi-ddr-links")
          .action((x, c) => c.copy(spiDdrLinks = x))
          .text("Use SPI DDR for DS links (default: true)"),

        // Process scheduling
        opt[Boolean]("enable-scheduler")
          .action((x, c) => c.copy(enableScheduler = x))
          .text("Enable process scheduler (default: true)"),
        opt[Int]("scheduler-queue-depth")
          .action((x, c) => c.copy(schedulerQueueDepth = x))
          .text("Scheduler queue depth (default: 16)"),

        // Timer configuration
        opt[Boolean]("enable-timers")
          .action((x, c) => c.copy(enableTimers = x))
          .text("Enable timer system (default: true)"),
        opt[Int]("timer-precision")
          .action((x, c) => c.copy(timerPrecision = x))
          .text("Timer precision in microseconds (default: 1)"),

        // Clock configuration
        opt[Boolean]("enable-multi-clock")
          .action((x, c) => c.copy(enableMultiClock = x))
          .text("Enable multiple clock domains (default: true)"),
        opt[Int]("system-clock-mhz")
          .action((x, c) => c.copy(systemClockMHz = x))
          .text("System clock frequency in MHz (default: 100)"),

        // Advanced features
        opt[Boolean]("enable-profiling")
          .action((x, c) => c.copy(enableProfiling = x))
          .text("Enable performance profiling (default: false)"),
        opt[Boolean]("enable-debug")
          .action((x, c) => c.copy(enableDebug = x))
          .text("Enable debug interfaces (default: false)"),

        // Boot ROM options
        opt[Unit]("enable-boot-rom")
          .action((_, c) => c.copy(enableBootRom = true))
          .text("Enable boot ROM"),
        opt[String]("boot-rom-hex")
          .action((x, c) => c.copy(bootRomHexFile = Some(x), enableBootRom = true))
          .text("Boot ROM hex file to load (implies --enable-boot-rom)"),
        opt[Long]("boot-rom-address")
          .action((x, c) => c.copy(bootRomStartAddress = x))
          .text("Boot ROM start address (default: 0x80000000)"),
        opt[Int]("boot-rom-size")
          .action((x, c) => c.copy(bootRomSize = x))
          .text("Boot ROM size in bytes (default: 4096)"),
        opt[Unit]("boot-inmos")
          .action((_, c) =>
            c.copy(enableBootRom = true, bootRomHexFile = Some("scripts/hex/bootload.hex"))
          )
          .text("Boot with INMOS bootloader"),

        // Test/Simulation options (from GenerateWithTest)
        opt[String]("hex")
          .action((x, c) => c.copy(bootRomHexFile = Some(x), enableBootRom = true))
          .text("Hex file to load (alias for --boot-rom-hex)"),
        opt[Unit]("wave")
          .action((_, c) => c) // Placeholder for simulation
          .text("Enable waveform generation (requires simulation)"),
        opt[Unit]("konata")
          .action((_, c) => c) // Placeholder for simulation
          .text("Enable Konata pipeline trace (requires simulation)"),

        // FPGA options (from T9000_FPGA)
        opt[String]("fpga")
          .action((x, c) => c.copy(fpgaTarget = Some(x)))
          .text("Generate FPGA wrapper for target (ecp5, ice40, xilinx)"),
        opt[String]("fpga-output-dir")
          .action((x, c) => c.copy(fpgaOutputDir = x))
          .text("FPGA output directory (default: ./fpga)"),

        // Output options
        opt[Unit]("report-model")
          .action((_, c) => c.copy(reportModel = true))
          .text("Print architecture report"),
        opt[Unit]("report-performance")
          .action((_, c) => c.copy(reportPerformance = true))
          .text("Print performance characteristics"),
        opt[Unit]("report-compliance")
          .action((_, c) => c.copy(reportCompliance = true))
          .text("Print T9000 compliance report"),
        opt[String]("output-dir")
          .action((x, c) => c.copy(outputDir = x))
          .text("Output directory (default: ./generated)"),

        // Special flags
        opt[Unit]("minimal")
          .action((_, c) => c)
          .text("Generate minimal configuration (use generateMinimal instead)"),
        help("help").text("Display this help message"),
        version("version").text("Show version information")
      )
    }

    val param = OParser.parse(parser, args, T9000Param()) match {
      case Some(p) =>
        // Validate configuration
        val warnings = p.validate()
        if (warnings.nonEmpty) {
          println("Configuration warnings:")
          warnings.foreach(w => println(s"  ⚠️  $w"))
        }
        p
      case _ => {
        println("Invalid arguments. Use --help for usage information.")
        return
      }
    }

    println(param.summary())
    println()

    // Configure globals based on parameters
    T9000Transputer.configureGlobals(param)

    val analysis = new AnalysisUtils

    // Use common configuration
    val spinalConfig = SpinalConfig(
      targetDirectory = param.outputDir,
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    )

    val report = spinalConfig.generateVerilog {
      val plugins = param.plugins()
      val core = PluginHost.on {
        Transputer(plugins)
      }

      // Set appropriate name based on configuration
      val defName = if (param.enableBootRom && param.bootRomHexFile.isDefined) {
        "T9000TransputerWithBoot"
      } else {
        "T9000Transputer"
      }
      core.setDefinitionName(defName)

      // Generate memory initialization files if boot ROM is enabled
      if (param.enableBootRom && param.bootRomHexFile.isDefined) {
        core.afterElaboration {
          generateMemInitFiles(param.bootRomHexFile.get, param.outputDir)
        }
      }

      core
    }

    analysis.report(report)

    if (param.reportModel) {
      printArchitectureReport(param)
    }

    if (param.reportPerformance) {
      printPerformanceReport(param)
    }

    if (param.reportCompliance) {
      printComplianceReport(param)
    }

    println(s"\nVerilog generated: ${report.toplevelName}.v")
    println(s"Output directory: ${param.outputDir}")

    if (param.enableBootRom && param.bootRomHexFile.isDefined) {
      println(s"Boot ROM initialized from: ${param.bootRomHexFile.get}")
      println("Boot ROM initialization files created in: " + param.outputDir)
      println("\nTo simulate with boot ROM:")
      println("1. Ensure memory initialization files are in the simulation directory")
      println("2. The ROM will be initialized at startup")
      println(
        s"3. The processor will start executing from 0x${param.bootRomStartAddress.toHexString}"
      )
    }

    println("\nT9000 Transputer generation completed successfully!")
  }

  /** Generate FPGA wrapper for T9000 Transputer */
  private def generateFPGA(args: Array[String]): Unit = {
    val builder = OParser.builder[FPGAConfig]
    val parser = {
      import builder._
      OParser.sequence(
        programName("t9000FPGAGenerate"),
        head("T9000 FPGA Generator", "1.0"),
        opt[String]("fpga")
          .action((x, c) => c.copy(target = x))
          .text("FPGA target: ecp5, ice40, xilinx (default: ecp5)"),
        opt[String]("hex")
          .action((x, c) => c.copy(bootRomHexFile = Some(x)))
          .text("Hex file to load into boot ROM"),
        opt[String]("fpga-output-dir")
          .action((x, c) => c.copy(outputDir = x))
          .text("FPGA output directory (default: ./fpga)"),
        opt[Unit]("demo")
          .action((_, c) => c.copy(demoMode = true))
          .text("Generate demo logic instead of full T9000"),
        opt[Unit]("enable-boot-rom")
          .action((_, c) => c.copy(enableBootRom = true))
          .text("Include boot ROM in FPGA design"),
        help("help").text("Display this help message")
      )
    }

    val config = OParser.parse(parser, args, FPGAConfig()) match {
      case Some(c) => c
      case _ => {
        println("Invalid FPGA arguments. Use --help for usage information.")
        return
      }
    }

    println(s"Generating T9000 FPGA wrapper for ${config.target.toUpperCase()} target")
    if (config.bootRomHexFile.isDefined) {
      println(s"Boot ROM: ${config.bootRomHexFile.get}")
    }
    println()

    val spinalConfig = SpinalConfig(
      targetDirectory = config.outputDir,
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      ),
      onlyStdLogicVectorAtTopLevelIo = true
    )

    val report = spinalConfig.generateVerilog {
      if (config.demoMode) {
        // Generate simple demo logic
        new T9000_FPGA_Demo()
      } else {
        // Generate FPGA wrapper with T9000 core
        new T9000_FPGA_Wrapper(config)
      }
    }

    // Generate memory initialization files if boot ROM is specified
    if (config.bootRomHexFile.isDefined) {
      generateMemInitFiles(config.bootRomHexFile.get, config.outputDir)
    }

    println(s"\nFPGA Verilog generated: ${report.toplevelName}.v")
    println(s"Output directory: ${config.outputDir}")
    println(s"Target FPGA: ${config.target.toUpperCase()}")

    if (config.bootRomHexFile.isDefined) {
      println(s"Boot ROM initialized from: ${config.bootRomHexFile.get}")
    }

    println("\nNext steps:")
    println("1. cd fpga")
    println("2. make all    # Complete synthesis flow")
    println("3. make burn   # Program FPGA (if connected)")
    println("\nT9000 FPGA generation completed successfully!")
  }

  private def generateMinimal(args: Array[String]): Unit = {
    val builder = OParser.builder[Param]
    val parser = {
      import builder._
      OParser.sequence(
        programName("transputerGenerate"),
        head("Minimal Transputer Generator", "1.0"),
        opt[Int]("word-width")
          .action((x, c) => c.copy(wordWidth = x))
          .text("Width of words in bits"),
        opt[Int]("link-count")
          .action((x, c) => c.copy(linkCount = x))
          .text("Number of link channels"),
        opt[Boolean]("fpu").action((x, c) => c.copy(enableFpu = x)).text("Enable FPU"),
        opt[Int]("fpu-precision")
          .action((x, c) => c.copy(fpuPrecision = x))
          .text("FPU precision in bits (32/64)"),
        opt[Int]("cache-size").action((x, c) => c.copy(cacheSize = x)).text("Cache size in bytes"),
        opt[Unit]("report-model")
          .action((_, c) => c.copy(reportModel = true))
          .text("Print pipeline model"),
        help("help").text("Display this help message")
      )
    }

    val param = OParser.parse(parser, args, Param()) match {
      case Some(p) => p
      case _ => {
        println("Invalid arguments")
        return
      }
    }

    println(s"Generating minimal Transputer with configuration:")
    println(s"  Word Width: ${param.wordWidth} bits")
    println(s"  Link Count: ${param.linkCount}")
    println(s"  FPU: ${if (param.enableFpu) s"${param.fpuPrecision}-bit" else "disabled"}")

    val db = Transputer.defaultDatabase()
    val analysis = new AnalysisUtils

    // Use common configuration for minimal build
    val spinalConfig = SpinalConfig(
      targetDirectory = "./generated",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    )

    val report = spinalConfig.generateVerilog {
      val plugins = param.plugins()
      val core = PluginHost.on {
        Transputer(plugins)
      }
      core.setDefinitionName("Transputer")
      core
    }

    analysis.report(report)

    if (param.reportModel) {
      println("\n=== Minimal Pipeline Model ===")
      println("Pipeline Stages: Fetch -> Decode -> Execute -> Memory -> Writeback")
      println("Basic instruction timings provided.")
    }

    println(s"\nVerilog generated: ${report.toplevelName}.v")
    println("Minimal Transputer generation completed!")
  }

  private def printArchitectureReport(param: T9000Param): Unit = {
    println("\n" + "=" * 60)
    println("T9000 TRANSPUTER ARCHITECTURE REPORT")
    println("=" * 60)
    println("\n🏗️  Core Architecture:")
    println(s"  • ${param.wordWidth}-bit RISC architecture with evaluation stack")
    println(s"  • ${param.addrWidth}-bit virtual address space")
    println(s"  • ${param.linkCount} DS links for communication")
    println("  • 3-register evaluation stack (Areg, Breg, Creg)")
    println("  • 5-stage pipeline: Fetch → Decode → Execute → Memory → Writeback")

    println("\n🧮  Floating-Point Unit:")
    if (param.enableFpu) {
      println(s"  • ${param.fpuPrecision}-bit IEEE 754 compliant FPU")
      println("  • All 4 rounding modes supported")
      println("  • 5 exception flags")
      println("  • Special value handling (NaN, Infinity)")
    } else {
      println("  • FPU disabled")
    }

    println("\n🧠  Memory System:")
    println(s"  • ${param.mainCacheKb}KB main cache (4-bank)")
    println(s"  • ${param.wsCacheWords}-word workspace cache")
    if (param.enableMmu) {
      println("  • MMU with 4 regions")
    }
    if (param.enablePmi) {
      println(s"  • PMI: ${param.pmiMemorySize / (1024 * 1024)}MB ${param.pmiTimingPreset}")
    }

    println("\n🔗  Communication:")
    if (param.enableVcp) {
      println(s"  • VCP: ${param.vcpMaxChannels} channels")
      if (param.spiDdrLinks) {
        println("  • SPI DDR DS links")
      }
    }

    println("\n⏱️  Process Management:")
    if (param.enableScheduler) {
      println(s"  • Scheduler: ${param.schedulerQueueDepth}-entry queue")
    }
    if (param.enableTimers) {
      println(s"  • Timers: ${param.timerPrecision}μs precision")
    }
  }

  private def printPerformanceReport(param: T9000Param): Unit = {
    println("\n" + "=" * 60)
    println("T9000 PERFORMANCE CHARACTERISTICS")
    println("=" * 60)

    if (param.enableMultiClock) {
      println(s"\n⚡ Clock Domains:")
      println(s"  • System: ${param.systemClockMHz} MHz")
      println(s"  • Memory: ${param.memoryClockMHz} MHz")
      println(s"  • DS-Link: ${param.dsLinkClockMHz} MHz")
    }

    println("\n📊 Pipeline Performance:")
    println("  • 1 instruction/cycle throughput")
    println("  • Branch prediction reduces stalls")

    if (param.enableFpu) {
      println("\n🧮 FPU Performance:")
      println("  • Add/Sub: 2-3 cycles")
      println("  • Multiply: 3-4 cycles")
      println("  • Divide: 15 cycles")
    }
  }

  private def printComplianceReport(param: T9000Param): Unit = {
    println("\n" + "=" * 60)
    println("T9000 SPECIFICATION COMPLIANCE")
    println("=" * 60)
    println("\n✅ Instruction Set:")
    println("  • All 21 instruction tables implemented")
    println("  • 200+ opcodes supported")

    println("\n✅ Architecture Features:")
    println("  • 3-register stack: ✅")
    println("  • Process scheduling: " + (if (param.enableScheduler) "✅" else "❌"))
    println("  • Virtual channels: " + (if (param.enableVcp) "✅" else "❌"))
    println("  • Memory protection: " + (if (param.enableMmu) "✅" else "❌"))
    println("  • Timer system: " + (if (param.enableTimers) "✅" else "❌"))
  }

  /** Generate memory initialization files from hex file (from GenerateWithBootRom) */
  private def generateMemInitFiles(hexFile: String, outputDir: String): Unit = {
    import scala.io.Source
    import java.io.{PrintWriter, FileOutputStream}

    try {
      val source = Source.fromFile(hexFile)
      val lines = source.getLines().toList
      source.close()

      // Parse Intel HEX to get memory content
      val memory = Array.fill[Byte](4096)(0)
      var baseAddress = 0L

      for (line <- lines if line.startsWith(":")) {
        val recordLength = Integer.parseInt(line.substring(1, 3), 16)
        val address = Integer.parseInt(line.substring(3, 7), 16)
        val recordType = Integer.parseInt(line.substring(7, 9), 16)

        recordType match {
          case 0 => // Data record
            for (i <- 0 until recordLength) {
              val data = Integer.parseInt(line.substring(9 + i * 2, 11 + i * 2), 16).toByte
              val targetAddr = (baseAddress + address + i - 0x80000000L).toInt
              if (targetAddr >= 0 && targetAddr < 4096) {
                memory(targetAddr) = data
              }
            }

          case 4 => // Extended linear address
            val highAddr = Integer.parseInt(line.substring(9, 13), 16)
            baseAddress = (highAddr.toLong << 16)

          case _ => // Ignore other types
        }
      }

      // Write Verilog $readmemh format
      val hexWriter = new PrintWriter(s"$outputDir/boot_rom.hex")
      for (i <- 0 until 1024) { // 1024 32-bit words
        val word = (memory(i * 4) & 0xff) |
          ((memory(i * 4 + 1) & 0xff) << 8) |
          ((memory(i * 4 + 2) & 0xff) << 16) |
          ((memory(i * 4 + 3) & 0xff) << 24)
        hexWriter.println(f"$word%08x")
      }
      hexWriter.close()

      // Write binary format for some simulators
      val binWriter = new FileOutputStream(s"$outputDir/boot_rom.bin")
      binWriter.write(memory)
      binWriter.close()

      println(s"Created memory initialization files:")
      println(s"  - $outputDir/boot_rom.hex (Verilog hex format)")
      println(s"  - $outputDir/boot_rom.bin (Binary format)")

    } catch {
      case e: Exception =>
        println(s"Error creating memory initialization files: ${e.getMessage}")
    }
  }
}

/** FPGA Configuration */
case class FPGAConfig(
  target: String = "ecp5",
  outputDir: String = "./fpga",
  bootRomHexFile: Option[String] = None,
  enableBootRom: Boolean = false,
  demoMode: Boolean = false
)

/** T9000 FPGA Demo Wrapper - Simple demo logic for FPGA testing */
class T9000_FPGA_Demo extends Component {
  setDefinitionName("T9000_FPGA")

  val io = new Bundle {
    // Clock and reset
    val clk = in Bool ()
    val rst = in Bool ()

    // Status LEDs (8 LEDs)
    val led = out Bits (8 bits)

    // UART interface
    val uart_tx = out Bool ()
    val uart_rx = in Bool ()

    // Link 0 interface for debugging
    val link0_out = out Bool ()
    val link0_in = in Bool ()

    // Status signals
    val running = out Bool ()
    val error = out Bool ()
  }

  // For demo, use direct clock without PLL
  // In a real design, instantiate ECP5 PLL here
  val systemClockDomain = ClockDomain(
    clock = io.clk,
    reset = io.rst,
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = SYNC,
      resetActiveLevel = HIGH
    )
  )

  // Simple demonstration logic in system clock domain
  val demoArea = systemClockDomain {
    new Area {
      // Simple counter for demonstration
      val counter = Reg(UInt(32 bits)) init 0
      counter := counter + 1

      // Status LED mapping
      val statusCounter = Reg(UInt(24 bits)) init 0
      statusCounter := statusCounter + 1

      // LED[0] - Heartbeat (shows clock is running)
      io.led(0) := statusCounter.msb

      // LED[1-7] - Counter bits
      io.led(7 downto 1) := counter(31 downto 25).asBits

      // Status signals
      io.running := True
      io.error := False

      // UART loopback for testing
      io.uart_tx := !io.uart_rx

      // Link loopback
      io.link0_out := !io.link0_in
    }
  }
}

/** T9000 FPGA Wrapper with full T9000 core */
class T9000_FPGA_Wrapper(config: FPGAConfig) extends Component {
  setDefinitionName("T9000_FPGA")

  val io = new Bundle {
    // Clock and reset
    val clk = in Bool ()
    val rst = in Bool ()

    // Status LEDs (8 LEDs)
    val led = out Bits (8 bits)

    // UART interface
    val uart_tx = out Bool ()
    val uart_rx = in Bool ()

    // Link interfaces (4 links for T9000)
    val link_out = out Bits (4 bits)
    val link_in = in Bits (4 bits)

    // Memory interface (for external PMI memory)
    val mem_addr = out UInt (32 bits)
    val mem_data_out = out Bits (32 bits)
    val mem_data_in = in Bits (32 bits)
    val mem_we = out Bool ()
    val mem_oe = out Bool ()
    val mem_ce = out Bool ()

    // Status signals
    val running = out Bool ()
    val error = out Bool ()
    val halt = out Bool ()
  }

  // For full T9000, we'll add PLL as a separate component
  // This keeps the main logic clean and allows easy PLL customization
  val systemClockDomain = ClockDomain(
    clock = io.clk, // For now, use direct clock
    reset = io.rst,
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = SYNC,
      resetActiveLevel = HIGH
    )
  )

  // TODO: Add ECP5 PLL instantiation here:
  // val pll = new EHXPLLL(EHXPLLLConfig(
  //   clkiFreq = 25.0 MHz,
  //   mDiv = 1,
  //   fbDiv = 16,  // 25 * 16 = 400 MHz VCO
  //   opDiv = 4    // 400 / 4 = 100 MHz output
  // ))

  // T9000 core area with actual transputer implementation
  val t9000Area = systemClockDomain {
    new Area {
      // Configure T9000 parameters for FPGA implementation
      val t9000Param = T9000Param(
        // Core architecture optimized for FPGA
        wordWidth = 32,
        addrWidth = 32,
        linkCount = 4,

        // Enable key features for FPGA
        enableFpu = true,
        fpuPrecision = 64,
        ieee754Compliance = true,

        // Cache configuration for FPGA resources
        mainCacheKb = 4, // Smaller cache for FPGA
        wsCacheWords = 16, // Reduced workspace cache

        // Memory system
        enableMmu = false, // Disable MMU for simpler FPGA design
        enablePmi = false, // Disable PMI for now (hierarchy violation)
        pmiMemorySize = 16 * 1024 * 1024, // 16MB external memory
        pmiTimingPreset = "sram",

        // Communication
        enableVcp = true,
        vcpMaxChannels = 64, // Reduced for FPGA

        // Clock domains (single clock for FPGA)
        enableMultiClock = false,
        systemClockMHz = 100,

        // Process management
        enableScheduler = true,
        schedulerQueueDepth = 8, // Smaller queue for FPGA

        // Timers
        enableTimers = true,
        timerPrecision = 10, // 10us precision for FPGA

        // Boot ROM configuration
        enableBootRom = config.enableBootRom,
        bootRomHexFile = config.bootRomHexFile,
        bootRomStartAddress = 0x80000000L,
        bootRomSize = 4096,

        // Debugging and profiling
        enableProfiling = false,
        enableDebug = false
      )

      // Configure T9000 database for FPGA
      T9000Transputer.configureGlobals(t9000Param)

      // Create T9000 transputer core
      val t9000Core = PluginHost.on {
        Transputer(t9000Param.plugins())
      }

      // Map T9000 BMB system bus to external memory interface

      // LED status mapping from T9000 status
      val statusCounter = Reg(UInt(24 bits)) init 0
      statusCounter := statusCounter + 1

      io.led(0) := statusCounter.msb // Heartbeat
      io.led(1) := t9000Core.systemBus.cmd.valid // BMB command active
      io.led(2) := t9000Core.systemBus.rsp.valid // BMB response active
      io.led(
        3
      ) := t9000Core.systemBus.cmd.ready && t9000Core.systemBus.cmd.valid // Transaction active
      io.led(7 downto 4) := t9000Core.systemBus.cmd.payload
        .address(7 downto 4)
        .asBits // Address debug

      // Status signals derived from BMB activity
      io.running := True // T9000 always running (TODO: add proper status from scheduler)
      io.error := False // No error yet (TODO: add proper error handling)
      io.halt := False // Not halted (TODO: add proper halt detection)

      // UART interface - simple loopback for now (TODO: connect to IServer/Console)
      io.uart_tx := !io.uart_rx

      // Link interfaces - loopback for now (TODO: connect to VCP/Link plugins)
      io.link_out := ~io.link_in

      // External memory interface - BMB to simple memory bus conversion
      val bmb = t9000Core.systemBus

      // BMB address to memory address (32-bit)
      io.mem_addr := bmb.cmd.payload.address.resize(32 bits)

      // BMB data to memory data (handle width conversion)
      io.mem_data_out := bmb.cmd.payload.data.resize(32 bits)

      // Memory controls from BMB
      io.mem_we := bmb.cmd.valid && bmb.cmd.payload.isWrite
      io.mem_oe := bmb.cmd.valid && bmb.cmd.payload.isRead
      io.mem_ce := bmb.cmd.valid

      // BMB response from memory
      bmb.cmd.ready := True // Always ready for commands (TODO: add proper flow control)
      bmb.rsp.valid := RegNext(bmb.cmd.valid) init False
      bmb.rsp.payload.data := io.mem_data_in.resized // Memory data to BMB (width conversion)
      bmb.rsp.payload.context := RegNext(bmb.cmd.payload.context)
      bmb.rsp.payload.source := RegNext(bmb.cmd.payload.source)
      bmb.rsp.payload.last := True
    }
  }
}

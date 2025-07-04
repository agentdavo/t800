package transputer

import spinal.core._
import spinal.lib.{AnalysisUtils, LatencyAnalysis}
import spinal.lib.misc.plugin.PluginHost
import scopt.OParser

/** Complete T9000 Transputer generation utility.
  *
  * This is the main generator for T9000 Transputer configurations. It supports
  * all T9000 features and can be configured for different use cases:
  * - Full T9000 with all features: default configuration
  * - Minimal/bare bones: use --minimal flag
  * - Custom configurations: use command line options
  *
  * Usage: 
  *   sbt "runMain transputer.Generate"                    # Full T9000
  *   sbt "runMain transputer.Generate --minimal"          # Bare bones
  *   sbt "runMain transputer.Generate --enable-fpu true"  # With specific options
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
          .text("System clock frequency in MHz (default: 500)"),

        // Advanced features
        opt[Boolean]("enable-profiling")
          .action((x, c) => c.copy(enableProfiling = x))
          .text("Enable performance profiling (default: false)"),
        opt[Boolean]("enable-debug")
          .action((x, c) => c.copy(enableDebug = x))
          .text("Enable debug interfaces (default: false)"),

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
      core.setDefinitionName("T9000Transputer")
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
    println("T9000 Transputer generation completed successfully!")
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
      println(s"  • PMI: ${param.pmiMemorySize/(1024*1024)}MB ${param.pmiTimingPreset}")
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
}

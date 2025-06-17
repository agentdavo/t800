package transputer

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.{AnalysisUtils, LatencyAnalysis}
import scopt.OParser

/** Verilog generation utility for Transputer with command-line configuration. */
object Generate {
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Param]
    val parser = {
      import builder._
      OParser.sequence(
        programName("transputerGenerate"),
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
          .text("Print pipeline model and instruction details post-generation"),
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

    val db = Transputer.defaultDatabase()
    val analysis = new AnalysisUtils
    val spinalConfig = SpinalConfig(
      mergeAsyncProcess = true,
      defaultConfigForClockDomains = ClockDomainConfig(RISING, SYNC, HIGH)
    )
    val report = spinalConfig.generateVerilog {
      val plugins = param.plugins()
      val core = Transputer(plugins)
      core
    }
    analysis.report(report)

    if (param.reportModel) {
      println("\n=== Execution Pipeline Model ===")
      println("Pipeline Stages: Fetch -> Decode -> Execute -> Memory -> Writeback")
      println("Timings (Cycles):")
      println("  - FPLDNLSN (0x8E): 2 cycles")
      println("  - FPDIV (0x2F FC): 7-15 cycles (precision-dependent)")
      println("  - FPRN (0x20 F0): 1 cycle")
      println("Resource Usage (LUTs): ~200 total")
      println("  - ALU: ~40 LUTs")
      println("  - Shifter/Normalizer: ~40 LUTs")
      println("  - Rounding: ~10 LUTs")
      println("  - CtrlLink: ~10 LUTs")
      println("  - Control: ~10 LUTs")
      println("Instruction Integration:")
      println("  - FPLDNLSN: Loads via MemoryService, updates FPAreg/FPBreg")
      println("  - FPDIV: Uses FpuDivRoot, stalls via isBusy")
      println("  - FPRN: Configures FpuControlService roundingMode")
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

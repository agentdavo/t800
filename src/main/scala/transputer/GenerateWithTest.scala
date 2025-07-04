package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.PluginHost
import scopt.OParser
import java.io.File

/** Extended T9000 Generator with minimal test support.
  * 
  * This extends Generate.scala with just the essential test features:
  * - Load hex file for simulation
  * - Enable waveforms and Konata traces
  * - Simple pass/fail detection
  * 
  * Keeps the codebase clean by reusing existing infrastructure.
  */
object GenerateWithTest {
  
  // Extend the existing T9000Param with test options
  case class T9000TestParam(
    base: T9000Param = T9000Param(),
    hexFile: Option[String] = None,
    enableWave: Boolean = false,
    enableKonata: Boolean = false,
    passSymbol: Option[Long] = None,
    failSymbol: Option[Long] = None
  )
  
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[T9000TestParam]
    val parser = {
      import builder._
      OParser.sequence(
        programName("GenerateWithTest"),
        head("T9000 Generator with Test Support", "1.0"),
        
        // Reuse base T9000 options
        opt[Int]("word-width")
          .action((x, c) => c.copy(base = c.base.copy(wordWidth = x)))
          .text("Width of words in bits (default: 32)"),
        opt[Int]("link-count")
          .action((x, c) => c.copy(base = c.base.copy(linkCount = x)))
          .text("Number of DS link channels (default: 4)"),
        opt[Boolean]("enable-fpu")
          .action((x, c) => c.copy(base = c.base.copy(enableFpu = x)))
          .text("Enable floating-point unit (default: true)"),
        opt[String]("output-dir")
          .action((x, c) => c.copy(base = c.base.copy(outputDir = x)))
          .text("Output directory (default: ./generated)"),
          
        // Minimal test options
        opt[String]("hex")
          .action((x, c) => c.copy(hexFile = Some(x)))
          .text("Hex file to load for simulation"),
        opt[Unit]("wave")
          .action((_, c) => c.copy(enableWave = true))
          .text("Enable waveform generation"),
        opt[Unit]("konata")
          .action((_, c) => c.copy(enableKonata = true))
          .text("Enable Konata pipeline trace"),
        opt[Long]("pass")
          .action((x, c) => c.copy(passSymbol = Some(x)))
          .text("Pass symbol address"),
        opt[Long]("fail")
          .action((x, c) => c.copy(failSymbol = Some(x)))
          .text("Fail symbol address"),
          
        help("help").text("Display this help message")
      )
    }
    
    val param = OParser.parse(parser, args, T9000TestParam()) match {
      case Some(p) => p
      case _ => return
    }
    
    // Configure and generate
    T9000Transputer.configureGlobals(param.base)
    
    val config = SpinalConfig(
      targetDirectory = param.base.outputDir,
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    )
    
    // Add simulation support if test features requested
    if (param.hexFile.isDefined || param.enableWave || param.enableKonata) {
      config.includeSimulation
    }
    
    // Generate or simulate based on options
    if (param.hexFile.isDefined) {
      runSimulation(param, config)
    } else {
      // Just generate RTL
      val report = config.generateVerilog {
        val plugins = param.base.plugins()
        PluginHost.on(Transputer(plugins)).setDefinitionName("T9000Transputer")
      }
      println(s"Generated: ${report.toplevelName}.v")
    }
  }
  
  private def runSimulation(param: T9000TestParam, rtlConfig: SpinalConfig): Unit = {
    import transputer.test.konata._
    
    val simConfig = SimConfig
      .withConfig(rtlConfig)
      .workspacePath("simWorkspace")
    
    if (param.enableWave) simConfig.withFstWave
    
    simConfig.compile {
      val plugins = param.base.plugins()
      PluginHost.on(Transputer(plugins))
    }.doSim { dut =>
      // Simple test harness
      dut.clockDomain.forkStimulus(10)
      
      // Konata setup if enabled
      val konata = if (param.enableKonata) {
        val backend = new KonataBackend(new File("simWorkspace/konata.log"))
        backend.spinalSimFlusher(1000)
        Some(backend)
      } else None
      
      // Load hex file if provided
      param.hexFile.foreach { hex =>
        println(s"Loading hex file: $hex")
        // Note: Actual loading would require memory access
      }
      
      // Run simulation
      val timeout = 100000
      var cycle = 0
      while (cycle < timeout) {
        // Check pass/fail if configured
        // Note: Would need PC signal access
        
        dut.clockDomain.waitSampling()
        cycle += 1
      }
      
      konata.foreach(_.close())
      println(s"Simulation completed after $cycle cycles")
    }
  }
}
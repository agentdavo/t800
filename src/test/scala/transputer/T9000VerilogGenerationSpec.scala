package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import java.io.File

/** Test specification for T9000 Verilog Generation Tests that components generate clean Verilog
  * without requiring simulation
  */
class T9000VerilogGenerationSpec extends AnyFunSuite {

  test("T9000RegStack should generate clean Verilog") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false
    )

    T9000Transputer.configureGlobals(param)

    class RegStackTestDut extends Component {
      val core = Transputer(
        Seq(
          new transputer.plugins.core.transputer.TransputerPlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.core.pipeline.PipelinePlugin(),
          new transputer.plugins.bus.SystemBusPlugin()
        )
      )

      val systemBus = core.systemBus
    }

    // Generate Verilog only (no simulation)
    val compiled = SpinalVerilog(new RegStackTestDut)

    // Verify Verilog file was created
    val verilogFile = new File(compiled.rtlSourcesPaths.head)
    assert(verilogFile.exists(), "Verilog file should be generated")
    assert(verilogFile.length() > 1000, "Verilog file should contain substantial content")

    // Verify it contains expected module
    val content = scala.io.Source.fromFile(verilogFile).mkString
    assert(content.contains("module RegStackTestDut"), "Should contain RegStackTestDut module")
    assert(content.contains("Transputer core"), "Should instantiate Transputer core")
  }

  test("T9000Pipeline should generate clean Verilog") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false
    )

    T9000Transputer.configureGlobals(param)

    class PipelineTestDut extends Component {
      val core = Transputer(
        Seq(
          new transputer.plugins.core.transputer.TransputerPlugin(),
          new transputer.plugins.core.pipeline.PipelinePlugin(),
          new transputer.plugins.core.pipeline.PipelineBuilderPlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.core.fetch.FetchPlugin(), // Include FetchPlugin
          new transputer.plugins.bus.SystemBusPlugin()
        )
      )

      val systemBus = core.systemBus
    }

    val compiled = SpinalVerilog(new PipelineTestDut)

    val verilogFile = new File(compiled.rtlSourcesPaths.head)
    assert(verilogFile.exists(), "Verilog file should be generated")
    assert(verilogFile.length() > 2000, "Pipeline Verilog should be substantial")

    val content = scala.io.Source.fromFile(verilogFile).mkString
    assert(content.contains("module PipelineTestDut"), "Should contain PipelineTestDut module")
    assert(content.contains("Transputer core"), "Should instantiate Transputer core")
  }

  test("T9000MainCache should generate clean Verilog") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false,
      mainCacheKb = 16
    )

    T9000Transputer.configureGlobals(param)

    class MainCacheTestDut extends Component {
      val core = Transputer(
        Seq(
          new transputer.plugins.core.transputer.TransputerPlugin(),
          new transputer.plugins.core.cache.MainCachePlugin(),
          new transputer.plugins.core.pipeline.PipelinePlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.bus.SystemBusPlugin()
        )
      )

      val systemBus = core.systemBus
    }

    val compiled = SpinalVerilog(new MainCacheTestDut)

    val verilogFile = new File(compiled.rtlSourcesPaths.head)
    assert(verilogFile.exists(), "Verilog file should be generated")
    assert(verilogFile.length() > 2000, "Cache Verilog should be substantial")

    val content = scala.io.Source.fromFile(verilogFile).mkString
    assert(content.contains("module MainCacheTestDut"), "Should contain MainCacheTestDut module")
  }

  test("T9000Scheduler should generate clean Verilog") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = true, // Enable scheduler
      enableTimers = false,
      enableMmu = false
    )

    T9000Transputer.configureGlobals(param)

    class SchedulerTestDut extends Component {
      val core = Transputer(
        Seq(
          new transputer.plugins.core.transputer.TransputerPlugin(),
          new transputer.plugins.schedule.SchedulerPlugin(),
          new transputer.plugins.core.pipeline.PipelinePlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.bus.SystemBusPlugin()
        )
      )

      val systemBus = core.systemBus
    }

    val compiled = SpinalVerilog(new SchedulerTestDut)

    val verilogFile = new File(compiled.rtlSourcesPaths.head)
    assert(verilogFile.exists(), "Verilog file should be generated")

    val content = scala.io.Source.fromFile(verilogFile).mkString
    assert(content.contains("module SchedulerTestDut"), "Should contain SchedulerTestDut module")
  }

  test("T9000Integration should generate clean Verilog") {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = true,
      enableTimers = true,
      enableMmu = false
    )

    T9000Transputer.configureGlobals(param)

    class IntegrationTestDut extends Component {
      val core = Transputer(param.plugins())
      val systemBus = core.systemBus
    }

    val compiled = SpinalVerilog(new IntegrationTestDut)

    val verilogFile = new File(compiled.rtlSourcesPaths.head)
    assert(verilogFile.exists(), "Verilog file should be generated")
    assert(verilogFile.length() > 5000, "Integration Verilog should be substantial")

    val content = scala.io.Source.fromFile(verilogFile).mkString
    assert(
      content.contains("module IntegrationTestDut"),
      "Should contain IntegrationTestDut module"
    )
    assert(content.contains("Transputer core"), "Should instantiate Transputer core")
  }
}

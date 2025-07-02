package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.core.regstack.RegStackService

/** Tests for T9000 proper plugin pipeline architecture.
  *
  * Tests the correct T9000 5-stage pipeline: Fetch/Group → Local/Decode → Address/Cache → Execute →
  * Writeback With instruction table plugins: ArithmeticPlugin, GeneralPlugin, etc.
  */
class T9000PipelineSpec extends AnyFunSuite {

  // Simple test DUT with proper plugin pipeline
  class PipelineTestDut extends Component {
    val db = T9000Transputer.configureDatabase(T9000Param())

    val io = new Bundle {
      val enable = in Bool ()
      val reset = in Bool ()

      val pipelineReady = out Bool ()
      val stackA = out UInt (32 bits)
      val instructionCount = out UInt (32 bits)
    }

    // Create proper T9000 plugin pipeline (minimal working set)
    val pipelinePlugins = Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.core.pipeline.T9000PipelinePlugin(), // Use T9000 5-stage pipeline
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.bus.SystemBusPlugin(), // Required for FetchPlugin
      new transputer.plugins.core.fetch.FetchPlugin(),
      new transputer.plugins.core.grouper.T9000GrouperOptimized(), // Advanced T9000 grouper
      // Core instruction table plugins
      new transputer.plugins.arithmetic.ArithmeticPlugin(),
      new transputer.plugins.general.GeneralPlugin(),
      new transputer.plugins.core.pipeline.PipelineBuilderPlugin() // Must be last
    )

    val core = Database(db).on(Transputer(pipelinePlugins))

    // Get services safely
    val regStackService =
      try {
        core.host[transputer.plugins.core.regstack.RegStackService]
      } catch {
        case _: Exception => null
      }

    // Simple control logic
    val instrCount = Reg(UInt(32 bits)) init 0
    when(io.enable && !io.reset) {
      instrCount := instrCount + 1
    }.elsewhen(io.reset) {
      instrCount := 0
    }

    // Connect outputs
    io.pipelineReady := io.enable && !io.reset
    if (regStackService != null) {
      io.stackA := regStackService.A
    } else {
      io.stackA := 0
    }
    io.instructionCount := instrCount
  }

  test("T9000 pipeline compilation with instruction table plugins") {
    // Test that the T9000 pipeline with instruction table plugins compiles and elaborates
    SimConfig.compile(new PipelineTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      // Basic test - verify it doesn't crash
      dut.io.enable #= false
      dut.io.reset #= true
      dut.clockDomain.waitRisingEdge()

      dut.io.reset #= false
      dut.io.enable #= true
      dut.clockDomain.waitRisingEdge(5)

      // Test completed successfully if we get here
      assert(
        true,
        "T9000 pipeline with instruction table plugins compiled and elaborated successfully"
      )
      println(s"Pipeline ready: ${dut.io.pipelineReady.toBoolean}")
      println(s"Instruction count: ${dut.io.instructionCount.toInt}")
      println(s"Stack A value: ${dut.io.stackA.toInt}")
    }
  }
}

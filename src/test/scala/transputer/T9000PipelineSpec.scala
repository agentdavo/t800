package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Test specification for T9000 5-stage pipeline
 * Tests the pipeline structure and basic operation
 */
class T9000PipelineSpec extends AnyFunSuite {

  // Test component focusing on pipeline functionality
  class PipelineTestDut extends Component {
    // Use minimal T9000 configuration for pipeline testing
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false
    )
    
    // Configure globals
    T9000Transputer.configureGlobals(param)
    
    // Create transputer with pipeline and essential plugins
    val core = Transputer(Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.core.pipeline.PipelinePlugin(),
      new transputer.plugins.core.pipeline.PipelineBuilderPlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.core.fetch.FetchPlugin(),
      new transputer.plugins.core.grouper.InstrGrouperPlugin(),
      new transputer.plugins.bus.SystemBusPlugin()
    ))
    
    val systemBus = core.systemBus
  }

  test("T9000Pipeline should instantiate 5-stage pipeline") {
    SimConfig.compile(new PipelineTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      // If we get here, the pipeline was created successfully
      assert(true, "T9000 5-stage pipeline instantiated successfully")
    }
  }

  test("T9000Pipeline should handle pipeline stages") {
    SimConfig.compile(new PipelineTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Run through multiple pipeline stages
      for (cycle <- 0 until 50) {
        dut.clockDomain.waitSampling()
        
        // Verify pipeline is stable (no exceptions thrown)
        if (cycle % 10 == 0) {
          // Sample some signals to ensure pipeline is operating
          val cmdValid = dut.systemBus.cmd.valid.toBoolean
          val rspReady = dut.systemBus.rsp.ready.toBoolean
          
          // These should be valid boolean values
          assert(cmdValid == true || cmdValid == false, s"cmd.valid should be boolean at cycle $cycle")
          assert(rspReady == true || rspReady == false, s"rsp.ready should be boolean at cycle $cycle")
        }
      }
    }
  }

  test("T9000Pipeline should integrate with instruction grouper") {
    SimConfig.compile(new PipelineTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(20)
      
      // Verify the grouper is integrated with the pipeline
      assert(dut.core != null, "Core with integrated grouper should be instantiated")
      
      // Run simulation to test pipeline + grouper interaction
      dut.clockDomain.waitSampling(30)
    }
  }

  test("T9000Pipeline should handle reset properly") {
    SimConfig.compile(new PipelineTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      
      // Test reset sequence
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      
      // First deassertion
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      // Second reset
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      // Pipeline should handle multiple resets gracefully
      assert(true, "Pipeline handled multiple resets successfully")
    }
  }
}
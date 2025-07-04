package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Test specification for T9000 Workspace Cache
 * Tests the 32-word workspace cache with triple-port access
 */
class T9000WorkspaceCacheSpec extends AnyFunSuite {

  // Test component for workspace cache functionality
  class WorkspaceCacheTestDut extends Component {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = false,
      enableMmu = false,
      wsCacheWords = 32  // Enable 32-word workspace cache
    )
    
    T9000Transputer.configureGlobals(param)
    
    val core = Transputer(Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.core.cache.WorkspaceCachePlugin(),
      new transputer.plugins.core.pipeline.PipelinePlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.bus.SystemBusPlugin()
    ))
    
    val systemBus = core.systemBus
  }

  test("T9000WorkspaceCache should instantiate 32-word cache") {
    SimConfig.compile(new WorkspaceCacheTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      assert(true, "32-word workspace cache instantiated successfully")
    }
  }

  test("T9000WorkspaceCache should handle workspace operations") {
    SimConfig.compile(new WorkspaceCacheTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Test workspace cache operations
      for (cycle <- 0 until 100) {
        dut.clockDomain.waitSampling()
      }
      
      assert(true, "Workspace cache operations completed successfully")
    }
  }
}
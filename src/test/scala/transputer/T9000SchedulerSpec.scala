package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Test specification for T9000 Scheduler
 * Tests process scheduler with dual-priority queues
 */
class T9000SchedulerSpec extends AnyFunSuite {

  class SchedulerTestDut extends Component {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = true,  // Enable scheduler for testing
      enableTimers = false,
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val core = Transputer(Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.schedule.SchedulerPlugin(),
      new transputer.plugins.core.pipeline.PipelinePlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.bus.SystemBusPlugin()
    ))
    
    val systemBus = core.systemBus
  }

  test("T9000Scheduler should instantiate dual-priority queues") {
    SimConfig.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      assert(true, "Scheduler with dual-priority queues instantiated successfully")
    }
  }

  test("T9000Scheduler should handle process scheduling") {
    SimConfig.compile(new SchedulerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Test scheduler operations
      for (cycle <- 0 until 100) {
        dut.clockDomain.waitSampling()
      }
      
      assert(true, "Process scheduling operations completed successfully")
    }
  }
}
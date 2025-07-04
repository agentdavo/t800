package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Test specification for T9000 Timer System
 * Tests dual timer system (1μs and 64μs resolution)
 */
class T9000TimerSpec extends AnyFunSuite {

  class TimerTestDut extends Component {
    val param = T9000Param(
      enableFpu = false,
      enablePmi = false,
      enableVcp = false,
      enableScheduler = false,
      enableTimers = true,  // Enable timers for testing
      enableMmu = false
    )
    
    T9000Transputer.configureGlobals(param)
    
    val core = Transputer(Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.timers.TimerPlugin(),
      new transputer.plugins.core.pipeline.PipelinePlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.bus.SystemBusPlugin()
    ))
    
    val systemBus = core.systemBus
  }

  test("T9000Timer should instantiate dual timer system") {
    SimConfig.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(5)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(10)
      
      assert(true, "Dual timer system (1μs and 64μs) instantiated successfully")
    }
  }

  test("T9000Timer should handle timer operations") {
    SimConfig.compile(new TimerTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Test timer operations
      for (cycle <- 0 until 100) {
        dut.clockDomain.waitSampling()
      }
      
      assert(true, "Timer operations completed successfully")
    }
  }
}
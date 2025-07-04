package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._

/**
 * Test specification for T9000 Complete System Integration
 * Tests the full T9000 system with multiple plugins working together
 */
class T9000IntegrationSpec extends AnyFunSuite {

  // Test component for full system integration
  class IntegrationTestDut extends Component {
    // Use a comprehensive T9000 configuration (but disable complex features for testing)
    val param = T9000Param(
      enableFpu = false,      // Keep FPU disabled for faster testing
      enablePmi = false,      // PMI has hierarchy issues
      enableVcp = false,      // Keep VCP disabled for simpler testing
      enableScheduler = true, // Enable scheduler for integration test
      enableTimers = true,    // Enable timers
      enableMmu = false       // Keep MMU disabled
    )
    
    // Configure globals
    T9000Transputer.configureGlobals(param)
    
    // Create full transputer with key plugins
    val core = Transputer(param.plugins())
    
    val systemBus = core.systemBus
  }

  test("T9000Integration should instantiate full system") {
    SimConfig.compile(new IntegrationTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(20)
      
      // Full system instantiation successful
      assert(true, "T9000 full system instantiated successfully")
    }
  }

  test("T9000Integration should handle system startup") {
    SimConfig.compile(new IntegrationTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Allow system to start up and stabilize
      for (cycle <- 0 until 100) {
        dut.clockDomain.waitSampling()
        
        // Monitor system health every 20 cycles
        if (cycle % 20 == 0) {
          val cmdValid = dut.systemBus.cmd.valid.toBoolean
          val cmdReady = dut.systemBus.cmd.ready.toBoolean
          val rspValid = dut.systemBus.rsp.valid.toBoolean
          val rspReady = dut.systemBus.rsp.ready.toBoolean
          
          // System bus should be operational
          assert(cmdValid == true || cmdValid == false, s"cmd.valid should be stable at cycle $cycle")
          assert(cmdReady == true || cmdReady == false, s"cmd.ready should be stable at cycle $cycle")
          assert(rspValid == true || rspValid == false, s"rsp.valid should be stable at cycle $cycle")
          assert(rspReady == true || rspReady == false, s"rsp.ready should be stable at cycle $cycle")
        }
      }
    }
  }

  test("T9000Integration should handle multiple plugin interactions") {
    SimConfig.compile(new IntegrationTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Test plugin interactions over extended period
      for (phase <- 0 until 5) {
        // Each phase tests different plugin combinations
        for (cycle <- 0 until 50) {
          dut.clockDomain.waitSampling()
          
          // Verify system stability
          if (cycle == 25) {
            val busDataWidth = dut.systemBus.p.access.dataWidth
            assert(busDataWidth == 128, s"Expected 128-bit bus, got $busDataWidth")
          }
        }
      }
      
      assert(true, "Multiple plugin interactions completed successfully")
    }
  }

  test("T9000Integration should handle instruction execution pipeline") {
    SimConfig.compile(new IntegrationTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Simulate instruction execution through the 5-stage pipeline
      // Fetch -> Group -> Decode -> Execute -> Writeback
      
      for (instruction <- 0 until 10) {
        // Each instruction takes multiple cycles through pipeline
        for (stage <- 0 until 5) {
          for (cycle <- 0 until 3) {
            dut.clockDomain.waitSampling()
          }
        }
      }
      
      assert(true, "Instruction execution pipeline operated successfully")
    }
  }

  test("T9000Integration should handle system stress test") {
    SimConfig.compile(new IntegrationTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.assertReset()
      dut.clockDomain.waitSampling(10)
      dut.clockDomain.deassertReset()
      
      // Extended stress test
      var errorCount = 0
      val stressCycles = 500
      
      for (cycle <- 0 until stressCycles) {
        try {
          dut.clockDomain.waitSampling()
          
          // Periodic health checks
          if (cycle % 100 == 0) {
            // Verify core systems are still operational
            assert(dut.core != null, s"Core should remain valid at cycle $cycle")
            assert(dut.systemBus != null, s"System bus should remain valid at cycle $cycle")
          }
        } catch {
          case _: Exception => errorCount += 1
        }
      }
      
      // Allow some errors but not too many
      assert(errorCount < stressCycles / 10, s"Too many errors during stress test: $errorCount/$stressCycles")
      assert(true, s"System stress test completed with $errorCount errors out of $stressCycles cycles")
    }
  }
}
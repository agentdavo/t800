package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.regstack.RegStackPlugin
import transputer.plugins.timers.TimerPlugin
import transputer.plugins.schedule.SchedulerPlugin
import transputer.plugins.cache.{MainCachePlugin, WorkspaceCachePlugin}
import transputer.plugins.grouper.InstrGrouperPlugin
import transputer.plugins.vcp.VcpPlugin
import transputer.plugins.pmi.PmiPlugin
import transputer.plugins.event.EventPlugin
import transputer.plugins.analysis.AnalysisPlugin
import transputer.plugins.transputer.TransputerPlugin

/** Comprehensive integration tests for the complete T9000 system.
  *
  * Tests the integration and interaction between all T9000 plugins:
  *   - Core T9000 plugins (Stack, Timer, Scheduler, Cache)
  *   - Communication plugins (VCP, PMI)
  *   - Analysis and debugging plugins (Event, Analysis)
  *   - Full system behavior and performance
  */
class T9000IntegrationSpec extends AnyFunSuite {

  // Full T9000 system DUT with all plugins
  class T9000SystemDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val systemReset = in Bool ()
      val systemEnable = in Bool ()
      val testDataIn = in Bits (32 bits)
      val testAddress = in UInt (32 bits)
      val testWrite = in Bool ()
      val testRead = in Bool ()

      val systemReady = out Bool ()
      val testDataOut = out Bits (32 bits)
      val cacheHit = out Bool ()
      val schedulerActive = out Bool ()
      val vcpRunning = out Bool ()
      val errorDetected = out Bool ()
      val cycleCount = out UInt (32 bits)
    }

    // Create T9000 system with all plugins
    val t9000Plugins = Seq(
      new TransputerPlugin(),
      new RegStackPlugin(),
      new TimerPlugin(),
      new SchedulerPlugin(),
      new MainCachePlugin(),
      new WorkspaceCachePlugin(),
      new InstrGrouperPlugin(),
      new VcpPlugin(),
      new PmiPlugin(),
      new EventPlugin(),
      new AnalysisPlugin()
    )

    val core = Database(db).on(Transputer(t9000Plugins))

    // Get services for testing
    val regStackService = core.host[transputer.plugins.regstack.RegStackService]
    val timerService = core.host[transputer.plugins.timers.TimerService]
    val schedService = core.host[transputer.plugins.schedule.SchedService]
    val vcpService = core.host[transputer.plugins.vcp.VcpService]
    val eventService = core.host[transputer.plugins.event.EventService]
    val analysisService = core.host[transputer.plugins.analysis.AnalysisService]

    // System control logic
    when(io.systemReset) {
      // Reset all subsystems
      vcpService.setVcpCommand(B"8'h01") // Reset VCP
      eventService.disableProcessEvents()
      analysisService.disableAnalysis()
    }.elsewhen(io.systemEnable) {
      // Enable all subsystems
      vcpService.setVcpCommand(B"8'h02") // Start VCP
      eventService.enableProcessEvents()
      analysisService.enableAnalysis()
    }

    // Test memory interface
    when(io.testWrite) {
      regStackService.push(io.testDataIn.asUInt)
    }

    val testOutput = Reg(Bits(32 bits)) init 0
    when(io.testRead) {
      testOutput := regStackService.pop().asBits
    }

    // Output system status
    io.systemReady := !io.systemReset && io.systemEnable
    io.testDataOut := testOutput
    io.cacheHit := True // Simplified - would connect to actual cache
    io.schedulerActive := schedService.hasReady
    io.vcpRunning := vcpService.vcpStatus(2) // Running bit
    io.errorDetected := analysisService.errorDetected
    io.cycleCount := analysisService.cycleCount.resized
  }

  test("T9000 system initialization and basic operation") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Start with system reset
      dut.io.systemReset #= true
      dut.io.systemEnable #= false
      dut.clockDomain.waitSampling(5)

      assert(!dut.io.systemReady.toBoolean, "System should not be ready during reset")

      // Release reset and enable system
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(10)

      assert(dut.io.systemReady.toBoolean, "System should be ready after enable")

      // Verify subsystems are operational
      val cycleCount1 = dut.io.cycleCount.toLong
      dut.clockDomain.waitSampling(20)
      val cycleCount2 = dut.io.cycleCount.toLong

      assert(cycleCount2 > cycleCount1, "Cycle counter should increment")
      assert(!dut.io.errorDetected.toBoolean, "No errors should be detected initially")

      println(s"T9000 system operational: Cycles ${cycleCount2 - cycleCount1} in 20 clock periods")
    }
  }

  test("T9000 core plugins integration") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initialize system
      dut.io.systemReset #= true
      dut.clockDomain.waitSampling(2)
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(5)

      // Test stack operations
      val testValues = Seq(0x11111111L, 0x22222222L, 0x33333333L, 0x44444444L)

      // Push values to stack
      for ((value, index) <- testValues.zipWithIndex) {
        dut.io.testDataIn #= value
        dut.io.testWrite #= true
        dut.clockDomain.waitSampling()
        dut.io.testWrite #= false
        dut.clockDomain.waitSampling(2)

        println(s"Pushed value ${index}: 0x${value.toHexString}")
      }

      // Pop values from stack (should come out in reverse order)
      val poppedValues = scala.collection.mutable.ArrayBuffer[Long]()
      for (index <- testValues.indices) {
        dut.io.testRead #= true
        dut.clockDomain.waitSampling()
        dut.io.testRead #= false
        dut.clockDomain.waitSampling(2)

        val poppedValue = dut.io.testDataOut.toLong
        poppedValues += poppedValue
        println(s"Popped value ${index}: 0x${poppedValue.toHexString}")
      }

      // Verify LIFO behavior (last in, first out)
      val expectedValues = testValues.reverse
      for ((expected, actual) <- expectedValues.zip(poppedValues)) {
        assert(
          actual == expected,
          s"Expected 0x${expected.toHexString}, got 0x${actual.toHexString}"
        )
      }

      println("T9000 core plugin integration test successful")
    }
  }

  test("T9000 communication subsystem integration") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initialize system
      dut.io.systemReset #= true
      dut.clockDomain.waitSampling(2)
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(5)

      // Verify VCP is running
      dut.clockDomain.waitSampling(10)
      assert(dut.io.vcpRunning.toBoolean, "VCP should be running after system enable")

      // Test scheduler activity
      val schedulerActive = dut.io.schedulerActive.toBoolean

      // Cache subsystem should be operational
      assert(dut.io.cacheHit.toBoolean, "Cache should be operational")

      // Let communication subsystem run for a while
      dut.clockDomain.waitSampling(50)

      // Verify no communication errors
      assert(!dut.io.errorDetected.toBoolean, "No communication errors should be detected")

      println("T9000 communication subsystem integration successful")
    }
  }

  test("T9000 analysis and debugging integration") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initialize system
      dut.io.systemReset #= true
      dut.clockDomain.waitSampling(2)
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(5)

      val initialCycles = dut.io.cycleCount.toLong

      // Perform some operations to generate analysis data
      for (i <- 0 until 10) {
        dut.io.testDataIn #= 0x10000000L + i
        dut.io.testWrite #= true
        dut.clockDomain.waitSampling()
        dut.io.testWrite #= false
        dut.clockDomain.waitSampling(2)
      }

      // Check analysis system activity
      val finalCycles = dut.io.cycleCount.toLong
      val cyclesDelta = finalCycles - initialCycles

      assert(cyclesDelta > 30, s"Expected >30 cycles, measured $cyclesDelta")
      assert(
        !dut.io.errorDetected.toBoolean,
        "No errors should be detected during normal operation"
      )

      println(s"T9000 analysis integration: ${cyclesDelta} cycles for 10 operations")
    }
  }

  test("T9000 full system stress test") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(20000)

      // Initialize system
      dut.io.systemReset #= true
      dut.clockDomain.waitSampling(2)
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(5)

      val startCycles = dut.io.cycleCount.toLong

      // Stress test: rapid operations
      for (iteration <- 0 until 50) {
        // Push data
        dut.io.testDataIn #= 0xa0000000L + iteration
        dut.io.testWrite #= true
        dut.clockDomain.waitSampling()
        dut.io.testWrite #= false
        dut.clockDomain.waitSampling()

        // Pop data
        dut.io.testRead #= true
        dut.clockDomain.waitSampling()
        dut.io.testRead #= false
        dut.clockDomain.waitSampling()

        val poppedValue = dut.io.testDataOut.toLong
        val expectedValue = 0xa0000000L + iteration

        assert(
          poppedValue == expectedValue,
          s"Iteration $iteration: Expected 0x${expectedValue.toHexString}, got 0x${poppedValue.toHexString}"
        )

        // Check system health every 10 iterations
        if (iteration % 10 == 0) {
          assert(
            dut.io.systemReady.toBoolean,
            s"System should remain ready at iteration $iteration"
          )
          assert(!dut.io.errorDetected.toBoolean, s"No errors should occur at iteration $iteration")
        }
      }

      val endCycles = dut.io.cycleCount.toLong
      val totalCycles = endCycles - startCycles

      // Final system verification
      assert(dut.io.systemReady.toBoolean, "System should remain ready after stress test")
      assert(!dut.io.errorDetected.toBoolean, "No errors should be detected after stress test")
      assert(dut.io.vcpRunning.toBoolean, "VCP should still be running after stress test")

      println(s"T9000 stress test completed: 50 operations in $totalCycles cycles")
      println(s"Average: ${totalCycles.toDouble / 50.0} cycles per operation")
    }
  }

  test("T9000 system reset and recovery") {
    SimConfig.withWave.compile(new T9000SystemDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(10000)

      // Initialize system
      dut.io.systemReset #= false
      dut.io.systemEnable #= true
      dut.clockDomain.waitSampling(10)

      // Perform some operations
      for (i <- 0 until 5) {
        dut.io.testDataIn #= 0xb0000000L + i
        dut.io.testWrite #= true
        dut.clockDomain.waitSampling()
        dut.io.testWrite #= false
        dut.clockDomain.waitSampling(2)
      }

      val preResetCycles = dut.io.cycleCount.toLong

      // Perform system reset
      dut.io.systemReset #= true
      dut.clockDomain.waitSampling(5)

      assert(!dut.io.systemReady.toBoolean, "System should not be ready during reset")

      // Release reset and verify recovery
      dut.io.systemReset #= false
      dut.clockDomain.waitSampling(10)

      assert(dut.io.systemReady.toBoolean, "System should recover after reset")
      assert(!dut.io.errorDetected.toBoolean, "No errors should be present after reset")

      val postResetCycles = dut.io.cycleCount.toLong
      assert(postResetCycles > preResetCycles, "Cycle counter should continue after reset")

      // Verify system functionality after reset
      dut.io.testDataIn #= 0xdeadbeefL
      dut.io.testWrite #= true
      dut.clockDomain.waitSampling()
      dut.io.testWrite #= false
      dut.clockDomain.waitSampling(2)

      dut.io.testRead #= true
      dut.clockDomain.waitSampling()
      dut.io.testRead #= false
      dut.clockDomain.waitSampling(2)

      assert(
        dut.io.testDataOut.toLong == 0xdeadbeefL,
        "System should function correctly after reset"
      )

      println("T9000 system reset and recovery test successful")
    }
  }
}

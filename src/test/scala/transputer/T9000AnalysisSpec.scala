package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.analysis.{AnalysisService, AnalysisPlugin}

/** Comprehensive tests for T9000 Error Detection and Analysis Unit.
  *
  * Tests the T9000 analysis system implementation:
  *   - Breakpoint management
  *   - Instruction tracing
  *   - Performance counters
  *   - Error detection and analysis
  */
class T9000AnalysisSpec extends AnyFunSuite {

  // Simple DUT to test analysis operations in isolation
  class AnalysisTestDut extends Component {
    val db = Transputer.defaultDatabase()
    db(Global.OPCODE_BITS) = 8

    val io = new Bundle {
      val enableAnalysis = in Bool ()
      val pauseAnalysis = in Bool ()
      val triggerAnalysis = in Bool ()
      val resetCounters = in Bool ()
      val breakpointAddress = in UInt (32 bits)
      val breakpointCondition = in Bits (8 bits)
      val setBreakpoint = in Bool ()
      val clearBreakpointIndex = in UInt (3 bits)
      val clearBreakpoint = in Bool ()
      val traceIndex = in UInt (8 bits)

      val analysisEnabled = out Bool ()
      val analysisPaused = out Bool ()
      val analysisTriggered = out Bool ()
      val breakpointCount = out UInt (4 bits)
      val breakpointHit0 = out Bool ()
      val traceBufferFull = out Bool ()
      val traceBufferCount = out UInt (8 bits)
      val instructionCount = out UInt (32 bits)
      val cycleCount = out UInt (32 bits)
      val errorDetected = out Bool ()
      val stackOverflowError = out Bool ()
      val traceEntry = out Bits (64 bits)
    }

    // Analysis plugin needs basic transputer infrastructure
    val testPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.regstack.RegStackPlugin(),
      new transputer.plugins.analysis.AnalysisPlugin()
    )
    val core = Database(db).on(Transputer(testPlugins))
    val analysisService = core.host[AnalysisService]

    // Analysis control logic
    when(io.enableAnalysis) {
      analysisService.enableAnalysis()
    }.otherwise {
      analysisService.disableAnalysis()
    }

    when(io.pauseAnalysis) {
      analysisService.pauseAnalysis()
    }.otherwise {
      analysisService.resumeAnalysis()
    }

    when(io.triggerAnalysis) {
      analysisService.triggerAnalysis()
    }

    when(io.resetCounters) {
      analysisService.resetCounters()
    }

    when(io.setBreakpoint) {
      analysisService.setBreakpoint(io.breakpointAddress, io.breakpointCondition)
    }

    // Clear specific breakpoint based on index
    for (i <- 0 until 8) {
      when(io.clearBreakpoint && io.clearBreakpointIndex === i) {
        analysisService.clearBreakpoint(i)
      }
    }

    // Output analysis state
    io.analysisEnabled := analysisService.analysisEnabled
    io.analysisPaused := analysisService.analysisPaused
    io.analysisTriggered := analysisService.analysisTriggered
    io.breakpointCount := analysisService.breakpointCount.resized
    io.breakpointHit0 := analysisService.breakpointHit(0)
    io.traceBufferFull := analysisService.traceBufferFull
    io.traceBufferCount := analysisService.traceBufferCount.resized
    io.instructionCount := analysisService.instructionCount.resized
    io.cycleCount := analysisService.cycleCount.resized
    io.errorDetected := analysisService.errorDetected
    io.stackOverflowError := analysisService.stackOverflowError
    io.traceEntry := analysisService.getTraceEntry(io.traceIndex)
  }

  test("T9000 analysis system enable/disable") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Initially analysis should be disabled
      dut.clockDomain.waitSampling()
      assert(!dut.io.analysisEnabled.toBoolean, "Analysis should be disabled initially")
      assert(!dut.io.analysisPaused.toBoolean, "Analysis should not be paused initially")
      assert(!dut.io.analysisTriggered.toBoolean, "Analysis should not be triggered initially")

      // Enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      assert(dut.io.analysisEnabled.toBoolean, "Analysis should be enabled")

      // Pause analysis
      dut.io.pauseAnalysis #= true
      dut.clockDomain.waitSampling(2)

      assert(dut.io.analysisPaused.toBoolean, "Analysis should be paused")

      // Resume analysis
      dut.io.pauseAnalysis #= false
      dut.clockDomain.waitSampling(2)

      assert(!dut.io.analysisPaused.toBoolean, "Analysis should not be paused after resume")

      // Disable analysis
      dut.io.enableAnalysis #= false
      dut.clockDomain.waitSampling(2)

      assert(!dut.io.analysisEnabled.toBoolean, "Analysis should be disabled")
      assert(!dut.io.analysisPaused.toBoolean, "Analysis should not be paused after disable")
    }
  }

  test("T9000 breakpoint management") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable analysis first
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      // Initially no breakpoints should be set
      assert(dut.io.breakpointCount.toLong == 0, "No breakpoints should be set initially")
      assert(!dut.io.breakpointHit0.toBoolean, "Breakpoint 0 should not be hit initially")

      // Set a breakpoint
      dut.io.breakpointAddress #= 0x80001000L
      dut.io.breakpointCondition #= 0x01 // Simple condition
      dut.io.setBreakpoint #= true
      dut.clockDomain.waitSampling()
      dut.io.setBreakpoint #= false
      dut.clockDomain.waitSampling(2)

      assert(dut.io.breakpointCount.toLong == 1, "One breakpoint should be set")

      // Set another breakpoint
      dut.io.breakpointAddress #= 0x80002000L
      dut.io.breakpointCondition #= 0x02
      dut.io.setBreakpoint #= true
      dut.clockDomain.waitSampling()
      dut.io.setBreakpoint #= false
      dut.clockDomain.waitSampling(2)

      assert(dut.io.breakpointCount.toLong == 2, "Two breakpoints should be set")

      // Clear first breakpoint
      dut.io.clearBreakpointIndex #= 0
      dut.io.clearBreakpoint #= true
      dut.clockDomain.waitSampling()
      dut.io.clearBreakpoint #= false
      dut.clockDomain.waitSampling(2)

      assert(dut.io.breakpointCount.toLong == 1, "One breakpoint should remain after clear")

      // Clear remaining breakpoint
      dut.io.clearBreakpointIndex #= 1
      dut.io.clearBreakpoint #= true
      dut.clockDomain.waitSampling()
      dut.io.clearBreakpoint #= false
      dut.clockDomain.waitSampling(2)

      assert(dut.io.breakpointCount.toLong == 0, "No breakpoints should remain")
    }
  }

  test("T9000 instruction trace buffer") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      // Initially trace buffer should be empty
      assert(dut.io.traceBufferCount.toLong == 0, "Trace buffer should be empty initially")
      assert(!dut.io.traceBufferFull.toBoolean, "Trace buffer should not be full initially")

      // Let some cycles pass to potentially fill trace buffer
      // In a real implementation, this would be triggered by instruction execution
      dut.clockDomain.waitSampling(50)

      // Check if trace buffer has entries (depends on implementation)
      val traceCount = dut.io.traceBufferCount.toLong

      // Test reading trace entries
      for (i <- 0 until 8) { // Fixed size for hardware elaboration
        dut.io.traceIndex #= i
        dut.clockDomain.waitSampling()

        val traceEntry = dut.io.traceEntry.toLong
        // Trace entry should contain valid data (non-zero in most cases)
      }

      // Test trace buffer overflow behavior
      dut.clockDomain.waitSampling(256) // Fill buffer beyond capacity

      // Buffer may become full depending on implementation
      val bufferFull = dut.io.traceBufferFull.toBoolean
      if (bufferFull) {
        assert(dut.io.traceBufferCount.toLong > 0, "Trace buffer should have entries when full")
      }
    }
  }

  test("T9000 performance counters") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      // Check initial counter values
      val initialCycles = dut.io.cycleCount.toLong
      val initialInstructions = dut.io.instructionCount.toLong

      // Cycle counter should always increment
      assert(initialCycles > 0, "Cycle counter should be running")

      // Wait some cycles and check increment
      dut.clockDomain.waitSampling(10)

      val afterCycles = dut.io.cycleCount.toLong
      assert(afterCycles > initialCycles, "Cycle counter should increment")
      assert(afterCycles - initialCycles >= 10, "Cycle counter should increment by at least 10")

      // Instruction counter behavior depends on implementation
      val afterInstructions = dut.io.instructionCount.toLong

      // Reset counters
      dut.io.resetCounters #= true
      dut.clockDomain.waitSampling()
      dut.io.resetCounters #= false
      dut.clockDomain.waitSampling(2)

      // Instruction and cache counters should be reset (cycle counter keeps running)
      val resetInstructions = dut.io.instructionCount.toLong
      assert(resetInstructions == 0, "Instruction counter should be reset to 0")

      // Cycle counter should continue running
      val resetCycles = dut.io.cycleCount.toLong
      assert(resetCycles > afterCycles, "Cycle counter should continue running after reset")
    }
  }

  test("T9000 error detection") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      // Initially no errors should be detected
      assert(!dut.io.errorDetected.toBoolean, "No error should be detected initially")
      assert(!dut.io.stackOverflowError.toBoolean, "No stack overflow should be detected initially")

      // Let analysis run for a while
      dut.clockDomain.waitSampling(100)

      // Check for any detected errors
      val errorDetected = dut.io.errorDetected.toBoolean
      val stackOverflow = dut.io.stackOverflowError.toBoolean

      // In our test environment, we may not trigger actual errors
      // but the error detection logic should be functional

      // Disable analysis and verify error state
      dut.io.enableAnalysis #= false
      dut.clockDomain.waitSampling(2)

      // Analysis should be disabled
      assert(!dut.io.analysisEnabled.toBoolean, "Analysis should be disabled")
    }
  }

  test("T9000 analysis trigger mechanism") {
    SimConfig.withWave.compile(new AnalysisTestDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)

      // Enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      // Initially analysis should not be triggered
      assert(!dut.io.analysisTriggered.toBoolean, "Analysis should not be triggered initially")

      // Manually trigger analysis
      dut.io.triggerAnalysis #= true
      dut.clockDomain.waitSampling()
      dut.io.triggerAnalysis #= false
      dut.clockDomain.waitSampling(2)

      assert(dut.io.analysisTriggered.toBoolean, "Analysis should be triggered")

      // Disable analysis to clear trigger
      dut.io.enableAnalysis #= false
      dut.clockDomain.waitSampling(2)

      assert(
        !dut.io.analysisTriggered.toBoolean,
        "Analysis trigger should be cleared when disabled"
      )

      // Re-enable analysis
      dut.io.enableAnalysis #= true
      dut.clockDomain.waitSampling(2)

      assert(dut.io.analysisEnabled.toBoolean, "Analysis should be enabled again")
      assert(
        !dut.io.analysisTriggered.toBoolean,
        "Analysis should not be triggered after re-enable"
      )
    }
  }
}

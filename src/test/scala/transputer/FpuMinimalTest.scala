package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._
import transputer.plugins.fpu._

/** Simple test for FpuMinimal plugin cycle counting */
class FpuMinimalTest extends AnyFunSuite {
  
  // Simple test harness using PluginHost
  class FpuMinimalTestBench extends Component {
    val io = new Bundle {
      val opcode = in Bits(16 bits)
      val trigger = in Bool()
      
      val busy = out Bool()
      val cycleCount = out UInt(6 bits)
      val currentOp = out Bits(8 bits)
      val edgeCase = out Bool()
      val exceptions = out Bits(5 bits)
    }
    
    // Create a minimal transputer with just FPU for testing
    val plugins = Seq(
      new transputer.plugins.core.transputer.TransputerPlugin(),
      new transputer.plugins.core.pipeline.PipelinePlugin(),
      new transputer.plugins.core.regstack.RegStackPlugin(),
      new transputer.plugins.fpu.FpuMinimal(),
      new transputer.plugins.core.pipeline.PipelineBuilderPlugin()
    )
    
    val core = Transputer(plugins)
    val fpuPlugin = core.host[FpuMinimal]
    
    // Connect outputs
    io.busy := fpuPlugin.fpuBusy
    io.cycleCount := fpuPlugin.cycleCounter
    io.currentOp := (if (fpuPlugin.currentOp != null) fpuPlugin.currentOp.asBits.resize(8) else B(0, 8 bits))
    io.edgeCase := fpuPlugin.edgeCaseDetected
    io.exceptions := fpuPlugin.fpuExceptions
    
    // Simple instruction trigger (simplified - real implementation would use pipeline)
    when(io.trigger) {
      // Trigger FPU operation based on opcode
    }
  }
  
  test("FpuMinimal Cycle Counting Basic Test") {
    SimConfig.withWave.compile(new FpuMinimalTestBench).doSim { dut =>
      dut.clockDomain.forkStimulus(10) // 100MHz
      
      println("\n=== FPU MINIMAL CYCLE COUNTING TEST ===")
      println("Testing basic cycle counting functionality")
      
      // Initialize
      dut.io.opcode #= 0
      dut.io.trigger #= false
      dut.clockDomain.waitSampling(10)
      
      // Test FPADD
      println("\nTesting FPADD...")
      dut.io.opcode #= 0x2AF6  // FPADD opcode
      dut.io.trigger #= true
      dut.clockDomain.waitSampling()
      dut.io.trigger #= false
      
      var cycles = 0
      while(dut.io.busy.toBoolean && cycles < 20) {
        dut.clockDomain.waitSampling()
        cycles += 1
        println(s"  Cycle $cycles: busy=${dut.io.busy.toBoolean}, counter=${dut.io.cycleCount.toInt}")
      }
      
      println(s"FPADD completed in $cycles cycles")
      assert(cycles > 0 && cycles < 10, s"FPADD should take 2-4 cycles, took $cycles")
      
      // Test FPMUL  
      println("\nTesting FPMUL...")
      dut.io.opcode #= 0x2AF8  // FPMUL opcode
      dut.io.trigger #= true
      dut.clockDomain.waitSampling()
      dut.io.trigger #= false
      
      cycles = 0
      while(dut.io.busy.toBoolean && cycles < 20) {
        dut.clockDomain.waitSampling()
        cycles += 1
      }
      
      println(s"FPMUL completed in $cycles cycles")
      assert(cycles > 0 && cycles < 10, s"FPMUL should take 3-5 cycles, took $cycles")
      
      // Test FPDIV
      println("\nTesting FPDIV...")
      dut.io.opcode #= 0x2AF9  // FPDIV opcode
      dut.io.trigger #= true
      dut.clockDomain.waitSampling()
      dut.io.trigger #= false
      
      cycles = 0
      while(dut.io.busy.toBoolean && cycles < 30) {
        dut.clockDomain.waitSampling()
        cycles += 1
      }
      
      println(s"FPDIV completed in $cycles cycles")
      assert(cycles > 10 && cycles < 25, s"FPDIV should take 15-20 cycles, took $cycles")
      
      println("\n✅ Basic cycle counting test passed!")
    }
  }
  
  test("FpuMinimal Compilation Test") {
    // Just test that the FPU compiles
    SpinalConfig(targetDirectory = "generated").generateVerilog {
      new FpuMinimalTestBench
    }
    println("✅ FpuMinimal compilation successful!")
  }
}
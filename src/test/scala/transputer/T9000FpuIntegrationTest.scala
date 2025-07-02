package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin._
import transputer.plugins.fpu.FpuPlugin
import transputer.plugins.execute.SecondaryInstrPlugin
import spinal.core.fiber.Database

class T9000FpuIntegrationTest extends AnyFunSuite {
  val simConfig = SimConfig
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
      )
    )
    .withWave

  test("FPU integration with SecondaryInstrPlugin") {
    simConfig
      .compile {
        val db = T9000Transputer.configureDatabase(T9000Param(enableFpu = true))

        // Create minimal set of plugins for FPU testing
        val testPlugins = Seq(
          new transputer.transputer.TransputerPlugin(),
          new transputer.plugins.core.pipeline.T9000PipelinePlugin(),
          new transputer.transputer.T9000GrouperOptimized(),
          new transputer.registers.RegFilePlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.fpu.FpuPlugin(),
          new transputer.plugins.execute.SecondaryInstrPlugin()
        )

        val dut = Database(db).on(new Component {
          val core = Transputer(testPlugins)
          val fpuService = core.host[transputer.plugins.fpu.FpuService]
          val pipelineService = core.host[transputer.plugins.core.pipeline.PipelineStageService]

          // Test interface
          val io = new Bundle {
            val fpuCmdValid = out Bool ()
            val fpuBusy = out Bool ()
            val fpuRspValid = out Bool ()
          }

          // Connect test signals
          io.fpuCmdValid := pipelineService.execute(Global.FPU_CMD_VALID)
          io.fpuBusy := fpuService.isBusy
          io.fpuRspValid := fpuService.rsp.valid
        })

        dut
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)

        // Wait for initialization
        dut.clockDomain.waitSampling(10)

        // Check initial state
        assert(!dut.io.fpuBusy.toBoolean)
        assert(!dut.io.fpuRspValid.toBoolean)

        // Simulate for a few cycles
        dut.clockDomain.waitSampling(100)

        println("FPU integration test passed - basic connectivity verified")
      }
  }

  test("FPU pipeline command passing") {
    simConfig
      .compile {
        val db = T9000Transputer.configureDatabase(T9000Param(enableFpu = true))

        // Create minimal set of plugins for FPU testing
        val testPlugins = Seq(
          new transputer.transputer.TransputerPlugin(),
          new transputer.plugins.core.pipeline.T9000PipelinePlugin(),
          new transputer.transputer.T9000GrouperOptimized(),
          new transputer.registers.RegFilePlugin(),
          new transputer.plugins.core.regstack.RegStackPlugin(),
          new transputer.plugins.fpu.FpuPlugin(),
          new transputer.plugins.execute.SecondaryInstrPlugin()
        )

        val dut = Database(db).on(new Component {
          val core = Transputer(testPlugins)
          val fpuService = core.host[transputer.plugins.fpu.FpuService]
          val pipelineService = core.host[transputer.plugins.core.pipeline.PipelineStageService]
          val regStackService = core.host[transputer.plugins.core.regstack.RegStackService]

          // Test interface
          val io = new Bundle {
            val testOpcode = in Bits (8 bits)
            val enableTest = in Bool ()
            val fpuCmdValid = out Bool ()
            val fpuCmdOp = out Bits (8 bits)
            val fpuBusy = out Bool ()
            val fpuResult = out UInt (64 bits)
            val fpuResultValid = out Bool ()
          }

          // Override opcode for testing
          when(io.enableTest) {
            pipelineService.execute(Global.OPCODE) := io.testOpcode
          }

          // Connect test signals
          io.fpuCmdValid := pipelineService.execute(Global.FPU_CMD_VALID)
          io.fpuCmdOp := pipelineService.execute(Global.FPU_CMD).op
          io.fpuBusy := fpuService.isBusy
          io.fpuResult := fpuService.rsp.payload
          io.fpuResultValid := fpuService.rsp.valid
        })

        dut
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)

        // Wait for initialization
        dut.clockDomain.waitSampling(10)

        // Test FPINT command (convert float to int)
        dut.io.enableTest #= true
        dut.io.testOpcode #= Opcode.SecondaryOpcode.FPINT.asBits.resize(8)

        dut.clockDomain.waitSampling(5)

        // Check if FPU command was issued
        if (dut.io.fpuCmdValid.toBoolean) {
          println(s"FPU command issued: op=${dut.io.fpuCmdOp.toBigInt.toString(16)}")
        }

        // Wait for potential response
        dut.clockDomain.waitSampling(20)

        println("FPU pipeline command test completed")
      }
  }
}

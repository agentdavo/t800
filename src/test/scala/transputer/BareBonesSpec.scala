package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.registers.RegFilePlugin

/** Basic sanity checks for the BareBones core. */
class BareBonesSpec extends AnyFunSuite {
  test("BareBones generates verilog") {
    SpinalVerilog(new BareBones)
  }

  test("BareBones exposes minimal plugins") {
    SimConfig.compile(new BareBones).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      assert(dut.core.host[TransputerPlugin] != null)
      assert(dut.core.host[RegFilePlugin] != null)
      assert(dut.core.host[PipelinePlugin] != null)
      val builder = dut.core.host[PipelineBuilderPlugin]
      assert(builder.buildPipeline().isEmpty)
    }
  }
}

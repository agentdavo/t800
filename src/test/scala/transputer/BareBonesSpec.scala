package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.registers.RegFilePlugin
import transputer.plugins.fetch.FetchPlugin

/** Basic sanity checks for the BareBones core. */
class BareBonesSpec extends AnyFunSuite {
  ignore("BareBones instantiates") {
    assert(new BareBones != null)
  }

  ignore("BareBones exposes minimal plugins") {
    val dut = new BareBones
    assert(dut.core.host[TransputerPlugin] != null)
    assert(dut.core.host[RegFilePlugin] != null)
    assert(dut.core.host[PipelinePlugin] != null)
    assert(dut.core.host[FetchPlugin] != null)
    val builder = dut.core.host[PipelineBuilderPlugin]
    assert(builder.buildPipeline().isEmpty)
  }
}

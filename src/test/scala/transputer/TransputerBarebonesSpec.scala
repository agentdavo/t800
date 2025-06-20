package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.fetch.FetchPlugin

class TransputerBarebonesSpec extends AnyFunSuite {
  test("minimal plugin stack generates verilog") {
    val plugins = Seq(
      new TransputerPlugin,
      new PipelinePlugin,
      new FetchPlugin,
      new PipelineBuilderPlugin
    )
    SpinalVerilog(Transputer(plugins))
  }
}

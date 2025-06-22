package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Plugin}
import spinal.lib.misc.pipeline._
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin, PipelineStageService, PipelineService}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.registers.RegFilePlugin

/** Simple plugin inserting a link between decode and execute stages. */
class DummyLinkPlugin extends FiberPlugin with PipelineService {
  private var links: Seq[Link] = Seq()
  override def getLinks(): Seq[Link] = links

  during build new Area {
    implicit val h: PluginHost = host
    val pipe = Plugin[PipelineStageService]
    links = Seq(StageLink(pipe.decode.down, pipe.execute.up))
  }
}

class PipelineBuilderPluginSpec extends AnyFunSuite {
  test("builder collects stage links") {
    SimConfig.compile {
      val host = new PluginHost
      val plugins = Seq(
        new TransputerPlugin,
        new RegFilePlugin,
        new PipelinePlugin,
        new DummyLinkPlugin,
        new PipelineBuilderPlugin
      )
      PluginHost(host).on(Transputer(plugins))
    }.doSim { dut =>
      val builder = dut.host[PipelineBuilderPlugin]
      assert(builder.buildPipeline().nonEmpty)
    }
  }
}


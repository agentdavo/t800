package transputer.plugins.pipeline

import spinal.core._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._

/** Build all [[Link]] connections provided by [[PipelineService]] implementations. This collects
  * StageLinks from plugins and passes them to the SpinalHDL pipeline [[Builder]].
  */
class PipelineBuilderPlugin extends FiberPlugin {
  val version = "PipelineBuilderPlugin v0.5"

  private var builtLinks: Seq[Link] = Seq()

  /** Return the links created during build. Useful for unit tests. */
  def buildPipeline(): Seq[Link] = builtLinks

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    implicit val h: PluginHost = host
    builtLinks = Plugin.list[PipelineService].flatMap(_.getLinks())
    if (builtLinks.nonEmpty) Builder(builtLinks)
    println(s"[${this.getDisplayName()}] build end")
  }
}

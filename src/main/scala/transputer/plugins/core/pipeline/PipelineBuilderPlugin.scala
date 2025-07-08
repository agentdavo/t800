package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._

/** Build all [[Link]] connections provided by [[PipelineService]] implementations. This collects
  * StageLinks from plugins and passes them to the SpinalHDL pipeline [[Builder]].
  */
class PipelineBuilderPlugin extends FiberPlugin {
  override def getDisplayName(): String = "PipelineBuilderPlugin"
  setName("pipelineBuilder")
  val version = "PipelineBuilderPlugin v0.5"

  // Use scala.collection.Seq for compatibility across Scala versions
  private var builtLinks: scala.collection.Seq[Link] = Seq()

  /** Return the links created during build. Useful for unit tests. */
  def buildPipeline(): scala.collection.Seq[Link] = builtLinks

  during setup new Area {
    println(s"[${PipelineBuilderPlugin.this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    println(s"[${PipelineBuilderPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${PipelineBuilderPlugin.this.getDisplayName()}] build start")
    implicit val h: PluginHost = host
    builtLinks = Plugin.list[PipelineService].flatMap(_.getLinks())
    if (builtLinks.nonEmpty) Builder(builtLinks)
    println(s"[${PipelineBuilderPlugin.this.getDisplayName()}] build end")
  }
}

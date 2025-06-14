package t800.plugins

import spinal.core._
import spinal.core.Area
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline

/** Collect all [[PipelineService]] implementations and build their links.
  */
trait PipelineService {
  def getLinks(): Seq[pipeline.Link]
}

/** The function of this plugin is to collect all the pipeline parts defined by the other's
  * PipelineService plugins and then invoke the spinal.lib.misc.pipeline.builder on them in order to
  * finalize the pipelines hardware.
  */
class PipelineBuilderPlugin extends FiberPlugin {
  val version = "PipelineBuilderPlugin v0.1"
  val elaborationLock = Retainer()
  val logic = during build new Area {
    report(L"Initializing $version")
    elaborationLock.await()
    val chunks = host.list[PipelineService]
    val links = chunks.flatMap(_.getLinks())
    pipeline.Builder(links)
  }
}

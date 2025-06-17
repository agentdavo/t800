package transputer.plugins.pipeline

import spinal.core._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import transputer.Global
import transputer.Transputer
import transputer.plugins.pipeline.PipelineStageService

class PipelineBuilderPlugin extends FiberPlugin {
  val version = "PipelineBuilderPlugin v0.5"
  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    println(s"[${this.getDisplayName()}] build end")
  }
}

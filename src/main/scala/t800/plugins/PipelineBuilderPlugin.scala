package t800.plugins

import spinal.core._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.core.fiber.Retainer
import t800.{Global, T800}

class PipelineBuilderPlugin extends FiberPlugin {
  val version = "PipelineBuilderPlugin v0.3"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val pipe = Plugin[PipelineSrv]
    val fetch = Plugin[FetchPlugin]
    val grouper = try Plugin[GrouperPlugin] catch { case _: Exception => null } // Optional GrouperPlugin
    val primary = Plugin[PrimaryInstrPlugin]

    // Configure pipeline links
    val links = if (grouper != null) {
      // Grouped mode: Fetch -> Grouper -> PrimaryInstr
      Seq(
        StageLink(fetch.pipe.fetch.down, grouper.pipe.decode.up),
        StageLink(grouper.out.down, primary.pipe.decode.up)
      )
    } else {
      // Single-issue mode: Fetch -> PrimaryInstr
      Seq(
        StageLink(fetch.pipe.fetch.down, primary.pipe.decode.up)
      )
    }

    pipe.addLinks(links)
    println(s"[${this.getDisplayName()}] build end")
  }
}

package t800

import spinal.core._
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */
object TopVerilog {
  def main(args: Array[String]): Unit = {
    val variant = args
      .sliding(2, 1)
      .collectFirst { case Array("--variant", v) =>
        v
      }
      .getOrElse("full")

    val plugins = variant match {
      case "min" =>
        Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin,
          new FetchPlugin,
          new ExecutePlugin,
          new ChannelPlugin,
          new SchedulerPlugin,
          new TimerPlugin
        )
      case _ =>
        Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin,
          new FetchPlugin,
          new ExecutePlugin,
          new ChannelPlugin,
          new FpuPlugin,
          new SchedulerPlugin,
          new TimerPlugin
        )
    }

    SpinalVerilog(new T800(plugins))
  }
}

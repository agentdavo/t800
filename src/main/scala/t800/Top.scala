package t800

import spinal.core._
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */
object TopVerilog {
  def main(args: Array[String]): Unit = {
    val plugins = Seq(
      new StackPlugin,
      new PipelinePlugin,
      new MemoryPlugin,
      new FetchPlugin,
      new ChannelPlugin,
      new ExecutePlugin,
      new FpuPlugin,
      new SchedulerPlugin,
      new TimerPlugin
    )

    val db = T800.defaultDatabase()
    SpinalVerilog(new T800(plugins, db))
  }
}

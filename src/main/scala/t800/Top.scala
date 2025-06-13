package t800

import spinal.core._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */
object TopVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog {
      val host = new PluginHost
      val db = T800.defaultDatabase()
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
      new T800(host, plugins, db)
    }
  }
}

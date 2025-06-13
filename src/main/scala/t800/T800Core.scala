package t800

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import t800.plugins._

object T800 {

  /** Create a database pre-loaded with defaults from [[TConsts]]. */
  def defaultDatabase(): Database = {
    val db = new Database
    db(Global.WORD_BITS) = TConsts.WordBits
    db(Global.ADDR_BITS) = TConsts.AddrBits
    db(Global.ROM_WORDS) = TConsts.RomWords
    db(Global.RAM_WORDS) = TConsts.RamWords
    db(Global.LINK_COUNT) = TConsts.LinkCount
    db(Global.FPU_PRECISION) = TConsts.FpuPrecision
    db(Global.SCHED_QUEUE_DEPTH) = TConsts.SchedQueueDepth
    db(Global.RESET_PC) = TConsts.ResetPC
    db
  }
}

class T800(
  val host: PluginHost,
  plugins: Seq[FiberPlugin],
  database: Database = T800.defaultDatabase()
) extends Component {
  Database(database).on {
    host.asHostOf(plugins)
    plugins.foreach(_.awaitBuild())
  }
}

class T800Core
    extends T800(
      new PluginHost,
      Seq(
        new StackPlugin,
        new PipelinePlugin,
        new MemoryPlugin,
        new FetchPlugin,
        new ExecutePlugin,
        new FpuPlugin,
        new SchedulerPlugin,
        new TimerPlugin
      )
    )

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new T800Core)
  }
}

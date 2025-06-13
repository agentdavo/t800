package t800

import spinal.core._
import spinal.lib.misc.database.Database
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
    db(Global.RESET_PC) = TConsts.ResetPC
    db
  }
}

class T800(plugins: Seq[FiberPlugin], database: Database = T800.defaultDatabase())
    extends Component {
  val host = new PluginHost
  Database(database).on {
    host.asHostOf(plugins)
  }
}

class T800Core
    extends T800(
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

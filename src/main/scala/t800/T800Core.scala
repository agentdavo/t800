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
    db(Global.PC_BITS) = TConsts.AddrBits
    db(Global.INSTR_BITS) = 8
    db(Global.IPTR_BITS) = TConsts.AddrBits
    db(Global.OPCODE_BITS) = 8
    db(Global.ROM_WORDS) = TConsts.RomWords
    db(Global.RAM_WORDS) = TConsts.RamWords
    db(Global.LINK_COUNT) = TConsts.LinkCount
    db(Global.FPU_PRECISION) = TConsts.FpuPrecision
    db(Global.SCHED_QUEUE_DEPTH) = TConsts.SchedQueueDepth
    db(Global.RESET_IPTR) = TConsts.ResetIPtr
    db
  }

  /** Standard plugin stack used by [[T800Core]] and [[TopVerilog]]. */
  def defaultPlugins(): Seq[FiberPlugin] =
    Seq(
      new StackPlugin,
      new PipelinePlugin,
      new MemoryPlugin,
      new FetchPlugin,
      new ChannelPlugin,
      new GrouperPlugin,
      new PrimaryInstrPlugin,
      new SecondaryInstrPlugin,
      new FpuPlugin,
      new SchedulerPlugin,
      new TimerPlugin,
      new PipelineBuilderPlugin
    )
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

class T800Core extends T800(new PluginHost, T800.defaultPlugins())

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog {
      val host = new PluginHost
      val plugins = T800.defaultPlugins()
      PluginHost(host).on {
        new T800(host, plugins)
      }
    }
  }
}

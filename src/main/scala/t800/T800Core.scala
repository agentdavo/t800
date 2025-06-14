package t800

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter}
import t800.plugins._

object T800 {
  /** Create a database pre-loaded with defaults from [[Global]]. */
  def defaultDatabase(): Database = {
    // Assumes Global.WORD_BITS, etc., are Element[Int] keys in t800.plugins
    val db = new Database
    db(Global.WORD_BITS) = Global.WordBits
    db(Global.ADDR_BITS) = Global.AddrBits
    db(Global.PC_BITS) = Global.AddrBits
    db(Global.INSTR_BITS) = 8
    db(Global.IPTR_BITS) = Global.AddrBits
    db(Global.OPCODE_BITS) = 8
    db(Global.ROM_WORDS) = Global.RomWords
    db(Global.RAM_WORDS) = Global.RamWords
    db(Global.LINK_COUNT) = Global.LinkCount
    db(Global.FPU_PRECISION) = Global.FpuPrecision
    db(Global.SCHED_QUEUE_DEPTH) = Global.SchedQueueDepth
    db(Global.RESET_IPTR) = Global.ResetIPtr
    db
  }

  /** Parameters for the 128-bit system bus. */
  val systemBusParam = BmbParameter(
    access = BmbAccessParameter(
      addressWidth = Global.ADDR_BITS, // Assumes Global.AddrBits is defined
      dataWidth = 128, // 128-bit wide system bus
      lengthWidth = 4, // Supports up to 16-byte bursts
      sourceWidth = 4, // Supports multiple masters (e.g., CPU, VCP)
      contextWidth = 0
    )
  )

  /** Standard plugin stack used by [[T800Core]] and [[TopVerilog]], integrated with system bus. */
  def defaultPlugins(): Seq[FiberPlugin] = Seq(
    new StackPlugin,
    new PipelinePlugin,
    new MainCachePlugin,
    new WorkspaceCachePlugin,
    new PmiPlugin,
    new FetchPlugin,
    new ChannelPlugin,
    new GrouperPlugin,
    new PrimaryInstrPlugin,
    new SecondaryInstrPlugin,
    new FpuPlugin,
    new SchedulerPlugin,
    new TimerPlugin,
    new PipelineBuilderPlugin,
    new MemoryManagementPlugin // Replaces MemoryPlugin
  )
}

class T800(
  val host: PluginHost,
  plugins: Seq[FiberPlugin],
  database: Database = T800.defaultDatabase()
) extends Component {
  val systemBus = master(Bmb(T800.systemBusParam))

  Database(database).on {
    host.asHostOf(plugins)
    plugins.foreach(_.awaitBuild())
    // Connect plugins to system bus (assumes plugins implement connectToSystemBus)
    plugins.foreach { plugin =>
      plugin match {
        case cache: MainCachePlugin => cache.connectToSystemBus(systemBus)
        case workspace: WorkspaceCachePlugin => workspace.connectToSystemBus(systemBus)
        case pmi: PmiPlugin => pmi.connectToSystemBus(systemBus)
        case _ => // Other plugins may not need direct bus connection
      }
    }
  }
}

class T800Core extends T800(new PluginHost, T800.defaultPlugins())

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    val report = SpinalVerilog {
      val host = new PluginHost
      val plugins = T800.defaultPlugins()
      new T800(host, plugins)
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

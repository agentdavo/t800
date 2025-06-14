package t800

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter}
import t800.plugins._

object T800 {
  /** Create an empty database, populated by TransputerPlugin. */
  def defaultDatabase(): Database = new Database

  /** Parameters for the 128-bit system bus, using Global.AddrBits. */
  val systemBusParam = BmbParameter(
    access = BmbAccessParameter(
      addressWidth = Global.AddrBits, // Dynamic, set by TransputerPlugin
      dataWidth = 128, // 128-bit wide system bus
      lengthWidth = 4, // Supports up to 16-byte bursts
      sourceWidth = 4, // Supports multiple masters (e.g., CPU, VCP)
      contextWidth = 0
    )
  )

  /** Standard plugin stack for T800, aligned with T9000 architecture. */
  def defaultPlugins(): Seq[FiberPlugin] = Seq(
    new TransputerPlugin, // Must be first to set Database values
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
    new MemoryManagementPlugin
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
    // Connect plugins to system bus
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

package t800

import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter}
import t800.SystemBusSrv

object T800 {

  /** Create an empty database, populated by TransputerPlugin. */
  def defaultDatabase(): Database = new Database

  /** Parameters for the 128-bit system bus, using Global.AddrBits. */
  val systemBusParam = BmbParameter(
    addressWidth = Global.AddrBits,
    dataWidth = 128,
    sourceWidth = 4,
    contextWidth = 0,
    lengthWidth = 4
  )

  /** Standard plugin stack for T800, aligned with T9000 architecture. */
  def defaultPlugins(): Seq[FiberPlugin] = Param().plugins()

  /** Minimal plugin set used by unit tests. */
  def unitPlugins(): Seq[FiberPlugin] = Param().plugins()
}

class T800(
  val host: PluginHost,
  plugins: Seq[FiberPlugin],
  database: Database = T800.defaultDatabase()
) extends Component {
  val systemBus = master(Bmb(T800.systemBusParam))

  Database(database).on {
    host.addService(new SystemBusSrv { def bus: Bmb = systemBus })
    host.asHostOf(plugins)
    plugins.foreach(_.awaitBuild())
    // Connect plugins to system bus
    // Plugins with system bus I/O would normally connect here. Most are
    // stubbed out in the minimal variant used for unit testing.
  }
}

class T800Core extends T800(new PluginHost, Param().plugins())

/** Unit variant with only the TransputerPlugin, used by minimal tests. */
class T800Unit(db: Database = T800.defaultDatabase())
    extends T800(new PluginHost, T800.unitPlugins(), db)

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    val param = Param()
    val host = new PluginHost
    val core = new T800(host, param.plugins())
    val report = SpinalVerilog(core)
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

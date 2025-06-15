package t800

import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Hostable}
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

  /** Convenience constructor returning an empty T800. */
  def apply(): T800 = new T800(Database.get)

  /** Convenience constructor wiring the given plugins. */
  def apply(plugins: scala.collection.Seq[Hostable]): T800 = {
    val t = new T800(Database.get)
    t.host.asHostOf(plugins)
    t
  }
}

class T800(val database: Database = new Database) extends Component {
  val host = database on new PluginHost
  val systemBus = master(Bmb(T800.systemBusParam))
  host.addService(new SystemBusSrv { def bus: Bmb = systemBus })
}

class T800Core extends Component {
  val core = T800(T800.defaultPlugins())
}

/** Unit variant with only the TransputerPlugin, used by minimal tests. */
class T800Unit(db: Database = T800.defaultDatabase()) extends Component {
  val core = Database(db).on(T800(T800.unitPlugins()))
}

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    val param = Param()
    val core = Database(T800.defaultDatabase()).on(T800(param.plugins()))
    val report = SpinalVerilog(core)
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

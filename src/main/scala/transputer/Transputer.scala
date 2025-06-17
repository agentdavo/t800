package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Hostable}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.registers.RegFilePlugin
import transputer.plugins.SystemBusService

object Transputer {

  /** Create an empty database, populated by TransputerPlugin. */
  def defaultDatabase(): Database = new Database

  /** Parameters for the 128-bit system bus, using Global.ADDR_BITS. */
  def systemBusParam: BmbParameter = BmbParameter(
    addressWidth = Global.AddrBitsValue,
    dataWidth = 128,
    sourceWidth = 4,
    contextWidth = 0,
    lengthWidth = 4
  )

  /** Standard plugin stack for Transputer, aligned with T9000 architecture. */
  def defaultPlugins(): Seq[FiberPlugin] = Param().plugins()

  /** Minimal plugin set used by unit tests. */
  def unitPlugins(): Seq[FiberPlugin] =
    Seq(
      new TransputerPlugin,
      new RegFilePlugin,
      new PipelinePlugin,
      new PipelineBuilderPlugin
    )

  /** Convenience constructor returning an empty Transputer. */
  def apply(): Transputer = new Transputer(Database.get)

  /** Convenience constructor wiring the given plugins. */
  def apply(plugins: scala.collection.Seq[Hostable]): Transputer = {
    val t = new Transputer(Database.get)
    t.host.asHostOf(plugins)
    t
  }
}

class Transputer(val database: Database = new Database) extends Component {
  val host = database on new PluginHost
  val systemBus = master(Bmb(Transputer.systemBusParam))
  host.addService(new SystemBusService { def bus: Bmb = systemBus })
}

class TransputerCore extends Component {
  val core = Transputer(Transputer.defaultPlugins())
}

class TransputerUnit(db: Database = Transputer.defaultDatabase()) extends Component {
  val core = Database(db).on(Transputer(Transputer.unitPlugins()))
}

object TransputerCoreVerilog {
  def main(args: Array[String]): Unit = {
    // Configure Database for variant support
    val db = Transputer.defaultDatabase()
    val param = Param()
    args.find(_.startsWith("--double-precision")).foreach { _ =>
      db(Global.FPU_PRECISION) = 64
    }
    args.find(_.startsWith("--link-count=")).foreach { arg =>
      db(Global.LINK_COUNT) = arg.split("=")(1).toInt
    }
    args.find(_.startsWith("--ram-words=")).foreach { arg =>
      db(Global.RAM_WORDS) = arg.split("=")(1).toInt
    }

    // Generate Verilog with configured plugins
    val core = Database(db).on(Transputer(param.plugins()))
    val report = SpinalVerilog(core)
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Hostable}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.regstack.RegStackPlugin
import transputer.plugins.fetch.FetchPlugin
// import transputer.plugins.execute.DummySecondaryInstrPlugin
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

  /** Minimal plugin set used by unit tests. Uses real implementations to test the T9000-inspired
    * pipeline flow.
    */
  def unitPlugins(): Seq[FiberPlugin] =
    Seq(
      new TransputerPlugin,
      new RegStackPlugin,
      new PipelinePlugin,
      new transputer.plugins.bus.SystemBusPlugin, // Bus arbitration
      // T9000-inspired instruction processing pipeline
      new FetchPlugin, // systemBus -> FetchPlugin
      new transputer.plugins.grouper.InstrGrouperPlugin, // FetchPlugin -> GrouperPlugin
      // T9000 three-register stack is now part of RegStackPlugin above
      // new transputer.plugins.timers.TimerPlugin,                   // T9000 dual timer system
      // new transputer.plugins.schedule.SchedulerPlugin,             // T9000 process scheduler
      // new transputer.plugins.decode.DummyPrimaryInstrPlugin,       // GrouperPlugin -> PrimaryInstrPlugin (dummy for now)
      // new DummySecondaryInstrPlugin,    // PrimaryInstrPlugin -> SecondaryInstrPlugin (dummy for now)
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

    // Parse command line arguments and configure database
    args.find(_.startsWith("--double-precision")).foreach { _ =>
      db(Global.FPU_PRECISION) = 64
    }
    args.find(_.startsWith("--link-count=")).foreach { arg =>
      db(Global.LINK_COUNT) = arg.split("=")(1).toInt
    }
    args.find(_.startsWith("--ram-words=")).foreach { arg =>
      db(Global.RAM_WORDS) = arg.split("=")(1).toInt
    }

    println("Generating Transputer Core...")
    println(s"Configuration:")
    println(
      s"  FPU Precision: ${if (db.storageExists(Global.FPU_PRECISION)) db(Global.FPU_PRECISION)
        else "32"} bits"
    )
    println(
      s"  Link Count: ${if (db.storageExists(Global.LINK_COUNT)) db(Global.LINK_COUNT) else "4"}"
    )
    println(
      s"  RAM Words: ${if (db.storageExists(Global.RAM_WORDS)) db(Global.RAM_WORDS) else "65536"}"
    )

    // Generate Verilog with database context - create plugins inside SpinalHDL context
    val report = SpinalVerilog {
      Database(db).on {
        val param = Param() // Create param inside SpinalHDL context
        Transputer(param.plugins()) // Now plugins are created in proper context
      }
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

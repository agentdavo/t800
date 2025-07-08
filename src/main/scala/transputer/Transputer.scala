package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Hostable}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import transputer.plugins.core.transputer.TransputerPlugin
import transputer.plugins.core.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.core.regstack.RegStackPlugin
import transputer.plugins.core.fetch.FetchPlugin
// import transputer.plugins.execute.DummySecondaryInstrPlugin
import transputer.plugins.SystemBusService

object Transputer {

  /** Simple placeholder for database functionality. */
  def defaultDatabase(): Unit = ()

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
      new transputer.plugins.core.grouper.InstrGrouperPlugin, // FetchPlugin -> GrouperPlugin
      // T9000 three-register stack is now part of RegStackPlugin above
      // new transputer.plugins.timers.TimerPlugin,                   // T9000 dual timer system
      // new transputer.plugins.schedule.SchedulerPlugin,             // T9000 process scheduler
      // new transputer.plugins.decode.DummyPrimaryInstrPlugin,       // GrouperPlugin -> PrimaryInstrPlugin (dummy for now)
      // new DummySecondaryInstrPlugin,    // PrimaryInstrPlugin -> SecondaryInstrPlugin (dummy for now)
      new PipelineBuilderPlugin
    )

  /** Convenience constructor returning an empty Transputer. */
  def apply(): Transputer = new Transputer()

  /** Convenience constructor wiring the given plugins. */
  def apply(plugins: scala.collection.Seq[Hostable]): Transputer = {
    val t = new Transputer()
    t.host.asHostOf(plugins.toSeq)
    t
  }
}

class Transputer() extends Component {
  val host = new PluginHost
  val systemBus = master(Bmb(Transputer.systemBusParam))
  host.addService(new SystemBusService { def bus: Bmb = systemBus })
}

class TransputerCore extends Component {
  val core = Transputer(Transputer.defaultPlugins())
}

class TransputerUnit() extends Component {
  val core = Transputer(Transputer.unitPlugins())
}

object TransputerCoreVerilog {
  def main(args: Array[String]): Unit = {
    println("Generating Transputer Core...")
    println(s"Configuration:")
    println(s"  FPU Precision: ${Global.FPU_PRECISION} bits")
    println(s"  Link Count: ${Global.LINK_COUNT}")
    println(s"  RAM Words: ${Global.RAM_WORDS}")

    // Generate Verilog with simplified approach
    val report = SpinalVerilog {
      val param = Param() // Create param inside SpinalHDL context
      PluginHost.on {
        Transputer(param.plugins()) // Now plugins are created in proper context
      }
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}


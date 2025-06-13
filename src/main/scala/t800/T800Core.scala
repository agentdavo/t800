package t800

import spinal.core._
import t800.plugins._
import t800.Global

class T800(plugins: Seq[FiberPlugin]) extends Component {
  val database = new Database
  val host = Database(database) on new PluginHost

  database.update(Global.WORD_BITS, TConsts.WordBits)
  database.update(Global.HART_COUNT, 1)

  host.asHostOf(plugins)
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

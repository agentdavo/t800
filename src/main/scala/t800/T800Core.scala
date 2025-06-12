package t800

import spinal.core._
import t800.plugins._

class T800(plugins: Seq[FiberPlugin]) extends Component {
  val host = new PluginHost
  host.asHostOf(plugins)
}

class T800Core
    extends T800(
      Seq(
        new StackPlugin,
        new DecodeExecutePlugin,
        new FpuPlugin,
        new SchedulerPlugin
      )
    )

object T800CoreVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new T800Core)
  }
}

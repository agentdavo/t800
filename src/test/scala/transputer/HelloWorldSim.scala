package transputer

import spinal.core._
import spinal.core.sim._
import transputer.plugins._
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins.schedule.SchedulerPlugin
import transputer.plugins.timers.TimerPlugin
import transputer.plugins.grouper.GrouperPlugin

object HelloWorldSim {

  def main(args: Array[String]): Unit = {
    val romInit = Seq(
      0x24f42540L, 0x4526fe48L, 0xfe4c26feL, 0x26fe4c26L, 0x4022fe4fL, 0xfe4727feL, 0x27fe4f26L,
      0x4c26fe42L, 0xfe4426feL, 0xfe4aL, 0L, 0L, 0L, 0L, 0L, 0L
    ).map(BigInt(_))
    SimConfig.withWave
      .compile {
        val host = new PluginHost
        val p = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(romInit),
          new FetchPlugin,
          new GrouperPlugin,
          new PrimaryInstrPlugin,
          new SecondaryInstrPlugin,
          new ChannelPlugin,
          new SchedulerPlugin,
          new TimerPlugin,
          new PipelineBuilderPlugin
        )
        PluginHost(host).on(Transputer(p))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val chanService = dut.host[ChannelPinsService].pins
        chanService.out.foreach(_.ready #= true)
        var out = List[Int]()
        dut.clockDomain.onSamplings {
          if (chanService.out(0).valid.toBoolean) {
            out ::= chanService.out(0).payload.toInt & 0xff
          }
        }
        dut.clockDomain.waitSampling(200)
        val msg = out.reverse.map(_.toChar).mkString
        println("BYTES=" + out.reverse.map(b => f"$b%02x").mkString(" "))
        println("OUT=" + msg)
        assert(msg == "hello world\n")
      }
  }
}

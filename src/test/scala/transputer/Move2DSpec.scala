package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins.schedule.SchedulerPlugin
import transputer.plugins.grouper.GrouperPlugin

class Move2DSpec extends AnyFunSuite {
  test("MOVE2DALL transfers 2D bytes") {
    val romInit = Seq.fill(16)(BigInt(0))
    // program assembled manually
    val words = Seq(BigInt(0x25444440L), BigInt(0x254042fbL), BigInt(0x0000fffcL))
    val rom = romInit.zipWithIndex.map { case (_, i) =>
      if (i < words.length) words(i) else BigInt(0)
    }

    SimConfig
      .compile {
        val host = new PluginHost
        val p = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
          new GrouperPlugin,
          new ChannelPlugin,
          new DummyTimerPlugin,
          new DummyFpuPlugin,
          new PrimaryInstrPlugin,
          new SecondaryInstrPlugin,
          new SchedulerPlugin,
          new PipelineBuilderPlugin
        )
        PluginHost(host).on(Transputer(p))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val mem = dut.host[MemAccessSrv]
        mem.ram.setBigInt(0, BigInt(0x11223344L))
        mem.ram.setBigInt(1, BigInt(0x55667788L))

        val chan = dut.host[ChannelPinsSrv].pins
        chan.out.foreach(_.ready #= true)
        var out = List[Int]()
        dut.clockDomain.onSamplings {
          if (chan.out(0).valid.toBoolean) out ::= (chan.out(0).payload.toInt & 0xff)
        }
        dut.clockDomain.waitSampling(300)
        assert(out.reverse == List(0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55))
      }
  }
}

package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins.schedule.SchedulerPlugin
import transputer.{DummyTimerPlugin, DummyFpuPlugin}
import transputer.plugins.grouper.GrouperPlugin

class ChannelDmaSpec extends AnyFunSuite {
  test("DMA opcode transfers bytes") {
    val romInit = Seq.fill(16)(BigInt(0))
    val base = romInit.updated(0, BigInt(0x4a484040L))

    SimConfig
      .compile {
        val host = new PluginHost
        val p = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(base),
          new FetchPlugin,
          new ChannelPlugin,
          new GrouperPlugin,
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
        val mem = dut.host[MemAccessService]
        mem.ram.setBigInt(0, BigInt(0x11223344L))
        mem.ram.setBigInt(1, BigInt(0x55667788L))

        val chan = dut.host[ChannelPinsService].pins
        chan.out.foreach(_.ready #= true)
        var out = List[Int]()
        dut.clockDomain.onSamplings {
          if (chan.out(0).valid.toBoolean) out ::= (chan.out(0).payload.toInt & 0xff)
        }
        dut.clockDomain.waitSampling(200)
        assert(out.reverse == List(0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55))
      }
  }
}

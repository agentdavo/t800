package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import spinal.lib._
import transputer.plugins.schedule.SchedulerPlugin
import spinal.lib.misc.plugin.PluginHost
import transputer.{DummyTimerPlugin, DummyFpuPlugin}
import transputer.plugins.grouper.GrouperPlugin

class OprStlSpec extends AnyFunSuite {
  test("STL stores A and updates stack") {
    val romInit = Seq.fill(16)(BigInt(0))
    val word0 = BigInt(0x42224323L)
    val word1 = BigInt(0x00d04121L)
    val rom = romInit.updated(0, word0).updated(1, word1)

    SimConfig
      .compile {
        val host = new PluginHost
        val p = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
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
        dut.clockDomain.waitSampling(40)
        val stack = dut.host[StackService]
        assert(stack.A.toBigInt == 0x22)
        assert(stack.B.toBigInt == 0x33)
      }
  }
}

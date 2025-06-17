package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins.schedule.SchedulerPlugin
import transputer.{DummyTimerPlugin, DummyFpuPlugin}
import transputer.plugins.grouper.GrouperPlugin

class PrefixSpec extends AnyFunSuite {
  test("PFIX/NFIX build literals") {
    val romInit = Seq.fill(16)(BigInt(0))
    val word0 = BigInt(0x00004f60L) // NFIX 0 ; LDC 15
    val rom = romInit.updated(0, word0)

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
        dut.clockDomain.waitSampling(20)
        val stack = dut.host[StackSrv]
        assert(stack.A.toBigInt == BigInt("ffffffff", 16))
      }
  }
}

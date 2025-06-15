package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins.schedule.SchedulerPlugin
import t800.{DummyTimerPlugin, DummyFpuPlugin}
import t800.plugins.grouper.GrouperPlugin

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
        PluginHost(host).on {
          new T800(host, p)
        }
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling(20)
        val stack = dut.host[StackSrv]
        assert(stack.A.toBigInt == BigInt("ffffffff", 16))
      }
  }
}

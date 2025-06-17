package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins._
import transputer.{DummyTimerPlugin, DummyFpuPlugin}
import transputer.plugins.schedule.{SchedulerPlugin, SchedSrv}
import transputer.plugins.grouper.GrouperPlugin

class SchedulerSaveSpec extends AnyFunSuite {
  test("LEND re-enqueues current workspace") {
    val romInit = Seq.fill(16)(BigInt(0))
    val word0 = BigInt(0x00001521L) // LEND; STOPP
    val rom = romInit.updated(0, word0)

    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(
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
        PluginHost(host).on(Transputer(plugins))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val stack = dut.host[StackSrv]
        val sched = dut.host[SchedSrv]
        stack.A #= 2
        dut.clockDomain.waitSampling(20)
        assert(sched.nextProc.toBigInt == stack.WPtr.toBigInt)
      }
  }
}

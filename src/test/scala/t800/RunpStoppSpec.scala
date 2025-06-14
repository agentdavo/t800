package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost
import t800.plugins._
import t800.{DummyTimerPlugin, DummyFpuPlugin}

class RunpStoppSpec extends AnyFunSuite {
  test("RUNP enqueues A and STOPP enqueues WPtr") {
    val romInit = Seq.fill(16)(BigInt(0))
    // bytes: RUNP, STOPP, 0, 0
    val word0 = BigInt(0x00001539L)
    val rom = romInit.updated(0, word0)

    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
          new DummyTimerPlugin,
          new DummyFpuPlugin,
          new ExecutePlugin,
          new SchedulerPlugin
        )
        PluginHost(host).on(new T800(host, plugins))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val stack = dut.host[StackSrv]
        val sched = dut.host[SchedSrv]
        stack.A #= 0x1234
        dut.clockDomain.waitSampling(15)
        val first = sched.nextProc.toBigInt
        dut.clockDomain.waitSampling(30)
        val second = sched.nextProc.toBigInt
        assert(first == 0x1234)
        assert(second == stack.WPtr.toBigInt)
      }
  }
}

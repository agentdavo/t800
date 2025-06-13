package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import spinal.lib._
import spinal.lib.misc.plugin.PluginHost

import t800.{DummyTimerPlugin, DummyFpuPlugin}

class OprLdlSpec extends AnyFunSuite {
  test("LDL loads from workspace") {
    val romInit = Seq.fill(16)(BigInt(0))
    val word0 = BigInt(0x42224323L)
    val word1 = BigInt(0x70d04121L)
    val rom = romInit.updated(0, word0).updated(1, word1)

    SimConfig
      .compile {
        val host = new PluginHost
        val p = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
          new DummyTimerPlugin,
          new DummyFpuPlugin,
          new ExecutePlugin,
          new SchedulerPlugin
        )
        PluginHost(host).on {
          new T800(host, p)
        }
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling(50)
        val stack = dut.host[StackSrv]
        assert(stack.A.toBigInt == 0x11)
        assert(stack.B.toBigInt == 0x22)
        assert(stack.C.toBigInt == 0x33)
      }
  }
}

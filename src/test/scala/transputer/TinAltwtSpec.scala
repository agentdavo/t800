package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins._
import transputer.plugins.timers.TimerPlugin
import transputer.plugins.grouper.GrouperPlugin

import transputer.plugins.schedule.SchedulerPlugin
class TinAltwtSpec extends AnyFunSuite {
  def buildRom(opcodes: Seq[Int]): Seq[BigInt] = {
    val bytes = opcodes.map(_.toByte)
    val words = bytes
      .grouped(4)
      .map { chunk =>
        val p = chunk.padTo(4, 0.toByte)
        val w = (p(3) & 0xff) << 24 | (p(2) & 0xff) << 16 | (p(1) & 0xff) << 8 | (p(0) & 0xff)
        BigInt(w & 0xffffffffL)
      }
      .toSeq
    val romInit = Seq.fill(16)(BigInt(0))
    romInit.zipWithIndex.map { case (_, i) => if (i < words.length) words(i) else BigInt(0) }
  }

  def runTest(opcode: Int): Unit = {
    val rom = buildRom(Seq(0x48, opcode, 0x4b, 0x15))
    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
          new GrouperPlugin,
          new DummyFpuPlugin,
          new PrimaryInstrPlugin,
          new SecondaryInstrPlugin,
          new SchedulerPlugin,
          new TimerPlugin,
          new PipelineBuilderPlugin
        )
        PluginHost(host).on(Transputer(plugins))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling(40)
        val stack = dut.host[StackService]
        assert(stack.A.toBigInt == 0x0b)
      }
  }

  test("TIN waits for timer") { runTest(0x2b) }
  test("ALTWT waits for timer") { runTest(0x44) }
}

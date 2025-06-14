package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import spinal.lib.misc.plugin.PluginHost

class GrouperPluginSpec extends AnyFunSuite {
  test("instruction groups preserve order") {
    // Program bytes: ldl, ldl, wsub, ldnl, ldl, ldl, wsub, ldnl, add, ldl, ldl, wsub, stnl
    val bytes = Seq(0x70, 0x71, 0x0a, 0x3f, 0x72, 0x73, 0x0a, 0x37, 0x05, 0x74, 0x75, 0x0a, 0xe1)
    val romInit = Array.fill(16)(BigInt(0))
    for ((b, idx) <- bytes.zipWithIndex) {
      val word = idx / 4
      val shift = (idx % 4) * 8
      romInit(word) = romInit(word) | (BigInt(b & 0xff) << shift)
    }
    val rom = romInit.toSeq

    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin(rom),
          new FetchPlugin,
          new GrouperPlugin,
          new PipelineBuilderPlugin
        )
        PluginHost(host).on(new T800(host, plugins))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val grouper = dut.host[GroupedInstrSrv].groups
        // Wait for the first group to become valid
        while (!grouper.valid.toBoolean) dut.clockDomain.waitSampling()
        val count = grouper.payload.count.toBigInt.toInt
        val instrs = grouper.payload.instructions.map(_.toBigInt.toInt & 0xff)
        assert(count == 8)
        assert(instrs.take(count) == bytes.take(8))
      }
  }
}

package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import spinal.lib.misc.plugin.PluginHost
import transputer.plugins.schedule.SchedulerPlugin
import transputer.plugins.timers.TimerPlugin
import transputer.plugins.grouper.GrouperPlugin

class HelloWorldSpec extends AnyFunSuite {

  // Simple link plugin exposing ChannelPinsService
  class ChannelPlugin extends FiberPlugin {
    private var pinsReg: ChannelPins = null
    during setup new Area {
      pinsReg = ChannelPins(Global.LinkCount)
      pinsReg.in.foreach(_.setIdle())
      pinsReg.out.foreach(_.setIdle())
      addService(new ChannelPinsService { def pins: ChannelPins = pinsReg })
    }
    during build new Area {}
  }

  test("ROM program prints hello world") {
    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(
          new StackPlugin,
          new PipelinePlugin,
          new MemoryPlugin,
          new FetchPlugin,
          new GrouperPlugin,
          new PrimaryInstrPlugin,
          new SecondaryInstrPlugin,
          new ChannelPlugin,
          new SchedulerPlugin,
          new TimerPlugin,
          new PipelineBuilderPlugin
        )
        PluginHost(host).on(Transputer(plugins))
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val memService = dut.host[MemAccessService]
        val chan = dut.host[ChannelPinsService].pins
        val prog = Seq(
          0x40, 0x25, 0xf4, 0x24, 0x48, 0xfe, 0x26, 0x45, 0xfe, 0x26, 0x4c, 0xfe, 0x26, 0x4c, 0xfe,
          0x26, 0x4f, 0xfe, 0x22, 0x40, 0xfe, 0x27, 0x47, 0xfe, 0x26, 0x4f, 0xfe, 0x27, 0x42, 0xfe,
          0x26, 0x4c, 0xfe, 0x26, 0x44, 0xfe, 0x4a, 0xfe, 0x00
        )
        // load program into ROM
        val rom = memService.rom
        for ((b, idx) <- prog.zipWithIndex) {
          val addr = idx / 4
          val shift = (idx % 4) * 8
          val old = if (addr < rom.wordCount) rom.getBigInt(addr).toLong else 0L
          val value = (old | (b & 0xffL) << shift) & 0xffffffffL
          rom.setBigInt(addr, BigInt(value))
        }
        chan.out.foreach(_.ready #= true)
        var out = List[Int]()
        dut.clockDomain.onSamplings {
          if (chan.out(0).valid.toBoolean)
            out ::= (chan.out(0).payload.toInt & 0xff)
        }
        dut.clockDomain.waitSampling(200)
        val msg = out.reverse.map(_.toChar).mkString
        assert(msg == "hello world\n")
      }
  }
}

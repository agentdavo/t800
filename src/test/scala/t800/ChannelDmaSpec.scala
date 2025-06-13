package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}

class DummyTimerPlugin extends FiberPlugin {
  private var hiReg, loReg: UInt = null
  during setup new Area {
    hiReg = Reg(UInt(TConsts.WordBits bits)) init 0
    loReg = Reg(UInt(TConsts.WordBits bits)) init 0
    addService(new TimerSrv {
      override def hi: UInt = hiReg
      override def lo: UInt = loReg
      override def set(value: UInt): Unit = {
        hiReg := value
        loReg := value
      }
      override def enableHi(): Unit = {}
      override def enableLo(): Unit = {}
      override def disableHi(): Unit = hiReg := hiReg
      override def disableLo(): Unit = loReg := loReg
    })
  }
}

class DummyFpuPlugin extends FiberPlugin {
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null
  during setup new Area {
    pipeReg = Flow(new FpCmd())
    pipeReg.setIdle()
    rspReg = Flow(UInt(TConsts.WordBits bits))
    rspReg.setIdle()
    addService(new FpuSrv {
      override def pipe: Flow[FpCmd] = pipeReg
      override def rsp: Flow[UInt] = rspReg
    })
  }
}

class ChannelDmaSpec extends AnyFunSuite {
  test("DMA opcode transfers bytes") {
    val romInit = Seq.fill(16)(BigInt(0))
    val base = romInit.updated(0, BigInt(0x4a484040L))

    SimConfig
      .compile {
        PluginHost.on {
          val host = new PluginHost
          val p = Seq(
            new StackPlugin,
            new PipelinePlugin,
            new MemoryPlugin(base),
            new FetchPlugin,
            new ExecutePlugin,
            new ChannelPlugin,
            new SchedulerPlugin,
            new DummyTimerPlugin,
            new DummyFpuPlugin
          )
          new T800(host, p)
        }
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val mem = dut.host[MemAccessSrv]
        mem.ram.setBigInt(0, BigInt(0x11223344L))
        mem.ram.setBigInt(1, BigInt(0x55667788L))

        val chan = dut.host[ChannelPinsSrv].pins
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

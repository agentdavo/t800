package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import t800.Global
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Plugin}

trait TimerProbeSrv { def hi: UInt; def lo: UInt }

class TimerProbePlugin extends FiberPlugin {
  var hiOut: UInt = null
  var loOut: UInt = null

  during setup new Area {
    hiOut = out UInt (Global.WORD_BITS bits)
    loOut = out UInt (Global.WORD_BITS bits)
  }

  during build new Area {
    implicit val h: PluginHost = host
    val timer = Plugin[TimerSrv]
    hiOut := timer.hi
    loOut := timer.lo
    addService(new TimerProbeSrv { def hi = hiOut; def lo = loOut })
  }
}

class TimerEnableSpec extends AnyFunSuite {
  test("counters pause and resume") {
    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(new TimerPlugin, new TimerProbePlugin)
        PluginHost(host).on {
          new T800(host, plugins)
        }
      }
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val timer = dut.host[TimerSrv]

        dut.clockDomain.waitSampling(5)
        timer.disableHi()
        timer.disableLo()
        dut.clockDomain.waitSampling()
        val hiPaused = timer.hi.toBigInt
        val loPaused = timer.lo.toBigInt

        dut.clockDomain.waitSampling(70)
        assert(timer.hi.toBigInt == hiPaused)
        assert(timer.lo.toBigInt == loPaused)

        timer.enableHi()
        timer.enableLo()
        dut.clockDomain.waitSampling()
        val hiResume = timer.hi.toBigInt
        val loResume = timer.lo.toBigInt

        dut.clockDomain.waitSampling(70)
        assert(timer.hi.toBigInt == hiResume + 70)
        assert(timer.lo.toBigInt == loResume + 1)
      }
  }
}

package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import t800.Global

trait TimerProbeSrv { def hi: UInt; def lo: UInt }

class TimerProbePlugin extends FiberPlugin {
  var hiOut: UInt = null
  var loOut: UInt = null

  override def setup(): Unit = {
    hiOut = out UInt (Global.WORD_BITS bits)
    loOut = out UInt (Global.WORD_BITS bits)
  }

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val timer = Plugin[TimerSrv]
    hiOut := timer.hi
    loOut := timer.lo
    addService(new TimerProbeSrv { def hi = hiOut; def lo = loOut })
  }
}

class TimerEnableSpec extends AnyFunSuite {
  test("counters pause and resume") {
    val plugins = Seq(new TimerPlugin, new TimerProbePlugin)
    SimConfig.compile(new T800(plugins)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val timer = dut.host.service[TimerSrv]

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

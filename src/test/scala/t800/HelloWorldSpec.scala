package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._

class HelloWorldSpec extends AnyFunSuite {
  ignore("ROM program prints hello world") {
    val romInit = Seq(
      0x24f42540L, 0x4526fe48L, 0xfe4c26feL, 0x26fe4c26L, 0x4022fe4fL, 0xfe4727feL, 0x27fe4f26L,
      0x4c26fe42L, 0xfe4426feL, 0xfe4aL, 0L, 0L, 0L, 0L, 0L, 0L
    ).map(BigInt(_))
    val plugins = Seq(
      new StackPlugin,
      new PipelinePlugin,
      new MemoryPlugin(romInit),
      new FetchPlugin,
      new ExecutePlugin,
      new ChannelPlugin,
      new SchedulerPlugin,
      new TimerPlugin
    )
    SimConfig.withWave.compile(new T800(plugins)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val pins = dut.host.service[ChannelPinsSrv].pins
      pins.out.foreach(_.ready #= true)
      var out = List[Int]()
      dut.clockDomain.onSamplings {
        if (pins.out(0).valid.toBoolean)
          out ::= (pins.out(0).payload.toInt & 0xff)
      }
      dut.clockDomain.waitSampling(1000)
      val msg = out.reverse.map(_.toChar).mkString
      println("OUT=" + msg)
      assert(msg == "hello world\n")
    }
  }
}

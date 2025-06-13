package t800

import spinal.core._
import spinal.core.sim._
import t800.plugins._

object HelloWorldSim {
  private def loadProgram(memSrv: MemAccessSrv, bytes: Seq[Int]): Unit = {
    val rom = memSrv.rom
    for ((b, idx) <- bytes.zipWithIndex) {
      val addr = idx / 4
      val shift = (idx % 4) * 8
      val old = if (addr < rom.wordCount) rom.getBigInt(addr).toLong else 0L
      val value = (old | (b & 0xffL) << shift) & 0xffffffffL
      rom.setBigInt(addr, BigInt(value))
    }
  }

  def main(args: Array[String]): Unit = {
    val plugins = Seq(
      new StackPlugin,
      new PipelinePlugin,
      new MemoryPlugin,
      new FetchPlugin,
      new ExecutePlugin,
      new ChannelPlugin,
      new SchedulerPlugin,
      new TimerPlugin
    )
    SimConfig.withWave.compile(new T800(plugins)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val memSrv = dut.host.service[MemAccessSrv]
      val chanSrv = dut.host.service[ChannelPinsSrv].pins
      // Program bytes: timer=0 and output "hello world\n"
      val prog = Seq(
        0x40, 0x25, 0xf4, 0x24, 0x48, 0xfe, 0x26, 0x45, 0xfe, 0x26, 0x4c, 0xfe, 0x26, 0x4c, 0xfe,
        0x26, 0x4f, 0xfe, 0x22, 0x40, 0xfe, 0x27, 0x47, 0xfe, 0x26, 0x4f, 0xfe, 0x27, 0x42, 0xfe,
        0x26, 0x4c, 0xfe, 0x26, 0x44, 0xfe, 0x4a, 0xfe, 0x00
      )
      loadProgram(memSrv, prog)
      chanSrv.out.foreach(_.ready #= true)
      var out = List[Int]()
      dut.clockDomain.onSamplings {
        if (chanSrv.out(0).valid.toBoolean) {
          out ::= chanSrv.out(0).payload.toInt & 0xff
        }
      }
      dut.clockDomain.waitSampling(200)
      val msg = out.reverse.map(_.toChar).mkString
      println("OUT=" + msg)
      assert(msg == "hello world\n")
    }
  }
}

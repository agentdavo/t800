package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.HexTools
import transputer.plugins.fetch.InstrFetchService

object BootRomFetchSim {
  def main(args: Array[String]): Unit = {
    val romFile = "bootrom.hex"

    // Parse boot ROM into 64-bit little-endian words
    val rom32 = HexTools.readHexFile(romFile, 0)
    val romWords = rom32
      .grouped(2)
      .map { chunk =>
        val lo = chunk.headOption.getOrElse(BigInt(0))
        val hi = chunk.lift(1).getOrElse(BigInt(0))
        (hi << 32) | lo
      }
      .toSeq

    SimConfig.withWave.compile(new BootRomDesign(romFile)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(1000)
      val fetch = dut.core.host[InstrFetchService]

      fetch.cmd.valid #= true
      fetch.cmd.address #= 0x80000000L

      // Wait for the command to be accepted
      dut.clockDomain.waitSamplingWhere(fetch.cmd.ready.toBoolean)

      for ((expected, idx) <- romWords.zipWithIndex) {
        assert(fetch.rsp.valid.toBoolean, s"rsp.valid low at word $idx")
        val got = fetch.rsp.payload.toBigInt
        assert(
          got == expected,
          f"Word $idx expected 0x$expected%x got 0x$got%x"
        )
        dut.clockDomain.waitSampling()
      }
    }
  }
}

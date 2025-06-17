package transputer

import spinal.core.sim._
import spinal.core._

object TransputerCoreSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new TransputerCore).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 150) { dut.clockDomain.waitSampling() }
    }
  }
}

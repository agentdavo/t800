package t800

import spinal.core.sim._
import spinal.core._

object T800CoreSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new T800Core).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      for (_ <- 0 until 150) { dut.clockDomain.waitSampling() }
    }
  }
}

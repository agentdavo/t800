package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

/** Simple test verifying dynamic right shift alignment. */
class AlignDut extends Component {
  val io = new Bundle {
    val mant1 = in UInt (53 bits)
    val mant2 = in UInt (53 bits)
    val expDiff = in SInt (11 bits)
    val sum = out UInt (54 bits)
  }
  val alignShift = io.expDiff.abs
  val m2Shifted = io.mant2 >> alignShift
  io.sum := io.mant1.resize(54) + m2Shifted.resize(54)
}

class FpuAdderSpec extends AnyFunSuite {
  test("mantissa alignment when op1 exponent is greater") {
    val compiled = SimConfig.compile(new AlignDut)
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      // 1.0 (mantissa 0x10000000000000)
      dut.io.mant1 #= BigInt("10000000000000", 16)
      // 0.25 (mantissa will be shifted by 2)
      dut.io.mant2 #= BigInt("10000000000000", 16)
      dut.io.expDiff #= 2
      dut.clockDomain.waitSampling()
      val expected = BigInt("14000000000000", 16)
      assert(dut.io.sum.toBigInt == expected)
    }
  }

  test("mantissa alignment when op2 exponent is greater") {
    val compiled = SimConfig.compile(new AlignDut)
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      // 0.5 (mantissa 0x10000000000000, but op1 exponent smaller by 1)
      dut.io.mant1 #= BigInt("10000000000000", 16)
      // 1.0 will remain unshifted
      dut.io.mant2 #= BigInt("10000000000000", 16)
      dut.io.expDiff #= -1
      dut.clockDomain.waitSampling()
      val expected = BigInt("18000000000000", 16)
      assert(dut.io.sum.toBigInt == expected)
    }
  }
}

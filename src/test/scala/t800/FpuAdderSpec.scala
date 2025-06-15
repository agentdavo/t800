package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.FpuAdder

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

  /** DUT wrapping the full adder pipeline */
  class AdderDut extends Component {
    val io = new Bundle {
      val a = in Bits (64 bits)
      val b = in Bits (64 bits)
      val add = in Bool ()
      val result = out Bits (64 bits)
    }
    val adder = new FpuAdder
    adder.io.op1 := io.a
    adder.io.op2 := io.b
    adder.io.isAdd := io.add
    adder.io.isExpInc := False
    adder.io.isExpDec := False
    adder.io.isMulBy2 := False
    adder.io.isDivBy2 := False
    adder.io.roundingMode := 0
    io.result := adder.io.result
  }

  test("add positive and negative numbers") {
    val dut = SimConfig.compile(new AdderDut)
    dut.doSim { d =>
      d.clockDomain.forkStimulus(10)
      d.io.a #= BigInt("4008000000000000", 16) // 3.0
      d.io.b #= BigInt("bff0000000000000", 16) // -1.0
      d.io.add #= true
      d.clockDomain.waitSampling(4)
      assert(d.io.result.toBigInt == BigInt("4000000000000000", 16)) // 2.0
    }
  }

  test("large exponent gap") {
    val dut = SimConfig.compile(new AdderDut)
    dut.doSim { d =>
      d.clockDomain.forkStimulus(10)
      d.io.a #= BigInt("4270000000000000", 16) // 2^40
      d.io.b #= BigInt("3ff0000000000000", 16) // 1.0
      d.io.add #= true
      d.clockDomain.waitSampling(4)
      assert(d.io.result.toBigInt == BigInt("4270000000001000", 16))
    }
  }

  test("denormal addition") {
    val dut = SimConfig.compile(new AdderDut)
    dut.doSim { d =>
      d.clockDomain.forkStimulus(10)
      d.io.a #= BigInt("000730d67819e8d2", 16) // ~1e-308
      d.io.b #= BigInt("000730d67819e8d2", 16)
      d.io.add #= true
      d.clockDomain.waitSampling(4)
      assert(d.io.result.toBigInt == BigInt("000e61acf033d1a4", 16))
    }
  }
}

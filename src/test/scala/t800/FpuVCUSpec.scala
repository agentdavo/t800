package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.FpuVCU

class VcuDut extends Component {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val opcode = in Bits (8 bits)
    val isSpecial = out Bool ()
    val specialResult = out Bits (64 bits)
    val trapEnable = out Bool ()
    val cmp = out Bool ()
  }
  private val vcu = new FpuVCU
  vcu.io.op1 := io.op1
  vcu.io.op2 := io.op2
  vcu.io.opcode := io.opcode
  io.isSpecial := vcu.io.isSpecial
  io.specialResult := vcu.io.specialResult
  io.trapEnable := vcu.io.trapEnable
  io.cmp := vcu.io.comparisonResult
}

class FpuVCUSpec extends AnyFunSuite {
  private val nan = BigInt("7ff8000000000000", 16)
  private val posInf = BigInt("7ff0000000000000", 16)
  private val negInf = BigInt("fff0000000000000", 16)
  private val posZero = BigInt("0000000000000000", 16)
  private val negZero = BigInt("8000000000000000", 16)
  private val denorm = BigInt("0000000000000001", 16)
  private val one = BigInt("3ff0000000000000", 16)
  private val two = BigInt("4000000000000000", 16)

  private def twosComp(value: BigInt, width: Int): BigInt = {
    val mod = BigInt(1) << width
    (value + mod) & (mod - 1)
  }

  private def normalizeDenormal(v: BigInt): BigInt = {
    val sign = if ((v >> 63) == 1) BigInt(1) else BigInt(0)
    val mantissa = v & ((BigInt(1) << 52) - 1)
    val shift = 52 - mantissa.bitLength
    val exponent = twosComp(BigInt(1 - shift), 11)
    val normMantissa = (mantissa << shift) & ((BigInt(1) << 52) - 1)
    (sign << 63) | (exponent << 52) | normMantissa
  }

  private def run(op1: BigInt, op2: BigInt, op: Int)(check: VcuDut => Unit): Unit = {
    SimConfig.compile(new VcuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= op1
      dut.io.op2 #= op2
      dut.io.opcode #= op
      dut.clockDomain.waitSampling()
      check(dut)
    }
  }

  test("detect NaN and canonical result") {
    run(nan, one, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(dut.io.trapEnable.toBoolean)
      assert(dut.io.specialResult.toBigInt == nan)
    }
  }

  test("detect infinities and sign propagate") {
    run(posInf, two, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(!dut.io.trapEnable.toBoolean)
      assert(dut.io.specialResult.toBigInt == posInf)
    }
    run(negInf, one, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(!dut.io.trapEnable.toBoolean)
      assert(dut.io.specialResult.toBigInt == negInf)
    }
  }

  test("normalize denormals") {
    val expected = normalizeDenormal(denorm)
    run(denorm, one, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(dut.io.trapEnable.toBoolean)
      assert(dut.io.specialResult.toBigInt == expected)
    }
  }

  test("propagate zero sign") {
    run(posZero, posZero, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(dut.io.specialResult.toBigInt == posZero)
    }
    run(negZero, posZero, 0x94) { dut =>
      assert(dut.io.isSpecial.toBoolean)
      assert(dut.io.specialResult.toBigInt == negZero)
    }
  }

  test("comparison opcodes") {
    run(two, one, 0x94) { dut => assert(dut.io.cmp.toBoolean) }
    run(one, one, 0x95) { dut => assert(dut.io.cmp.toBoolean) }
    run(one, two, 0x97) { dut => assert(!dut.io.cmp.toBoolean) }
    run(two, one, 0x97) { dut => assert(dut.io.cmp.toBoolean) }
    run(two, one, 0x9b) { dut => assert(dut.io.cmp.toBoolean) }
    run(one, one, 0x9b) { dut => assert(!dut.io.cmp.toBoolean) }
    run(one, one, 0x92) { dut => assert(dut.io.cmp.toBoolean) }
    run(nan, one, 0x92) { dut => assert(!dut.io.cmp.toBoolean) }
    run(nan, one, 0x91) { dut => assert(dut.io.cmp.toBoolean) }
    run(posInf, one, 0x93) { dut => assert(!dut.io.cmp.toBoolean) }
    run(one, one, 0x93) { dut => assert(dut.io.cmp.toBoolean) }
  }

  test("trapEnable on fpchkerr") {
    run(posInf, one, 0x83) { dut => assert(dut.io.trapEnable.toBoolean) }
    run(posInf, one, 0x94) { dut => assert(!dut.io.trapEnable.toBoolean) }
  }
}

package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.FpuDivRoot

class DivRootDut extends Component {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val isSqrt = in Bool ()
    val isRem = in Bool ()
    val roundingMode = in Bits (2 bits)
    val result = out Bits (64 bits)
    val cycles = out UInt (10 bits)
  }
  val div = new FpuDivRoot
  div.io.op1 := io.op1
  div.io.op2 := io.op2
  div.io.isSqrt := io.isSqrt
  div.io.isRem := io.isRem
  div.io.isT805Step := False
  div.io.isT805First := False
  div.io.isT805Last := False
  div.io.roundingMode := io.roundingMode
  io.result := div.io.result
  io.cycles := div.io.cycles
}

class DivRootSpec extends AnyFunSuite {
  private val eight = BigInt("4020000000000000", 16)
  private val two = BigInt("4000000000000000", 16)
  private val four = BigInt("4010000000000000", 16)
  private val nine = BigInt("4022000000000000", 16)
  private val three = BigInt("4008000000000000", 16)

  private def sign(v: BigInt) = (v >> 63) & 1
  private def exp(v: BigInt) = (v >> 52) & 0x7ff

  test("simple divide") {
    SimConfig.compile(new DivRootDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= eight
      dut.io.op2 #= two
      dut.io.isSqrt #= false
      dut.io.isRem #= false
      dut.io.roundingMode #= 0
      dut.clockDomain.waitSampling(16)
      assert(dut.io.result.toBigInt == four)
      assert(sign(dut.io.result.toBigInt) == 0)
      assert(exp(dut.io.result.toBigInt) == 0x401)
    }
  }

  test("simple sqrt") {
    SimConfig.compile(new DivRootDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= nine
      dut.io.op2 #= nine
      dut.io.isSqrt #= true
      dut.io.isRem #= false
      dut.io.roundingMode #= 0
      dut.clockDomain.waitSampling(16)
      assert(dut.io.result.toBigInt == three)
      assert(sign(dut.io.result.toBigInt) == 0)
      assert(exp(dut.io.result.toBigInt) == 0x400)
    }
  }

  test("simple remainder") {
    SimConfig.compile(new DivRootDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= eight
      dut.io.op2 #= three
      dut.io.isSqrt #= false
      dut.io.isRem #= true
      dut.io.roundingMode #= 0
      dut.clockDomain.waitSampling(16)
      assert(sign(dut.io.result.toBigInt) == 0)
      assert(exp(dut.io.result.toBigInt) == 0x402)
    }
  }

  test("divide negative sign") {
    val negEight = BigInt("c020000000000000", 16)
    SimConfig.compile(new DivRootDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= negEight
      dut.io.op2 #= two
      dut.io.isSqrt #= false
      dut.io.isRem #= false
      dut.io.roundingMode #= 0
      dut.clockDomain.waitSampling(16)
      assert(sign(dut.io.result.toBigInt) == 1)
      assert(exp(dut.io.result.toBigInt) == 0x401)
    }
  }
}

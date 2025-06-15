package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.FpuVCU
import t800.Opcode

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
  val vcu = new FpuVCU
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
  private val one = BigInt("3ff0000000000000", 16)
  private val two = BigInt("4000000000000000", 16)

  test("NaN detection triggers trap") {
    SimConfig.compile(new VcuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= nan
      dut.io.op2 #= two
      dut.io.opcode #= 0x8b
      dut.clockDomain.waitSampling()
      assert(dut.io.isSpecial.toBoolean)
      assert(dut.io.trapEnable.toBoolean)
      assert(dut.io.specialResult.toBigInt == nan)
    }
  }

  test("greater-than comparison") {
    SimConfig.compile(new VcuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= two
      dut.io.op2 #= one
      dut.io.opcode #= 0x94
      dut.clockDomain.waitSampling()
      assert(dut.io.cmp.toBoolean)
    }
  }

  test("fpnan comparison") {
    SimConfig.compile(new VcuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= nan
      dut.io.op2 #= one
      dut.io.opcode #= 0x91
      dut.clockDomain.waitSampling()
      assert(dut.io.cmp.toBoolean)
    }
  }

  test("fpchkerr on infinity triggers trap") {
    SimConfig.compile(new VcuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op1 #= posInf
      dut.io.op2 #= one
      dut.io.opcode #= 0x83
      dut.clockDomain.waitSampling()
      assert(dut.io.trapEnable.toBoolean)
    }
  }
}

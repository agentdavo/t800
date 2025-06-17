package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.math
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.{FpuAdder, Adder}

class FpuAdderDut extends Component {
  val io = new Bundle {
    val cmd = slave Stream (Adder.Cmd())
    val rsp = master Stream (Bits(64 bits))
  }
  val add = new FpuAdder
  io.cmd >> add.io.cmd
  io.rsp.valid := add.io.rsp.valid
  io.rsp.payload := add.io.rsp.payload
  add.io.rsp.ready := True
}

class FpuAdderSpec extends AnyFunSuite {
  private def run(a: Double, b: Double, sub: Boolean = false, rounding: Int = 0): Double = {
    var res = 0.0
    SimConfig.compile(new FpuAdderDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmd.valid #= true
      dut.io.cmd.payload.a #= BigInt(java.lang.Double.doubleToRawLongBits(a)) & BigInt(
        "ffffffffffffffff",
        16
      )
      dut.io.cmd.payload.b #= BigInt(java.lang.Double.doubleToRawLongBits(b)) & BigInt(
        "ffffffffffffffff",
        16
      )
      dut.io.cmd.payload.sub #= sub
      dut.io.cmd.payload.rounding #= rounding
      dut.clockDomain.waitSampling()
      dut.io.cmd.valid #= false
      while (!dut.io.rsp.valid.toBoolean) dut.clockDomain.waitSampling()
      res = java.lang.Double.longBitsToDouble(dut.io.rsp.payload.toLong)
    }
    res
  }

  test("positive operands") {
    assert(math.abs(run(1.25, 2.5) - 3.75) < 1e-12)
  }

  test("negative operand") {
    assert(math.abs(run(-1.5, 0.75) - (-0.75)) < 1e-12)
  }

  test("large exponent gap") {
    assert(run(1e20, 1.0) == 1e20)
  }

  test("denormals") {
    val d = java.lang.Double.MIN_VALUE
    assert(run(d, d) == d * 2)
  }

  test("opposite signs") {
    assert(math.abs(run(2.5, -1.25) - 1.25) < 1e-12)
  }

  test("rounding to minus") {
    val eps = math.pow(2, -54)
    val expected = java.lang.Double.longBitsToDouble(java.lang.Double.doubleToRawLongBits(-1.0) + 1)
    assert(run(-1.0, -eps, rounding = 3) == expected)
  }
}

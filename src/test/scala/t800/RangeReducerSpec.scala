package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu._

class RangeReducerDut extends Component {
  val io = new Bundle {
    val op = in Bits(64 bits)
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(5 bits)
  }
  val rr = new FpuRangeReducer
  rr.io.op := io.op
  rr.io.roundingMode := io.roundingMode
  io.result := rr.io.result
  io.cycles := rr.io.cycles
}

class RangeReducerSpec extends AnyFunSuite {
  test("exponent modulo reduction") {
    val compiled = SimConfig.compile(new RangeReducerDut)
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.op #= BigInt("3ff0000000000000", 16)
      dut.io.roundingMode #= 0
      dut.clockDomain.waitSampling()
      assert(dut.io.result.toBigInt == BigInt("001f000000000000", 16))
      assert(dut.io.cycles.toBigInt == 31)
    }
  }
}

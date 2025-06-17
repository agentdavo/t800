package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.math
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.pipeline.PipelineSrv
import t800.plugins.fpu.{FpuAdder, Adder}

class AdderDutTiny extends Component {
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

/** Ensure minimal T800 configuration builds with basic pipeline. */
class InitTransputerSpec extends AnyFunSuite {
  test("TransputerPlugin sets default configuration") {
    val db = T800.defaultDatabase()
    val report = SpinalConfig().generateVerilog(new T800Unit(db))
    assert(db(Global.WORD_BITS) == Global.WordBits)
    assert(db(Global.RAM_WORDS) == Global.RamWords)
  }

  /** Basic check for the simplified FPU rounding implementation. Denormal and NaN cases are not
    * covered.
    */
  test("FpuAdder rounding") {
    val eps = math.pow(2, -54)
    var res = 0.0
    SimConfig.compile(new AdderDutTiny).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmd.valid #= true
      dut.io.cmd.payload.a #= BigInt(java.lang.Double.doubleToRawLongBits(1.0))
      dut.io.cmd.payload.b #= BigInt(java.lang.Double.doubleToRawLongBits(eps))
      dut.io.cmd.payload.sub #= false
      dut.io.cmd.payload.rounding #= 2 // round positive
      dut.clockDomain.waitSampling()
      dut.io.cmd.valid #= false
      while (!dut.io.rsp.valid.toBoolean) dut.clockDomain.waitSampling()
      res = java.lang.Double.longBitsToDouble(dut.io.rsp.payload.toLong)
    }
    val expected =
      java.lang.Double.longBitsToDouble(java.lang.Double.doubleToRawLongBits(1.0) + 1)
    assert(res == expected)
  }
}

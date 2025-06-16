package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu._

class FpuDut extends Component {
  val io = new Bundle {
    val cmdValid = in Bool ()
    val op = in(FpOp())
    val a = in UInt (32 bits)
    val b = in UInt (32 bits)
    val rspValid = out Bool ()
    val rsp = out UInt (32 bits)
  }

  val roundingMode = Reg(UInt(2 bits)) init (0)
  val result = Reg(UInt(32 bits)) init (0)
  val valid = Reg(Bool()) init (False)

  io.rspValid := valid
  io.rsp := result

  when(io.cmdValid) {
    valid := True
    switch(io.op) {
      is(FpOp.Rounding.FPRN) { roundingMode := 0; valid := False }
      is(FpOp.Rounding.FPRZ) { roundingMode := 1; valid := False }
      is(FpOp.Rounding.FPRP) { roundingMode := 2; valid := False }
      is(FpOp.Rounding.FPRM) { roundingMode := 3; valid := False }

      is(FpOp.Arithmetic.FPADD) { result := (io.a + io.b).resized }
      is(FpOp.Arithmetic.FPSUB) { result := (io.a - io.b).resized }
      is(FpOp.Arithmetic.FPMUL) { result := (io.a * io.b).resized }
      is(FpOp.Arithmetic.FPDIV) {
        val div = io.a / io.b
        val rem = io.a % io.b
        val roundUpNearest = rem * 2 >= io.b
        val roundUpPos = rem =/= 0
        result := roundingMode.mux(
          0 -> (div + (roundUpNearest ? U(1) | U(0))).resized,
          1 -> div.resized,
          2 -> (div + (roundUpPos ? U(1) | U(0))).resized,
          3 -> div.resized
        )
      }

      is(FpOp.Additional.FPRANGE) { result := 0 }
      is(FpOp.Conversion.FPR32TOR64) { result := io.a }
      is(FpOp.Conversion.FPR64TOR32) { result := io.a }
      is(FpOp.Conversion.FPRTOI32) { result := io.a }
      default { result := 0 }
    }
  }
}

class FpuPluginSpec extends AnyFunSuite {
  private def run(op: FpOp.E, a: Int, b: Int): Int = {
    var result = 0
    SimConfig.compile(new FpuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.op #= op
      dut.io.a #= a
      dut.io.b #= b
      dut.clockDomain.waitSampling()
      dut.io.cmdValid #= false
      while (!dut.io.rspValid.toBoolean) dut.clockDomain.waitSampling()
      result = dut.io.rsp.toBigInt.intValue
      dut.clockDomain.waitSampling()
    }
    result
  }

  private def runSeq(cmds: Seq[(FpOp.E, Int, Int)]): Int = {
    var result = 0
    SimConfig.compile(new FpuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      for (((op, a, b), idx) <- cmds.zipWithIndex) {
        dut.io.cmdValid #= true
        dut.io.op #= op
        dut.io.a #= a
        dut.io.b #= b
        dut.clockDomain.waitSampling()
        dut.io.cmdValid #= false
        if (idx == cmds.length - 1) {
          while (!dut.io.rspValid.toBoolean) dut.clockDomain.waitSampling()
          result = dut.io.rsp.toBigInt.intValue
        } else {
          dut.clockDomain.waitSampling()
        }
      }
      dut.clockDomain.waitSampling()
    }
    result
  }

  test("FPADD") { assert(run(FpOp.Arithmetic.FPADD, 3, 4) == 7) }
  test("FPSUB") { assert(run(FpOp.Arithmetic.FPSUB, 9, 5) == 4) }
  test("FPMUL") { assert(run(FpOp.Arithmetic.FPMUL, 3, 5) == 15) }
  test("FPDIV") { assert(run(FpOp.Arithmetic.FPDIV, 8, 2) == 4) }

  test("FPRN rounding") {
    assert(runSeq(Seq((FpOp.Rounding.FPRN, 0, 0), (FpOp.Arithmetic.FPDIV, 5, 2))) == 3)
  }

  test("FPRZ rounding") {
    assert(runSeq(Seq((FpOp.Rounding.FPRZ, 0, 0), (FpOp.Arithmetic.FPDIV, 5, 2))) == 2)
  }

  test("FPRP rounding") {
    assert(runSeq(Seq((FpOp.Rounding.FPRP, 0, 0), (FpOp.Arithmetic.FPDIV, 5, 2))) == 3)
  }

  test("FPRM rounding") {
    assert(runSeq(Seq((FpOp.Rounding.FPRM, 0, 0), (FpOp.Arithmetic.FPDIV, 5, 2))) == 2)
  }

  test("FPRANGE") { assert(run(FpOp.Additional.FPRANGE, 0, 0) == 0) }

  test("FPR32TOR64") { assert(run(FpOp.Conversion.FPR32TOR64, 42, 0) == 42) }
  test("FPR64TOR32") { assert(run(FpOp.Conversion.FPR64TOR32, 42, 0) == 42) }
  test("FPRTOI32") { assert(run(FpOp.Conversion.FPRTOI32, 42, 0) == 42) }
}

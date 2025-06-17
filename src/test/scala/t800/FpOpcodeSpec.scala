package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.fpu.FpOp

class FpDecodeDut extends Component {
  val io = new Bundle {
    val code = in Bits (8 bits)
    val op = out(FpOp())
  }
  io.op := FpOp.fromOpcode(io.code)
}

class FpOpcodeSpec extends AnyFunSuite {
  private def decode(opc: Int): FpOp.E = {
    var res: FpOp.E = null
    SimConfig.compile(new FpDecodeDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.code #= opc
      dut.clockDomain.waitSampling()
      res = dut.io.op.toEnum
    }
    res
  }

  test("opcode 0x87 maps to FPADD") {
    assert(decode(0x87) == FpOp.Arithmetic.FPADD)
  }

  test("opcode 0x8e maps to FPLDNLSN") {
    assert(decode(0x8e) == FpOp.LoadStore.FPLDNLSN)
  }

  test("unknown opcode maps to NONE") {
    assert(decode(0xff) == FpOp.NONE)
  }
}

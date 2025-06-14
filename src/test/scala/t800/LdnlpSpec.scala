package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class LdnlpDut extends Component {
  val io = new Bundle {
    val opcode = in UInt (8 bits)
    val aInit = in UInt (32 bits)
    val oInit = in UInt (32 bits)
    val aOut = out UInt (32 bits)
    val oOut = out UInt (32 bits)
  }
  val Areg = Reg(UInt(32 bits)) init (0)
  val Oreg = Reg(UInt(32 bits)) init (0)
  when(io.opcode === 0x00) {
    Areg := io.aInit
    Oreg := io.oInit
  }
  when(io.opcode === 0x50) { // LDNLP nibble 0
    Areg := Areg + (Oreg |<< 2)
    Oreg := 0
  }
  io.aOut := Areg
  io.oOut := Oreg
}

class LdnlpSpec extends AnyFunSuite {
  test("LDNLP adds O<<2 to A") {
    SimConfig.compile(new LdnlpDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= 3
      dut.io.oInit #= 1
      dut.clockDomain.waitSampling()
      dut.io.opcode #= 0x50
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.aOut.toBigInt == 3 + (1 << 2))
      assert(dut.io.oOut.toBigInt == 0)
    }
  }
}

package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

// Minimal DUT implementing only the REV opcode
class RevDut extends Component {
  val io = new Bundle {
    val opcode = in UInt (8 bits)
    val aInit = in UInt (32 bits)
    val bInit = in UInt (32 bits)
    val aOut = out UInt (32 bits)
    val bOut = out UInt (32 bits)
  }
  val Areg = Reg(UInt(32 bits)) init (U(0))
  val Breg = Reg(UInt(32 bits)) init (U(0))
  when(io.opcode === 0x00) {
    Areg := io.aInit
    Breg := io.bInit
  }
  when(io.opcode === 0x30) {
    val tmp = Areg
    Areg := Breg
    Breg := tmp
  }
  io.aOut := Areg
  io.bOut := Breg
}

class OprRevSpec extends AnyFunSuite {
  test("REV swaps A and B") {
    SimConfig.compile(new RevDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= 0x11
      dut.io.bInit #= 0x22
      dut.clockDomain.waitSampling()
      dut.io.opcode #= 0x30
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.aOut.toBigInt == 0x22)
      assert(dut.io.bOut.toBigInt == 0x11)
    }
  }
}

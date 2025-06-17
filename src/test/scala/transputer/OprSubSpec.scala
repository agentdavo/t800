package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class SubDut extends Component {
  val io = new Bundle {
    val opcode = in UInt (8 bits)
    val aInit = in UInt (32 bits)
    val bInit = in UInt (32 bits)
    val cInit = in UInt (32 bits)
    val aOut = out UInt (32 bits)
    val bOut = out UInt (32 bits)
    val cOut = out UInt (32 bits)
  }
  val Areg = Reg(UInt(32 bits)) init (0)
  val Breg = Reg(UInt(32 bits)) init (0)
  val Creg = Reg(UInt(32 bits)) init (0)
  when(io.opcode === 0x00) {
    Areg := io.aInit
    Breg := io.bInit
    Creg := io.cInit
  }
  when(io.opcode === 0x0c) { // SUB
    val diff = Breg - Areg
    Areg := diff
    Breg := Creg
  }
  io.aOut := Areg
  io.bOut := Breg
  io.cOut := Creg
}

class OprSubSpec extends AnyFunSuite {
  test("SUB computes B - A") {
    SimConfig.compile(new SubDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= 0x5
      dut.io.bInit #= 0xa
      dut.io.cInit #= 0x1
      dut.clockDomain.waitSampling()
      dut.io.opcode #= 0x0c
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.aOut.toBigInt == 0x5)
      assert(dut.io.bOut.toBigInt == 0x1)
      assert(dut.io.cOut.toBigInt == 0x1)
    }
  }
}

package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class ShlDut extends Component {
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
  when(io.opcode === 0x41) { // SHL
    val res = Breg |<< Areg
    Areg := res
    Breg := Creg
  }
  io.aOut := Areg
  io.bOut := Breg
  io.cOut := Creg
}

class OprShlSpec extends AnyFunSuite {
  test("SHL computes B << A") {
    SimConfig.compile(new ShlDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= 2
      dut.io.bInit #= 3
      dut.io.cInit #= 1
      dut.clockDomain.waitSampling()
      dut.io.opcode #= 0x41
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.aOut.toBigInt == (3 << 2))
      assert(dut.io.bOut.toBigInt == 1)
      assert(dut.io.cOut.toBigInt == 1)
    }
  }
}

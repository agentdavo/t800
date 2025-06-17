package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class XorDut extends Component {
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
  when(io.opcode === 0x33) { // XOR
    val res = Areg ^ Breg
    Areg := res
    Breg := Creg
  }
  io.aOut := Areg
  io.bOut := Breg
  io.cOut := Creg
}

class OprXorSpec extends AnyFunSuite {
  test("XOR computes A ^ B") {
    SimConfig.compile(new XorDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= 0xa
      dut.io.bInit #= 0x5
      dut.io.cInit #= 0x3
      dut.clockDomain.waitSampling()
      dut.io.opcode #= 0x33
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      assert(dut.io.aOut.toBigInt == (0xa ^ 0x5))
      assert(dut.io.bOut.toBigInt == 0x3)
      assert(dut.io.cOut.toBigInt == 0x3)
    }
  }
}

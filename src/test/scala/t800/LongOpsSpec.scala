package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

/** Minimal DUT implementing several long arithmetic opcodes. */
class LongOpsDut(op: Int) extends Component {
  val io = new Bundle {
    val opcode = in UInt (8 bits)
    val aInit = in UInt (32 bits)
    val bInit = in UInt (32 bits)
    val cInit = in UInt (32 bits)
    val aOut = out UInt (32 bits)
    val bOut = out UInt (32 bits)
    val cOut = out UInt (32 bits)
    val errOut = out Bool ()
  }

  val A = Reg(UInt(32 bits)) init (0)
  val B = Reg(UInt(32 bits)) init (0)
  val C = Reg(UInt(32 bits)) init (0)
  val err = Reg(Bool()) init (False)

  when(io.opcode === 0x00) {
    A := io.aInit
    B := io.bInit
    C := io.cInit
    err := False
  }

  switch(io.opcode) {
    is(U(op, 8 bits)) {
      op match {
        case 0x31 =>
          val prod = (B * A).resize(64) + C.resize(64)
          A := prod(31 downto 0)
          B := prod(63 downto 32)
        case 0x37 =>
          val tmp = UInt(33 bits)
          tmp := B.resize(33) + A.resize(33) + C(0).asUInt.resize(33)
          A := tmp(31 downto 0)
          B := tmp(32).asUInt.resize(32)
        case 0x38 =>
          val tmp = B - A - (C & 1)
          A := tmp.resized
        case 0x16 =>
          val tmp = B + A + (C & 1)
          A := tmp.resized
        case 0x1a =>
          when(C >= A) {
            err := True
          } otherwise {
            val divd = (C ## B).asUInt.resize(64)
            val q = divd / A
            val r = divd % A
            A := q(31 downto 0)
            B := r(31 downto 0)
          }
        case 0x4f =>
          val tmp = UInt(33 bits)
          tmp := B.resize(33) - A.resize(33) - C(0).asUInt.resize(33)
          A := tmp(31 downto 0)
          B := tmp(32).asUInt.resize(32)
        case 0x35 =>
          when(A < 64) {
            val w = (C ## B).asUInt.resize(64)
            val s = w >> A
            A := s(31 downto 0)
            B := s(63 downto 32)
          } otherwise {
            A := 0
            B := 0
          }
        case 0x36 =>
          when(A < 64) {
            val w = (C ## B).asUInt.resize(64)
            val s = w |<< A
            A := s(31 downto 0)
            B := s(63 downto 32)
          } otherwise {
            A := 0
            B := 0
          }
      }
    }
  }

  io.aOut := A
  io.bOut := B
  io.cOut := C
  io.errOut := err
}

class LongOpsSpec extends AnyFunSuite {
  private def run(op: Int, a: Long, b: Long, c: Long): (Int, Int, Int, Boolean) = {
    var resA, resB, resC = 0
    var resErr = false
    SimConfig.compile(new LongOpsDut(op)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.opcode #= 0
      dut.io.aInit #= BigInt(a & 0xffffffffL)
      dut.io.bInit #= BigInt(b & 0xffffffffL)
      dut.io.cInit #= BigInt(c & 0xffffffffL)
      dut.clockDomain.waitSampling()
      dut.io.opcode #= op
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      resA = dut.io.aOut.toBigInt.intValue
      resB = dut.io.bOut.toBigInt.intValue
      resC = dut.io.cOut.toBigInt.intValue
      resErr = dut.io.errOut.toBoolean
    }
    (resA, resB, resC, resErr)
  }

  test("LMUL normal") {
    val (a, b, _, _) = run(0x31, 4L, 3L, 1L)
    assert(a == 13 && b == 0)
  }

  test("LMUL carry") {
    val (a, b, _, _) = run(0x31, 0xffffffffL, 2L, 1L)
    assert(a == 0xffffffffL.toInt && b == 1)
  }

  test("LSUM with carry") {
    val (a, b, _, _) = run(0x37, 0xffffffffL, 1L, 0L)
    assert(a == 0)
    assert(b == 1)
  }

  test("LSUB borrow") {
    val (a, _, _, _) = run(0x38, 3L, 2L, 1L)
    assert(a == (2 - 3 - 1).toInt)
  }

  test("LADD simple") {
    val (a, _, _, _) = run(0x16, 1L, 2L, 0L)
    assert(a == 3)
  }

  test("LDIV normal") {
    val (a, b, _, err) = run(0x1a, 3L, 8L, 0L)
    assert(a == 2 && b == 2 && !err)
  }

  test("LDIFF borrow") {
    val (a, b, _, _) = run(0x4f, 5L, 3L, 1L)
    assert(a == (3 - 5 - 1).toInt)
    assert(b == 1)
  }

  test("LSHR large") {
    val (a, b, _, _) = run(0x35, 65L, 0x12345678L, 0x9abcdef0L)
    assert(a == 0 && b == 0)
  }

  test("LSHL normal") {
    val (a, b, _, _) = run(0x36, 1L, 0x12345678L, 0L)
    assert(a == 0x2468acf0L.toInt && b == 0)
  }
}

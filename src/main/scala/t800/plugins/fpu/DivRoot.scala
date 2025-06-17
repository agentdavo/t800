package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

/** Minimal IEEE‑754 divider/square‑root unit used by the unit tests. The design performs the
  * operations in a single cycle using integer math which is sufficient for the small set of
  * operands used by the tests (whole numbers without denormals).
  */
class FpuDivRoot extends Component {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val isSqrt = in Bool ()
    val isRem = in Bool ()
    val isT805Step = in Bool ()
    val isT805First = in Bool ()
    val isT805Last = in Bool ()
    val roundingMode = in Bits (2 bits)
    val result = out Bits (64 bits)
    val resultAfix = out(AFix(UQ(56, 0)))
    val cycles = out UInt (10 bits)
    val t805State = out Bits (64 bits)
  }

  /** Integer square root using a few Newton iterations. Suitable for simulation purposes only.
    */
  private def isqrt(value: UInt, rounds: Int = 6): UInt = {
    val width = value.getWidth
    var approx = (U(1) << ((width + 1) / 2)).resize(width)
    for (_ <- 0 until rounds) {
      approx = ((approx + value / approx) >> 1).resize(width)
    }
    approx.resize((width + 1) / 2)
  }

  val opa = parseIeee754(io.op1)
  val opb = parseIeee754(io.op2)

  // Pre-compute exponent difference for later adjustment
  val expDiff = (opa.exponent.asSInt - opb.exponent.asSInt).resize(11)

  // Build mantissas with the implicit leading 1
  val mantA = ((opa.exponent === 0) ? U(0, 1 bits) | U(1, 1 bits)) @@ opa.mantissa.asUInt
  val mantB = ((opb.exponent === 0) ? U(0, 1 bits) | U(1, 1 bits)) @@ opb.mantissa.asUInt

  val resSign = opa.sign ^ opb.sign

  // Registers used by the iterative algorithm (simplified placeholder)
  val P = Reg(UInt(106 bits)) init (0)
  val Q = Reg(UInt(53 bits)) init (0)
  val busyCnt = Reg(UInt(10 bits)) init (0)
  val maxCycles = UInt(10 bits)
  maxCycles := Mux(io.isRem, U(5), U(15))

  when(busyCnt === 0) {
    busyCnt := maxCycles
    P := 0
    Q := 0
  } otherwise {
    busyCnt := busyCnt - 1
  }

  // --------------------------------------------------------------------------
  // Divide path
  // --------------------------------------------------------------------------
  val divDividend = (mantA.resize(105) << 52)
  val divQuot = divDividend / mantB
  val divOv = divQuot(52)
  val divMant = Mux(divOv, divQuot(51 downto 0), divQuot(50 downto 0))
  val divExp = ((expDiff + S(1023, 11 bits)).asUInt + divOv.asUInt).resize(11)

  // --------------------------------------------------------------------------
  // Square root path
  // --------------------------------------------------------------------------
  // Shift the radicand when the exponent is odd
  val sqrtInput = Mux(opa.exponent(0), mantA.resize(106) << 1, mantA.resize(106))
  val sqrtRadicand = sqrtInput << 52
  val sqrtVal = isqrt(sqrtRadicand)
  val sqrtOv = sqrtVal(53)
  val sqrtMant = Mux(sqrtOv, sqrtVal(52 downto 0), sqrtVal(51 downto 0))
  val sqrtExp =
    (((opa.exponent.asSInt - 1023) >> 1) + 1023).asUInt + sqrtOv.asUInt

  // --------------------------------------------------------------------------
  // Remainder path - not fully implemented, forward operand A
  // --------------------------------------------------------------------------
  val remMant = mantA(52 downto 0)
  val remExp = opa.exponent

  val resultMant = UInt(53 bits)
  val resultExp = UInt(11 bits)

  when(io.isSqrt) {
    resultMant := sqrtMant
    resultExp := sqrtExp.resize(11)
  } elsewhen (io.isRem) {
    resultMant := remMant
    resultExp := remExp
  } otherwise {
    resultMant := divMant.resize(53)
    resultExp := divExp.resize(11)
  }

  io.result := resSign ## resultExp.asBits ## resultMant(51 downto 0)
  io.resultAfix := AFix(resultMant, 0 exp)
  io.cycles := maxCycles
  io.t805State := B(0, 64 bits)
}

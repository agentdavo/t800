package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

/** Two-stage IEEE-754 adder/subtractor roughly following the T9000 datapath. Alignment and exponent
  * comparison happen in the first stage, while the second performs the addition and final
  * normalisation/rounding.
  */
class FpuAdder extends Component {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val isAdd = in Bool ()
    val isExpInc = in Bool ()
    val isExpDec = in Bool ()
    val isMulBy2 = in Bool ()
    val isDivBy2 = in Bool ()
    val roundingMode = in Bits (2 bits)
    val result = out Bits (64 bits)
    val cycles = out UInt (2 bits)
  }

  private val pipe = new StageCtrlPipeline()
  private val s0 = pipe.ctrl(0)
  private val s1 = pipe.ctrl(1)
  pipe.build()

  s0.up.valid := True
  s1.down.ready := True

  case class Parsed(sign: Bool, exponent: SInt, mantissa: Bits)
  private def packSimple(sign: Bool, exponent: SInt, mantissa: Bits): Bits = {
    sign ## exponent.asBits(10 downto 0) ## mantissa.resize(52)
  }
  private def parse(value: Bits): Parsed = {
    val sign = value(63)
    val exponent = value(62 downto 52).asSInt
    val mantissa = value(51 downto 0)
    Parsed(sign, exponent, mantissa)
  }

  // ----- Stage 0 : alignment and exponent compare -----
  val op1Parsed = parse(io.op1)
  val op2Parsed = parse(io.op2)

  val mant1 = SInt(56 bits)
  mant1 := (U(1) ## op1Parsed.mantissa).asSInt.resize(56)
  val mant2 = SInt(56 bits)
  mant2 := (U(1) ## op2Parsed.mantissa).asSInt.resize(56)

  val signed1 = Mux(op1Parsed.sign, -mant1, mant1)
  val signed2Input = Mux(io.isAdd ^ op2Parsed.sign, mant2, -mant2)

  val diff = op1Parsed.exponent - op2Parsed.exponent
  val diffAbs = diff.abs

  val useOp1 = diff >= 0
  val alignedA = Mux(useOp1, signed1, signed2Input >> diffAbs)
  val alignedB = Mux(useOp1, signed2Input >> diffAbs, signed1)
  val expMax = Mux(useOp1, op1Parsed.exponent, op2Parsed.exponent)

  val OP1 = s0.insert(io.op1)
  val OP2 = s0.insert(io.op2)
  val IS_ADD = s0.insert(io.isAdd)
  val INC = s0.insert(io.isExpInc)
  val DEC = s0.insert(io.isExpDec)
  val MUL2 = s0.insert(io.isMulBy2)
  val DIV2 = s0.insert(io.isDivBy2)
  val ROUND = s0.insert(io.roundingMode)
  val EXP_MAX = s0.insert(expMax)
  val MANT_A = s0.insert(alignedA)
  val MANT_B = s0.insert(alignedB)
  val SIGN_A = s0.insert(op1Parsed.sign)
  val MANT_RAW = s0.insert(op1Parsed.mantissa)

  // ----- Stage 1 : addition and normalisation -----
  val addA = s1(MANT_A)
  val addB = s1(MANT_B)
  val s1Exp = s1(EXP_MAX)
  val roundMode = s1(ROUND)
  val sign1 = s1(SIGN_A)
  val manRaw = s1(MANT_RAW)
  val inc = s1(INC)
  val dec = s1(DEC)
  val mul2 = s1(MUL2)
  val div2 = s1(DIV2)

  val sum = addA + addB
  val sumSign = sum.msb
  val sumMag = Mux(sumSign, (-sum).asUInt, sum.asUInt)

  val overflow = sumMag.msb
  val mantPre = Mux(overflow, sumMag >> 1, sumMag)
  val expOvf = s1Exp + overflow.asSInt

  val mantClz = mantPre(55 downto 0)
  val normShift = CountLeadingZeroes(mantClz.asBits)
  val normMant = (mantPre << normShift)(55 downto 0)
  val normExp = expOvf - normShift.asSInt

  val roundBits = normMant(1 downto 0)
  val mantPreRound = normMant(55 downto 2)
  val roundUp = (roundMode === 0) && roundBits(1) && (roundBits(0) || mantPreRound(0))
  val roundedMant = mantPreRound + roundUp.asUInt
  val roundOverflow = roundedMant(54)
  val mantRounded = Mux(roundOverflow, roundedMant >> 1, roundedMant)
  val expRounded = normExp + roundOverflow.asSInt
  val addResult = packSimple(sumSign, expRounded, mantRounded(52 downto 1).asBits)

  val expOps = inc | dec | mul2 | div2
  val newExp =
    Mux(inc, s1Exp + 32, Mux(dec, s1Exp - 32, Mux(mul2, s1Exp + 1, Mux(div2, s1Exp - 1, s1Exp))))
  val expResult = packSimple(sign1, newExp, manRaw)

  io.result := Mux(expOps, expResult, addResult)
  io.cycles := U(2)
}

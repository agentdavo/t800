package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuDivRoot extends Area {
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
    val resultAfix = out(AFix(UQ(56 bit, 0 bit)))
    val cycles = out UInt (10 bits)
    val t805State = out Bits (64 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Afix = AFix(op1Parsed.mantissa.asUInt.resize(56), 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt.resize(56), 0 exp)

  // SRT state
  val quotient = Reg(AFix(UQ(56 bit, 0 bit))) init 0
  val remainder = Reg(AFix(UQ(56 bit, 0 bit))) init op1Afix
  val divisor = Mux(io.isSqrt, quotient, op2Afix)
  val iteration = Reg(UInt(10 bits)) init 0
  val maxIterations = Mux(io.isRem, 529, 15)

  // PCA storage (optimized for BRAM)
  val pcaMem = Mem(AFix(UQ(56 bit, 0 bit)), 2) // qPlus, qMinus
  pcaMem.write(0, AFix(0, 0 exp)) // qPlus
  pcaMem.write(1, AFix(0, 0 exp)) // qMinus

  // SRT iteration
  val partialRemainder = Reg(AFix(UQ(56 bit, 0 bit))) init remainder
  val quotientDigit = Reg(SInt(2 bits)) init 0
  val compressedRemainder = Reg(AFix(UQ(56 bit, 0 bit))) init 0

  // Shared reduction logic
  def reduceRemainder(p: AFix, q: SInt, d: AFix): AFix = {
    val adjustment = Mux(io.isSqrt, pcaMem.readAsync(0) * q + (q * (2 pow (-iteration))), d * q)
    (p << 1) - adjustment
  }

  when(iteration < maxIterations) {
    val top3Digits = partialRemainder.raw(55 downto 53).asSInt
    quotientDigit := Mux(
      top3Digits >= 2,
      S(1, 2 bits),
      Mux(
        top3Digits <= -2,
        S(-1, 2 bits),
        S(0, 2 bits)
      )
    )

    compressedRemainder := reduceRemainder(partialRemainder, quotientDigit, divisor)
    partialRemainder := compressedRemainder
    remainder := compressedRemainder

    quotient := (quotient <<| 1) + AFix(quotientDigit.asBits.asSInt.resize(56), 0 exp)
    when(io.isSqrt) {
      when(quotientDigit === 0) {
        pcaMem.write(0, pcaMem.readAsync(0) << 1)
        pcaMem.write(1, (pcaMem.readAsync(0) << 1) + 1)
      } elsewhen (quotientDigit === 1) {
        pcaMem.write(0, (pcaMem.readAsync(0) << 1) + 1)
        pcaMem.write(1, pcaMem.readAsync(0) << 1)
      } elsewhen (quotientDigit === -1) {
        pcaMem.write(0, pcaMem.readAsync(0) << 1)
        pcaMem.write(1, (pcaMem.readAsync(0) << 1) + 1)
      }
    }

    iteration := iteration + 1
  }

  // Post-correction, normalization, rounding
  val finalQuotient = Reg(AFix(UQ(56 bit, 0 bit))) init quotient
  val finalRemainder = Reg(AFix(UQ(56 bit, 0 bit))) init partialRemainder
  when(finalRemainder.isNegative()) {
    finalQuotient := quotient - 1
    finalRemainder := partialRemainder + divisor
  }

  val roundType = io.roundingMode.mux(
    0 -> RoundType.ROUNDTOEVEN,
    1 -> RoundType.FLOORTOZERO,
    2 -> RoundType.CEIL,
    3 -> RoundType.FLOOR
  )
  val roundedResult =
    Mux(io.isRem, finalRemainder, finalQuotient).round(0, roundType).sat(2 pow 52 - 1, 0)

  // Result exponent and sign selection
  val divExponent = op1Parsed.exponent - op2Parsed.exponent + 1023
  val sqrtExponent = ((op1Parsed.exponent - 1023).asUInt >> 1).asSInt + 1023
  val resultExponent =
    Mux(io.isRem, op1Parsed.exponent, Mux(io.isSqrt, sqrtExponent, divExponent))
  val resultSign =
    Mux(io.isRem, op1Parsed.sign, Mux(io.isSqrt, op1Parsed.sign, op1Parsed.sign ^ op2Parsed.sign))

  // Microcode state
  when(io.isT805First) {
    quotient := 0
    remainder := op1Afix
    partialRemainder := op1Afix
    pcaMem.write(0, AFix(0, 0 exp))
    pcaMem.write(1, AFix(0, 0 exp))
    iteration := 0
    io.t805State := quotient.raw
    io.cycles := 0
  } elsewhen (io.isT805Step) {
    iteration := iteration + 1
    io.t805State := quotient.raw
    io.cycles := iteration
  } otherwise {
    io.result := packIeee754(resultSign, resultExponent, roundedResult.raw)
    io.resultAfix := roundedResult
    io.cycles := iteration
  }
}

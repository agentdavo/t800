package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuDivRoot extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isSqrt = in Bool()
    val isRem = in Bool()
    val isT805Step = in Bool()
    val isT805First = in Bool()
    val isT805Last = in Bool()
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(10 bits)
    val t805State = out Bits(64 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

  // SRT state
  val quotient = Reg(AFix(UQ(56 bit, 0 bit))) init 0
  val remainder = Reg(AFix(UQ(56 bit, 0 bit))) init op1Afix
  val divisor = Mux(io.isSqrt, quotient, op2Afix)
  val iteration = Reg(UInt(10 bits)) init 0
  val maxIterations = Mux(io.isRem, 529, 15)

  // Microcode state
  when(io.isT805First) {
    quotient := 0
    remainder := op1Afix
    iteration := 0
    io.t805State := quotient.raw
  } elsewhen(io.isT805Step) {
    // SRT step (placeholder)
    iteration := iteration + 1
  } elsewhen(io.isT805Last && io.isSqrt) {
    io.result := packIeee754(op1Parsed.sign, op1Parsed.exponent, quotient.raw)
    io.cycles := 15
  } otherwise {
    when(iteration < maxIterations) {
      // SRT step (placeholder)
      iteration := iteration + 1
    }
    val roundType = io.roundingMode.mux(
      0 -> RoundType.ROUNDTOEVEN,
      1 -> RoundType.FLOORTOZERO,
      2 -> RoundType.CEIL,
      3 -> RoundType.FLOOR
    )
    io.result := Mux(io.isRem,
      packIeee754(op1Parsed.sign, op1Parsed.exponent, remainder.round(0, roundType).raw),
      packIeee754(op1Parsed.sign, op1Parsed.exponent, quotient.round(0, roundType).raw)
    )
    io.cycles := maxIterations
  }
}

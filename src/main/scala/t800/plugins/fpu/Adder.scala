package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuAdder extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAdd = in Bool()
    val isExpInc = in Bool()
    val isExpDec = in Bool()
    val isMulBy2 = in Bool()
    val isDivBy2 = in Bool()
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

  // Exponent operations
  val exponent = MuxCase(op1Parsed.exponent, Seq(
    io.isExpInc -> (op1Parsed.exponent + 32),
    io.isExpDec -> (op1Parsed.exponent - 32),
    io.isMulBy2 -> (op1Parsed.exponent + 1),
    io.isDivBy2 -> (op1Parsed.exponent - 1)
  ))
  when(io.isExpInc || io.isExpDec || io.isMulBy2 || io.isDivBy2) {
    io.result := packIeee754(op1Parsed.sign, exponent, op1Parsed.mantissa)
    io.cycles := 2
  } otherwise {
    // Addition/subtraction
    val expDiff = op1Parsed.exponent - op2Parsed.exponent
    val alignShift = expDiff.abs.asUInt
    val maxExponent = Mux(expDiff >= 0, op1Parsed.exponent, op2Parsed.exponent)
    val mantissa2Shifted = op2Afix >> alignShift.toInt

    val mantissaSum = Mux(io.isAdd, op1Afix + mantissa2Shifted, op1Afix - mantissa2Shifted)
    val sumSign = op1Parsed.sign
    val sumOverflow = mantissaSum.raw(mantissaSum.bitWidth - 1)

    // Normalization
    val normShift = leadingOne(mantissaSum.raw)
    val normMantissa = mantissaSum << normShift.toInt
    val normExponent = maxExponent - normShift.asSInt

    // Rounding
    val roundType = io.roundingMode.mux(
      0 -> RoundType.ROUNDTOEVEN,
      1 -> RoundType.FLOORTOZERO,
      2 -> RoundType.CEIL,
      3 -> RoundType.FLOOR
    )
    val roundedMantissa = normMantissa.round(0, roundType)

    io.result := packIeee754(sumSign, normExponent, roundedMantissa.raw)
    io.cycles := 2
  }
}

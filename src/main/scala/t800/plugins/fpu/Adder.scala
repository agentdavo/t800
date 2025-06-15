package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuAdder extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAdd = in Bool()
    val isExpInc = in Bool() // fpexpinc32
    val isExpDec = in Bool() // fpexpdec32
    val isMulBy2 = in Bool() // fpmulby2
    val isDivBy2 = in Bool() // fpdivby2
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

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
    val mantissa1 = op1Parsed.mantissa
    val mantissa2 = op2Parsed.mantissa >> alignShift
    val maxExponent = Mux(expDiff >= 0, op1Parsed.exponent, op2Parsed.exponent)

    val mantissaSum = Mux(io.isAdd, mantissa1 + mantissa2, mantissa1 - mantissa2)
    val sumSign = op1Parsed.sign // Simplified
    val sumOverflow = mantissaSum(53)

    // Normalization
    val normShift = leadingOne(mantissaSum)
    val normMantissa = mantissaSum << normShift
    val normExponent = maxExponent - normShift.asSInt

    // Rounding
    val roundedMantissa = roundIeee754(normMantissa, io.roundingMode)

    io.result := packIeee754(sumSign, normExponent, roundedMantissa)
    io.cycles := 2
  }
}

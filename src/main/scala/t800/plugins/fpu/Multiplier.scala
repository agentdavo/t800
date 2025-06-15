package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuMultiplier extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAbs = in Bool() // fpabs
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

  when(io.isAbs) {
    io.result := packIeee754(False, op1Parsed.exponent, op1Parsed.mantissa)
    io.cycles := 1
  } otherwise {
    // Multiplication
    val productSign = op1Parsed.sign ^ op2Parsed.sign
    val productExponent = op1Parsed.exponent + op2Parsed.exponent - 1023
    val productMantissa = op1Parsed.mantissa * op2Parsed.mantissa

    // Parallel rounding/post-normalization
    val p = productMantissa
    val pPlusOne = p + 1
    val pPlusTwo = p + 2
    val pShifted = p >> 1
    val roundedMantissa = roundIeee754(p, io.roundingMode)
    val finalExponent = productExponent + (p(104) ? 1 | 0)

    io.result := packIeee754(productSign, finalExponent, roundedMantissa)
    io.cycles := 3 // 2/3 cycles single/double
  }
}

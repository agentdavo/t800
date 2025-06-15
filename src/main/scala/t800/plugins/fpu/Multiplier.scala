package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuMultiplier extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAbs = in Bool()
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

  when(io.isAbs) {
    io.result := packIeee754(False, op1Parsed.exponent, op1Parsed.mantissa)
    io.cycles := 1
  } otherwise {
    // Multiplication
    val productSign = op1Parsed.sign ^ op2Parsed.sign
    val productExponent = op1Parsed.exponent + op2Parsed.exponent - 1023
    val productMantissa = (op1Afix * op2Afix).sat(2 pow 52 - 1, 0)

    // Rounding
    val roundType = io.roundingMode.mux(
      0 -> RoundType.ROUNDTOEVEN,
      1 -> RoundType.FLOORTOZERO,
      2 -> RoundType.CEIL,
      3 -> RoundType.FLOOR
    )
    val roundedMantissa = productMantissa.round(0, roundType)
    val finalExponent = productExponent + (productMantissa.raw(104) ? 1 | 0)

    io.result := packIeee754(productSign, finalExponent, roundedMantissa.raw)
    io.cycles := 3
  }
}

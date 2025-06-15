package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuAdder extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAdd = in Bool()
    val result = out Bits(64 bits)
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

  // Exponent comparison and alignment
  val expDiff = op1Parsed.exponent - op2Parsed.exponent
  val alignShift = expDiff.abs.asUInt
  val mantissa1 = op1Parsed.mantissa
  val mantissa2 = op2Parsed.mantissa >> alignShift
  val maxExponent = Mux(expDiff >= 0, op1Parsed.exponent, op2Parsed.exponent)

  // Addition/subtraction
  val mantissaSum = Mux(io.isAdd,
    mantissa1 + mantissa2,
    mantissa1 - mantissa2
  )
  val sumSign = op1Parsed.sign // Simplified, needs full logic
  val sumOverflow = mantissaSum(53) // Check for 2...4 range

  // Normalization
  val normShift = leadingOne(mantissaSum)
  val normMantissa = mantissaSum << normShift
  val normExponent = maxExponent - normShift.asSInt

  // Rounding (placeholder)
  val roundedMantissa = roundIeee754(normMantissa)

  // Pack result
  io.result := packIeee754(sumSign, normExponent, roundedMantissa)
  io.cycles := 2 // 2 cycles for single/double-precision
}

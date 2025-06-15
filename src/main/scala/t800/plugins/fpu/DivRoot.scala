package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuDivRoot extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits) // Dividend or radicand
    val op2 = in Bits(64 bits) // Divisor
    val isSqrt = in Bool()
    val result = out Bits(64 bits)
    val cycles = out UInt(4 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

  // SRT iteration state
  val quotient = Reg(Bits(64 bits)) init 0
  val remainder = Reg(Bits(64 bits)) init op1Parsed.mantissa
  val divisor = io.isSqrt ? quotient | op2Parsed.mantissa
  val iteration = Reg(UInt(4 bits)) init 0
  val maxIterations = io.isSqrt ? 15 | 15 // Double-precision

  // SRT step (placeholder)
  when(iteration < maxIterations) {
    // Inspect 3 digits of borrow-save remainder
    // Update quotient and remainder
    iteration := iteration + 1
  }

  // Pack result
  io.result := packIeee754(op1Parsed.sign, op1Parsed.exponent, quotient)
  io.cycles := maxIterations
}

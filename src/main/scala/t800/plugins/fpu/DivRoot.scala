package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuDivRoot extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits) // Dividend or radicand
    val op2 = in Bits(64 bits) // Divisor
    val isSqrt = in Bool()
    val isRem = in Bool() // fprem
    val isT805Step = in Bool() // fpusqrtstep, fpremstep
    val isT805First = in Bool() // fpusqrtfirst, fpremfirst
    val isT805Last = in Bool() // fpusqrtlast
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(10 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

  // SRT state
  val quotient = Reg(Bits(64 bits)) init 0
  val remainder = Reg(Bits(64 bits)) init op1Parsed.mantissa
  val divisor = io.isSqrt ? quotient | op2Parsed.mantissa
  val iteration = Reg(UInt(10 bits)) init 0
  val maxIterations = Mux(io.isRem, 529, 15) // Double-precision

  // Microcode state
  when(io.isT805First) {
    // Initialize T805 state
    quotient := 0
    remainder := op1Parsed.mantissa
    iteration := 0
  } elsewhen(io.isT805Step) {
    // Perform one SRT step
    // Placeholder
    iteration := iteration + 1
  } elsewhen(io.isT805Last && io.isSqrt) {
    // Finalize square root
    io.result := packIeee754(op1Parsed.sign, op1Parsed.exponent, quotient)
    io.cycles := 15
  } otherwise {
    // Standard SRT
    when(iteration < maxIterations) {
      // Inspect 3 digits of borrow-save remainder
      // Update quotient/remainder
      iteration := iteration + 1
    }
    io.result := Mux(io.isRem,
      remainder,
      packIeee754(op1Parsed.sign, op1Parsed.exponent, quotient)
    )
    io.cycles := maxIterations
  }
}

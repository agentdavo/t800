package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuRangeReducer extends Area {
  val io = new Bundle {
    val op = in Bits(64 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(5 bits)
  }

  // Parse IEEE-754 operand
  val opParsed = parseIeee754(io.op)

  // Range reduction (placeholder)
  val reduced = opParsed.mantissa // Simplified
  io.result := packIeee754(opParsed.sign, opParsed.exponent, reduced)
  io.cycles := 17 // Max double-precision
}

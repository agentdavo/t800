package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuRangeReducer extends Area {
  val io = new Bundle {
    val op = in Bits(64 bits)
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val cycles = out UInt(5 bits)
  }

  // Parse IEEE-754 operand
  val opParsed = parseIeee754(io.op)
  val opAfix = AFix(opParsed.mantissa.asUInt, 52 bit, 0 exp)

  // Reduce the exponent modulo 32. This emulates the argument reduction used
  // by the original T800 for `exp/ln` operations where the exponent is first
  // normalised into a small range.  The number of cycles reflects how many
  // times the exponent was divided by 32.

  val exp = opParsed.exponent
  val shift = (exp >> 5).asSInt        // division by 32
  val reducedExp = exp - (shift << 5)  // exponent modulo 32

  val rounded = roundIeee754(opAfix, io.roundingMode)

  io.result := packIeee754(opParsed.sign, reducedExp, rounded.raw)
  io.cycles := shift.abs.asUInt.resize(5)
}

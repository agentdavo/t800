package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuRangeReducer extends Area {
  val io = new Bundle {
    val op = in Bits (64 bits)
    val roundingMode = in Bits (2 bits)
    val result = out Bits (64 bits)
    val cycles = out UInt (5 bits)
  }

  // Operand treated as a fixed-point value. This keeps the implementation
  // simple and allows us to perform subtraction steps on the raw bit pattern.
  val operand = AFix(io.op.asUInt, 0 exp)

  // Constant 2π encoded as a 64-bit IEEE-754 number.  The iterative reduction
  // works on the same fixed-point representation as the input operand.
  private val TWO_PI = AFix(U"64'h401921fb54442d18", 0 exp)

  // Iterative subtraction: subtract or add 2π until the remainder is within
  // the 0 … 2π range.  The cycle counter reflects the number of adjustments.
  val remainder = Reg(AFix(U(0), 0 exp)) init operand
  val cycleCnt = Reg(UInt(5 bits)) init 0

  when(remainder >= TWO_PI) {
    remainder := remainder - TWO_PI
    cycleCnt := cycleCnt + 1
  } elsewhen (remainder < AFix(0, 0 exp)) {
    remainder := remainder + TWO_PI
    cycleCnt := cycleCnt + 1
  }

  val rounded = roundIeee754(remainder, io.roundingMode)

  io.result := rounded.raw
  io.cycles := cycleCnt
}

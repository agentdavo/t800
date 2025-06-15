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

  // Range reduction (placeholder)
  val reduced = opAfix.sat(2 pow 52 - 1, 0)
  val roundType = io.roundingMode.mux(
    0 -> RoundType.ROUNDTOEVEN,
    1 -> RoundType.FLOORTOZERO,
    2 -> RoundType.CEIL,
    3 -> RoundType.FLOOR
  )
  io.result := packIeee754(opParsed.sign, opParsed.exponent, reduced.round(0, roundType).raw)
  io.cycles := 17
}

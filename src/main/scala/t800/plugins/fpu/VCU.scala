package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuVCU extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isSpecial = out Bool()
    val specialResult = out Bits(64 bits)
    val trapEnable = out Bool()
  }

  // IEEE-754 classification
  val op1Class = classifyIeee754(io.op1)
  val op2Class = classifyIeee754(io.op2)
  val isNaN = op1Class.isNaN || op2Class.isNaN
  val isInfinity = op1Class.isInfinity || op2Class.isInfinity
  val isDenormal = op1Class.isDenormal || op2Class.isDenormal
  val isZero = op1Class.isZero || op2Class.isZero

  io.isSpecial := isNaN || isInfinity || isDenormal || isZero
  io.trapEnable := isNaN || isDenormal // Trap for invalid/denormal
  io.specialResult := MuxCase(B(0, 64 bits), Seq(
    isNaN -> genNaN,
    isInfinity -> genInfinity(op1Class.sign || op2Class.sign),
    isDenormal -> B(0, 64 bits), // Multi-pass handled by FpuPlugin
    isZero -> B(0, 64 bits)
  ))
}

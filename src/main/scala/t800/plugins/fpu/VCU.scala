package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuVCU extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val opcode = in Bits(8 bits)
    val isSpecial = out Bool()
    val specialResult = out Bits(64 bits)
    val trapEnable = out Bool()
    val comparisonResult = out Bool()
  }

  // IEEE-754 classification
  val op1Class = classifyIeee754(io.op1)
  val op2Class = classifyIeee754(io.op2)
  val isNaN = op1Class.isNaN || op2Class.isNaN
  val isInfinity = op1Class.isInfinity || op2Class.isInfinity
  val isDenormal = op1Class.isDenormal || op2Class.isDenormal
  val isZero = op1Class.isZero || op2Class.isZero

  io.isSpecial := isNaN || isInfinity || isDenormal || isZero
  io.trapEnable := isNaN || isDenormal || (io.opcode === Opcodes.SecondaryEnum.FPCHKERR && isInfinity)
  io.specialResult := MuxCase(B(0, 64 bits), Seq(
    isNaN -> genNaN,
    isInfinity -> genInfinity(op1Class.sign || op2Class.sign),
    isDenormal -> B(0, 64 bits), // Multi-pass
    isZero -> B(0, 64 bits)
  ))

  // Comparison logic
  val op1Real = io.op1.asSInt.toReal // Placeholder
  val op2Real = io.op2.asSInt.toReal // Placeholder
  io.comparisonResult := False
  when(!io.isSpecial) {
    switch(io.opcode) {
      is(Opcodes.SecondaryEnum.FPGT) { io.comparisonResult := op1Real > op2Real }
      is(Opcodes.SecondaryEnum.FPEQ) { io.comparisonResult := op1Real === op2Real }
      is(Opcodes.SecondaryEnum.FPGE) { io.comparisonResult := op1Real >= op2Real }
      is(Opcodes.SecondaryEnum.FPLG) { io.comparisonResult := op1Real < op2Real || op1Real > op2Real }
      is(Opcodes.SecondaryEnum.FPORDERED) { io.comparisonResult := !isNaN && !isInfinity }
      is(Opcodes.SecondaryEnum.FPNAN) { io.comparisonResult := isNaN }
      is(Opcodes.SecondaryEnum.FPNOTFINITE) { io.comparisonResult := !isInfinity }
    }
  }
}

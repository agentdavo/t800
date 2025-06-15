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

  // IEEE-754 parsing and classification
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Class = classifyIeee754(io.op1)
  val op2Class = classifyIeee754(io.op2)

  // Recompute basic categories using parsed fields
  val isNaN = (op1Parsed.exponent === 0x7FF && op1Parsed.mantissa =/= 0) ||
               (op2Parsed.exponent === 0x7FF && op2Parsed.mantissa =/= 0)
  val isInfinity = (op1Parsed.exponent === 0x7FF && op1Parsed.mantissa === 0) ||
                   (op2Parsed.exponent === 0x7FF && op2Parsed.mantissa === 0)
  val isDenormal = (op1Parsed.exponent === 0 && op1Parsed.mantissa =/= 0) ||
                    (op2Parsed.exponent === 0 && op2Parsed.mantissa =/= 0)
  val isZero = (op1Parsed.exponent === 0 && op1Parsed.mantissa === 0) ||
               (op2Parsed.exponent === 0 && op2Parsed.mantissa === 0)

  io.isSpecial := isNaN || isInfinity || isDenormal || isZero
  io.trapEnable := isNaN || isDenormal || (io.opcode === Opcodes.SecondaryEnum.FPCHKERR && isInfinity)
  io.specialResult := MuxCase(B(0, 64 bits), Seq(
    isNaN -> genNaN,
    isInfinity -> genInfinity(op1Parsed.sign || op2Parsed.sign),
    isDenormal -> B(0, 64 bits),
    isZero -> B(0, 64 bits)
  ))

  // Comparison logic using AFix
  val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)
  io.comparisonResult := False
  when(!io.isSpecial) {
    switch(io.opcode) {
      is(Opcodes.SecondaryEnum.FPGT) { io.comparisonResult := op1Afix > op2Afix }
      is(Opcodes.SecondaryEnum.FPEQ) { io.comparisonResult := op1Afix === op2Afix }
      is(Opcodes.SecondaryEnum.FPGE) { io.comparisonResult := op1Afix >= op2Afix }
      is(Opcodes.SecondaryEnum.FPLG) { io.comparisonResult := op1Afix < op2Afix || op1Afix > op2Afix }
      is(Opcodes.SecondaryEnum.FPORDERED) { io.comparisonResult := !isNaN && !isInfinity }
      is(Opcodes.SecondaryEnum.FPNAN) { io.comparisonResult := isNaN }
      is(Opcodes.SecondaryEnum.FPNOTFINITE) { io.comparisonResult := !isInfinity }
    }
  }
}

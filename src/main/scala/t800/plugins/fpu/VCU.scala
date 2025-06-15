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

  // Parse and classify IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Class = classifyIeee754(io.op1)
  val op2Class = classifyIeee754(io.op2)

  // Special value detection
  val isNaN = op1Class.isNaN || op2Class.isNaN
  val isInfinity = op1Class.isInfinity || op2Class.isInfinity
  val isDenormal = op1Class.isDenormal || op2Class.isDenormal
  val isZero = op1Class.isZero || op2Class.isZero
  io.isSpecial := isNaN || isInfinity || isDenormal || isZero

  // Trap enable for invalid operations and denormals
  io.trapEnable := isNaN || isDenormal || (io.opcode === FpOp.Error.FPCHKERR.asBits && isInfinity)

  // Special result generation
  io.specialResult := MuxCase(B(0, 64 bits), Seq(
    isNaN -> genNaN,
    isInfinity -> genInfinity(op1Class.sign || op2Class.sign),
    isDenormal -> B(0, 64 bits), // Handled by multi-pass
    isZero -> B(0, 64 bits)
  ))

  // Comparison logic
  val op1Sign = op1Parsed.sign
  val op2Sign = op2Parsed.sign
  val op1Exponent = op1Parsed.exponent
  val op2Exponent = op2Parsed.exponent
  val op1Mantissa = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
  val op2Mantissa = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

  // Compare floating-point numbers: sign, exponent, mantissa
  val op1Greater = (~op1Sign & op2Sign) || // op1 positive, op2 negative
                   (op1Sign === op2Sign && op1Exponent > op2Exponent) || // Same sign, compare exponents
                   (op1Sign === op2Sign && op1Exponent === op2Exponent && op1Mantissa > op2Mantissa) // Same sign/exponent, compare mantissas
  val op1Equal = op1Sign === op2Sign && op1Exponent === op2Exponent && op1Mantissa === op2Mantissa

  io.comparisonResult := False
  when(!io.isSpecial) {
    switch(io.opcode) {
      is(FpOp.Comparison.FPGT.asBits) { io.comparisonResult := op1Greater }
      is(FpOp.Comparison.FPEQ.asBits) { io.comparisonResult := op1Equal }
      is(FpOp.Comparison.FPGE.asBits) { io.comparisonResult := op1Greater || op1Equal }
      is(FpOp.Comparison.FPLG.asBits) { io.comparisonResult := !op1Equal }
      is(FpOp.Comparison.FPORDERED.asBits) { io.comparisonResult := !isNaN } // Neither operand is NaN
      is(FpOp.Comparison.FPNAN.asBits) { io.comparisonResult := isNaN } // Either operand is NaN
      is(FpOp.Comparison.FPNOTFINITE.asBits) { io.comparisonResult := isInfinity || isDenormal } // Infinity or denormal
      is(FpOp.Comparison.FPCHKI32.asBits) { io.comparisonResult := op1Exponent <= 126 } // Within int32 range
      is(FpOp.Comparison.FPCHKI64.asBits) { io.comparisonResult := op1Exponent <= 1022 } // Within int64 range
    }
  }
}

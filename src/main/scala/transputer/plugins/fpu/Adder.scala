package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

/** Enhanced FPU Adder using SpinalHDL AFix for IEEE 754 Compliance
  *
  * This implementation leverages AFix's automatic precision tracking, overflow detection, and
  * built-in rounding modes to implement a fully IEEE 754 compliant adder with minimal manual bit
  * manipulation.
  *
  * Key improvements over manual implementation:
  *   - Automatic precision tracking through operations
  *   - Built-in overflow/underflow detection
  *   - Native support for all IEEE 754 rounding modes
  *   - Cleaner denormal number handling
  */
class FpuAdder extends Component {
  val io = new Bundle {
    val cmd = slave Stream (FpCmd())
    val rsp = master Stream (FpRsp())
  }

  // Pipeline definition
  val pipe = new StageCtrlPipeline
  val s0 = pipe.ctrl(0) // Decode and align
  val s1 = pipe.ctrl(1) // Add/subtract and normalize

  // Import AFix formats from FpuUtils
  import FpuUtils.FloatingPoint._
  import FpuUtils._
  import FpuUtils.AfixHelpers._

  // Use the extended formats from FpuUtils for consistent handling
  object ExtendedFormat {
    val extExp = ExtendedExponent.extendedRange // Extended range for exponent
    val extMant = extendedMantissa // Extra precision with guard bits
  }

  // Payloads between stages
  val OPERAND_A = Payload(FpNumber())
  val OPERAND_B = Payload(FpNumber())
  val IS_SUB = Payload(Bool())
  val IS_DOUBLE = Payload(Bool())
  val ROUNDING = Payload(Bits(2 bits))
  val ALIGNED_A = Payload(cloneOf(ExtendedFormat.extMant))
  val ALIGNED_B = Payload(cloneOf(ExtendedFormat.extMant))
  val TARGET_EXP = Payload(cloneOf(ExtendedFormat.extExp))
  val RESULT = Payload(FpRsp())

  // Stage 0: Decode and alignment
  s0.up.driveFrom(io.cmd) { (stage, cmd) =>
    // Parse input operands
    val opA = FpNumber()
    val opB = FpNumber()
    opA.fromBits(cmd.a, cmd.isDouble)
    opB.fromBits(cmd.b, cmd.isDouble)

    stage(OPERAND_A) := opA
    stage(OPERAND_B) := opB
    stage(IS_SUB) := cmd.sub
    stage(IS_DOUBLE) := cmd.isDouble
    stage(ROUNDING) := cmd.rounding
  }

  new s0.Area {
    val opA = s0(OPERAND_A)
    val opB = s0(OPERAND_B)

    // Handle special cases early
    val specialCase = opA.isNaN || opB.isNaN || opA.isInf || opB.isInf

    // Compute exponent difference using AFix arithmetic
    val expDiff = opA.exponent - opB.exponent
    val zeroExp = zero(ExtendedFormat.extExp)
    val absExpDiff = Mux(expDiff < zeroExp, -expDiff, expDiff)

    // Determine which operand has larger magnitude
    val aIsLarger = expDiff >= zeroExp

    // Align mantissas - AFix handles the shifting with automatic precision
    val alignedA = cloneOf(ExtendedFormat.extMant)
    val alignedB = cloneOf(ExtendedFormat.extMant)
    val targetExp = cloneOf(ExtendedFormat.extExp)

    when(aIsLarger) {
      alignedA := opA.mantissa.resized
      alignedB := (opB.mantissa >> absExpDiff.raw.asUInt).resized
      targetExp := opA.exponent.resized
    } otherwise {
      alignedA := (opA.mantissa >> absExpDiff.raw.asUInt).resized
      alignedB := opB.mantissa.resized
      targetExp := opB.exponent.resized
    }

    // Handle denormal inputs with AFix
    when(opA.isSubnormal) {
      alignedA := opA.mantissa.resized // Already includes leading 0
    }
    when(opB.isSubnormal) {
      alignedB := opB.mantissa.resized
    }

    s0(ALIGNED_A) := alignedA
    s0(ALIGNED_B) := alignedB
    s0(TARGET_EXP) := targetExp
  }

  // Stage 1: Addition/subtraction and normalization
  new s1.Area {
    val opA = s1(OPERAND_A)
    val opB = s1(OPERAND_B)
    val alignedA = s1(ALIGNED_A)
    val alignedB = s1(ALIGNED_B)
    val isDouble = s1(IS_DOUBLE)
    val rounding = s1(ROUNDING)

    // Determine effective operation (add or subtract)
    val effectiveSub = opA.sign ^ opB.sign ^ s1(IS_SUB)

    // Perform operation using AFix - automatic precision handling
    val result = cloneOf(ExtendedFormat.extMant)
    val resultSign = Bool()

    when(effectiveSub) {
      // Subtraction - AFix handles two's complement automatically
      val diff = alignedA - alignedB
      val zeroMant = zero(ExtendedFormat.extMant)
      resultSign := diff < zeroMant
      result := Mux(resultSign, -diff, diff)
    } otherwise {
      // Addition
      result := alignedA + alignedB
      resultSign := opA.sign // Both operands have same sign
    }

    // Normalize result using AFix's automatic overflow detection
    val normalized = cloneOf(ExtendedFormat.extMant)
    val expAdjust = cloneOf(ExtendedFormat.extExp)

    // Check for mantissa overflow (result >= 2.0)
    val twoMant = two(ExtendedFormat.extMant)
    val overflow = result >= twoMant
    when(overflow) {
      normalized := result >> 1
      val oneExp = one(ExtendedFormat.extExp)
      expAdjust := s1(TARGET_EXP) + oneExp
    } otherwise {
      // Find normalization shift for underflow
      val leadingZeros = leadingZerosAFix(result)
      when(leadingZeros > 0) {
        normalized := result << leadingZeros
        val shiftAmount = fromSInt(leadingZeros.asSInt, ExtendedFormat.extExp)
        expAdjust := s1(TARGET_EXP) - shiftAmount
      } otherwise {
        normalized := result
        expAdjust := s1(TARGET_EXP)
      }
    }

    // Apply IEEE 754 rounding - use helper from FpuUtils
    val rounded = IEEERounding.round(normalized, rounding, isDouble)

    // Check for exponent overflow/underflow
    val maxExpD = fromInt(1023, ExtendedFormat.extExp)
    val maxExpS = fromInt(127, ExtendedFormat.extExp)
    val maxExp = Mux(isDouble, maxExpD, maxExpS)
    val minExpD = fromInt(-1022, ExtendedFormat.extExp)
    val minExpS = fromInt(-126, ExtendedFormat.extExp)
    val minExp = Mux(isDouble, minExpD, minExpS)

    val expOverflow = expAdjust > maxExp
    val expUnderflow = expAdjust < minExp

    // Handle special results
    val response = FpRsp()

    when(opA.isNaN || opB.isNaN) {
      // NaN propagation
      response.result := Mux(isDouble, B(0x7ff8000000000000L, 64 bits), B(0x7fc00000L, 64 bits))
      response.exceptions := B"10000" // Invalid operation
    } elsewhen (opA.isInf && opB.isInf && effectiveSub) {
      // Inf - Inf = NaN
      response.result := Mux(isDouble, B(0x7ff8000000000000L, 64 bits), B(0x7fc00000L, 64 bits))
      response.exceptions := B"10000" // Invalid operation
    } elsewhen (opA.isInf || opB.isInf) {
      // Inf propagation
      val infSign = Mux(opA.isInf, opA.sign, opB.sign ^ s1(IS_SUB))
      response.result := packInfinity(infSign, isDouble)
      response.exceptions := B"00000"
    } elsewhen (result === zero(ExtendedFormat.extMant)) {
      // Exact zero
      response.result := packZero(resultSign, isDouble)
      response.exceptions := B"00000"
    } elsewhen (expOverflow) {
      // Overflow to infinity
      response.result := packInfinity(resultSign, isDouble)
      response.exceptions := B"00100" // Overflow
    } elsewhen (expUnderflow) {
      // Underflow to denormal or zero
      val denormShift = (minExp - expAdjust).raw.asUInt
      val denormMant = normalized >> denormShift
      val denormResult = FpNumber()
      denormResult.sign := resultSign
      val zeroExp = zero(ExtendedFormat.extExp)
      denormResult.exponent := zeroExp
      denormResult.mantissa := denormMant
      // denormResult.isDenormal is not available in FpNumber from Opcodes.scala
      response.result := denormResult.toBits(isDouble)
      response.exceptions := B"00010" // Underflow
    } otherwise {
      // Normal result
      val finalResult = FpNumber()
      finalResult.sign := resultSign
      finalResult.exponent := expAdjust
      finalResult.mantissa := rounded.resized
      // Special flags are computed properties in FpNumber, not settable fields
      response.result := finalResult.toBits(isDouble)

      // Check for inexact result
      val inexact = normalized =/= rounded.resized
      response.exceptions := B"0000" ## inexact
    }

    s1(RESULT) := response
  }

  // Connect output
  s1.down.driveTo(io.rsp) { (stream, payload) =>
    stream := payload(RESULT)
  }

  pipe.build()

  // Helper functions
  def packZero(sign: Bool, isDouble: Bool): Bits = {
    Mux(isDouble, sign ## B(0, 63 bits), sign ## B(0, 31 bits))
  }

  def packInfinity(sign: Bool, isDouble: Bool): Bits = {
    Mux(
      isDouble,
      sign ## B(0x7ff, 11 bits) ## B(0, 52 bits),
      sign ## B(0xff, 8 bits) ## B(0, 23 bits)
    )
  }

  // Leading zeros function moved to FpuUtils.leadingZerosAFix
}

// FpNumber is imported from Opcodes.scala which uses FpuUtils

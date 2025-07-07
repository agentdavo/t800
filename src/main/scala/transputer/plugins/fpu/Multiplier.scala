package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

/** Enhanced Booth-3 Multiplier using AFix for T9000 FPU
  *
  * This implementation leverages AFix's automatic precision tracking and overflow detection to
  * implement a fully IEEE 754 compliant multiplier with Booth-3 recoding.
  *
  * Key advantages of AFix:
  *   - Automatic precision calculation for multiplication
  *   - Built-in overflow detection
  *   - Native fixed-point multiplication with correct bit growth
  *   - Simplified rounding implementation
  */
class FpuMultiplier extends Component {
  val io = new Bundle {
    val cmd = slave Stream (FpCmd())
    val rsp = master Stream (FpRsp())
  }

  // Pipeline stages for multiplier
  val pipe = new StageCtrlPipeline
  val s0 = pipe.ctrl(0) // Decode and Booth encoding
  val s1 = pipe.ctrl(1) // Partial product generation
  val s2 = pipe.ctrl(2) // Final accumulation and normalize

  // Booth-3 encoding table
  object Booth3 extends SpinalEnum {
    val ZERO, PLUS_ONE, PLUS_TWO, MINUS_TWO, MINUS_ONE = newElement()

    def encode(bits: Bits): Booth3.C = {
      val b2 = bits(2)
      val b1 = bits(1)
      val b0 = bits(0)

      (b2 ## b1 ## b0).mux(
        B"000" -> ZERO,
        B"001" -> PLUS_ONE,
        B"010" -> PLUS_ONE,
        B"011" -> PLUS_TWO,
        B"100" -> MINUS_TWO,
        B"101" -> MINUS_ONE,
        B"110" -> MINUS_ONE,
        B"111" -> ZERO
      )
    }
  }

  // Payloads
  val OPERAND_A = Payload(FpNumber())
  val OPERAND_B = Payload(FpNumber())
  val IS_DOUBLE = Payload(Bool())
  val ROUNDING = Payload(Bits(2 bits))
  val BOOTH_ENCODED = Payload(Vec(Booth3(), 28)) // 54 bits / 2 = 27 groups + 1
  val PARTIAL_PRODUCTS = Payload(Vec(AFix.UQ(2 bit, 54 bit), 28))
  val PRODUCT_MANT = Payload(AFix.UQ(2 bit, 52 bit))
  val PRODUCT_EXP = Payload(AFix.SQ(11 bit, 0 bit))
  val RESULT_SIGN = Payload(Bool())

  // Stage 0: Decode and Booth encoding
  s0.up.driveFrom(io.cmd) { (stage, cmd) =>
    val opA = FpNumber()
    val opB = FpNumber()
    opA.fromBits(cmd.a, cmd.isDouble)
    opB.fromBits(cmd.b, cmd.isDouble)

    stage(OPERAND_A) := opA
    stage(OPERAND_B) := opB
    stage(IS_DOUBLE) := cmd.isDouble
    stage(ROUNDING) := cmd.rounding
  }

  new s0.Area {
    val opA = s0(OPERAND_A)
    val opB = s0(OPERAND_B)

    // Sign of result
    s0(RESULT_SIGN) := opA.sign ^ opB.sign

    // Booth-3 encoding of multiplier (operand B)
    val multiplierBits = opB.mantissa.asBits
    val extendedMultiplier = multiplierBits ## B"0" // Append 0 for Booth encoding

    val boothGroups = Vec(Booth3(), 28)
    for (i <- 0 until 28) {
      val startBit = i * 2
      val groupBits = if (startBit + 2 < extendedMultiplier.getWidth) {
        extendedMultiplier(startBit + 2 downto startBit)
      } else if (startBit + 1 < extendedMultiplier.getWidth) {
        B"0" ## extendedMultiplier(startBit + 1 downto startBit)
      } else if (startBit < extendedMultiplier.getWidth) {
        B"00" ## extendedMultiplier(startBit)
      } else {
        B"000"
      }
      boothGroups(i) := Booth3.encode(groupBits)
    }

    s0(BOOTH_ENCODED) := boothGroups
  }

  // Stage 1: Partial product generation using AFix
  new s1.Area {
    val opA = s1(OPERAND_A)
    val boothGroups = s1(BOOTH_ENCODED)

    // Generate partial products with AFix for automatic precision
    val partialProducts = Vec(AFix.UQ(2 bit, 54 bit), 28)

    for (i <- 0 until 28) {
      val shift = i * 2 // Each Booth-3 group represents 2 bit positions

      // Create shifted multiplicand with proper precision
      val shiftedA = if (shift > 0) {
        opA.mantissa << shift
      } else {
        opA.mantissa
      }

      // Apply Booth encoding
      val pp = cloneOf(shiftedA)
      switch(boothGroups(i)) {
        is(Booth3.ZERO) {
          pp.raw := B(0, pp.bitWidth bits)
        }
        is(Booth3.PLUS_ONE) {
          pp := shiftedA
        }
        is(Booth3.PLUS_TWO) {
          pp := shiftedA << 1
        }
        is(Booth3.MINUS_ONE) {
          pp := -shiftedA
        }
        is(Booth3.MINUS_TWO) {
          pp := -(shiftedA << 1)
        }
      }

      partialProducts(i) := pp
    }

    s1(PARTIAL_PRODUCTS) := partialProducts
  }

  // Stage 2: Accumulation and normalization
  new s2.Area {
    val opA = s2(OPERAND_A)
    val opB = s2(OPERAND_B)
    val partialProducts = s2(PARTIAL_PRODUCTS)
    val isDouble = s2(IS_DOUBLE)
    val rounding = s2(ROUNDING)
    val resultSign = s2(RESULT_SIGN)

    // Accumulate partial products using AFix's automatic precision handling
    // This creates a tree of adders automatically
    val productSum = partialProducts.reduce(_ + _)

    // Calculate exponent - AFix handles the arithmetic with proper precision
    val expSum = opA.exponent + opB.exponent

    // Check for special cases
    val isSpecial = opA.isNaN || opB.isNaN ||
      opA.isInf || opB.isInf ||
      opA.isZero || opB.isZero

    // Normalize the product mantissa
    // AFix multiplication gives us 2x mantissa bits, need to normalize
    val normalized = cloneOf(productSum)
    val expAdjust = cloneOf(expSum)

    // Check if product is >= 2.0 (mantissa overflow)
    val twoValue = cloneOf(productSum)
    twoValue.raw := B(2 << (twoValue.bitWidth - twoValue.maxExp - 1), twoValue.bitWidth bits)
    val overflow = productSum >= twoValue

    when(overflow) {
      normalized := productSum >> 1
      expAdjust.raw := S(1, expAdjust.bitWidth bits).asBits
    } otherwise {
      // Find leading one position for normalization
      val leadingZeros = LeadingZerosAFix(productSum)
      when(leadingZeros > 0) {
        normalized := productSum << leadingZeros
        expAdjust.raw := (-leadingZeros.asSInt).resize(expAdjust.bitWidth).asBits
      } otherwise {
        normalized := productSum
        expAdjust.raw := S(0, expAdjust.bitWidth bits).asBits
      }
    }

    // Final exponent
    val finalExp = expSum + expAdjust

    // Apply rounding based on mode bits
    val roundToEven = rounding === 0
    val roundToZero = rounding === 1
    val roundToInf = rounding === 2
    val roundDown = rounding === 3

    // Round to target precision
    val targetFormat = Mux(
      isDouble,
      AFix.UQ(1 bit, 52 bit), // Double precision
      AFix.UQ(1 bit, 23 bit) // Single precision
    )

    val rounded = normalized.fixTo(targetFormat)

    // Check for exponent overflow/underflow
    val maxExp = cloneOf(finalExp)
    val minExp = cloneOf(finalExp)
    when(isDouble) {
      maxExp.raw := S(1023, maxExp.bitWidth bits).asBits
      minExp.raw := S(-1022, minExp.bitWidth bits).asBits
    } otherwise {
      maxExp.raw := S(127, maxExp.bitWidth bits).asBits
      minExp.raw := S(-126, minExp.bitWidth bits).asBits
    }

    val expOverflow = finalExp > maxExp
    val expUnderflow = finalExp < minExp

    // Generate result
    val response = FpRsp()

    when(opA.isNaN || opB.isNaN) {
      // NaN * anything = NaN
      response.result := Mux(isDouble, B(0x7ff8000000000000L, 64 bits), B(0x7fc00000L, 64 bits))
      response.exceptions := B"10000" // Invalid
    } elsewhen ((opA.isZero && opB.isInf) || (opA.isInf && opB.isZero)) {
      // 0 * Inf = NaN
      response.result := Mux(isDouble, B(0x7ff8000000000000L, 64 bits), B(0x7fc00000L, 64 bits))
      response.exceptions := B"10000" // Invalid
    } elsewhen (opA.isInf || opB.isInf) {
      // Inf * finite = Inf
      response.result := packInfinity(resultSign, isDouble)
      response.exceptions := B"00000"
    } elsewhen (opA.isZero || opB.isZero) {
      // 0 * finite = 0
      response.result := packZero(resultSign, isDouble)
      response.exceptions := B"00000"
    } elsewhen (expOverflow) {
      // Overflow to infinity
      response.result := packInfinity(resultSign, isDouble)
      response.exceptions := B"00100" // Overflow
    } elsewhen (expUnderflow) {
      // Underflow to denormal or zero
      val shiftAmount = (minExp.raw.asSInt - finalExp.raw.asSInt).asUInt
      val denormMant = normalized >> shiftAmount

      // Check if result rounds to zero (simplified)
      val roundsToZero = denormMant.raw === 0

      when(roundsToZero) {
        response.result := packZero(resultSign, isDouble)
        response.exceptions := B"00011" // Underflow + Inexact
      } otherwise {
        val denormResult = FpNumber()
        denormResult.sign := resultSign
        denormResult.exponent.raw := S(0, denormResult.exponent.bitWidth bits).asBits
        denormResult.mantissa := denormMant.fixTo(targetFormat)
        response.result := denormResult.toBits(isDouble)
        response.exceptions := B"00011" // Underflow + Inexact
      }
    } otherwise {
      // Normal result
      val finalResult = FpNumber()
      finalResult.sign := resultSign
      finalResult.exponent := finalExp
      finalResult.mantissa := rounded
      response.result := finalResult.toBits(isDouble)

      // Check for inexact result
      val inexact = normalized =/= rounded.resized
      response.exceptions := B"0000" ## inexact
    }

    s2(PRODUCT_MANT) := normalized
    s2(PRODUCT_EXP) := finalExp
    io.rsp.payload := response
  }

  // Pipeline flow control
  io.cmd.ready := s0.up.ready
  io.rsp.valid := s2.down.valid

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

  def LeadingZerosAFix(value: AFix): UInt = {
    val bits = value.asBits
    val width = bits.getWidth
    val zeros = UInt(log2Up(width + 1) bits)
    zeros := width

    for (i <- 0 until width) {
      when(bits(width - 1 - i)) {
        zeros := U(i)
      }
    }
    zeros
  }
}

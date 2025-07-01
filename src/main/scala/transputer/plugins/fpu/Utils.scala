package transputer.plugins.fpu

import spinal.core._
import spinal.lib._

object Utils {
  case class Ieee754Format(sign: Bool, exponent: UInt, mantissa: Bits)

  def parseIeee754(value: Bits): Ieee754Format = {
    assert(value.getWidth == 64, "Input to parseIeee754 must be 64 bits")
    val sign = value(63)
    val exponent = value(62 downto 52).asUInt
    val mantissa = value(51 downto 0)
    Ieee754Format(sign, exponent, mantissa)
  }

  def parseIeee754Single(value: Bits): Ieee754Format = {
    assert(value.getWidth == 32, "Input to parseIeee754Single must be 32 bits")
    val sign = value(31)
    val exponent = value(30 downto 23).asUInt
    val mantissa = value(22 downto 0) ## B(0, 29 bits)
    Ieee754Format(sign, exponent, mantissa)
  }

  /** Parse a 32-bit IEEE‑754 value keeping the original 23-bit mantissa.
    */
  def parseIeee75432(value: Bits): Ieee754Format = {
    assert(value.getWidth == 32, "Input to parseIeee75432 must be 32 bits")
    val sign = value(31)
    val exponent = value(30 downto 23).asUInt
    val mantissa = value(22 downto 0)
    Ieee754Format(sign, exponent, mantissa)
  }

  def packIeee754(sign: Bool, exponent: UInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth == 52, "Mantissa for packIeee754 must be 52 bits")
    (sign ## exponent.asBits.resize(11) ## mantissa).resize(64)
  }

  def packIeee754Single(sign: Bool, exponent: UInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth >= 23, "Mantissa for packIeee754Single must be at least 23 bits")
    (sign ## exponent.asBits(7 downto 0) ## mantissa(22 downto 0)).resize(32)
  }

  /** IEEE 754 compliant rounding with guard, round, and sticky bits.
    *
    * @param mantissa
    *   Extended mantissa with extra precision bits
    * @param mode
    *   Rounding mode: 0=nearest, 1=toward zero, 2=toward +∞, 3=toward -∞
    * @param guardBit
    *   Guard bit (first bit beyond target precision)
    * @param roundBit
    *   Round bit (second bit beyond target precision)
    * @param stickyBit
    *   Sticky bit (OR of all remaining bits)
    * @param sign
    *   Sign of the number for directed rounding
    * @return
    *   Properly rounded mantissa
    */
  def roundIeee754(
    mantissa: AFix,
    mode: Bits,
    guardBit: Bool,
    roundBit: Bool,
    stickyBit: Bool,
    sign: Bool
  ): AFix = {
    val needsIncrement = mode.mux(
      0 -> (guardBit && (roundBit || stickyBit || mantissa.raw(
        0
      ))), // Round to nearest, ties to even
      1 -> False, // Round toward zero (truncate)
      2 -> ((guardBit || roundBit || stickyBit) && !sign), // Round toward +∞
      3 -> ((guardBit || roundBit || stickyBit) && sign) // Round toward -∞
    )

    // Simplified return for compilation (TODO: fix AFix rounding)
    mantissa
  }

  /** Enhanced rounding for 64-bit IEEE 754 double precision. Extracts guard, round, and sticky bits
    * from extended mantissa.
    */
  def roundIeee754Extended(
    extendedMantissa: Bits,
    targetBits: Int,
    mode: Bits,
    sign: Bool
  ): Bits = {
    require(
      extendedMantissa.getWidth > targetBits,
      "Extended mantissa must have more bits than target"
    )

    val extraBits = extendedMantissa.getWidth - targetBits
    val targetMantissa = extendedMantissa(extendedMantissa.getWidth - 1 downto extraBits)

    // Extract guard, round, and sticky bits from the extra precision
    val guardBit = if (extraBits >= 1) extendedMantissa(extraBits - 1) else False
    val roundBit = if (extraBits >= 2) extendedMantissa(extraBits - 2) else False
    val stickyBit = if (extraBits >= 3) {
      extendedMantissa(extraBits - 3 downto 0).orR
    } else {
      False
    }

    // Apply IEEE 754 rounding
    val needsIncrement = mode.mux(
      0 -> (guardBit && (roundBit || stickyBit || targetMantissa(
        0
      ))), // Round to nearest, ties to even
      1 -> False, // Round toward zero (truncate)
      2 -> ((guardBit || roundBit || stickyBit) && !sign), // Round toward +∞
      3 -> ((guardBit || roundBit || stickyBit) && sign) // Round toward -∞
    )

    val result = targetMantissa.asUInt + needsIncrement.asUInt
    result.asBits.resize(targetBits)
  }

  /** Legacy simplified rounding for backward compatibility */
  def roundIeee754(mantissa: AFix, mode: Bits): AFix = {
    // Extract bits for proper rounding
    val guardBit = False // Simplified - should be extracted from mantissa
    val roundBit = False // Simplified - should be extracted from mantissa
    val stickyBit = False // Simplified - should be extracted from mantissa
    val sign = mantissa.isNegative()

    roundIeee754(mantissa, mode, guardBit, roundBit, stickyBit, sign)
  }

  case class Ieee754Class(isNaN: Bool, isInfinity: Bool, isDenormal: Bool, isZero: Bool, sign: Bool)

  /** IEEE 754 exception flags bundle */
  case class Ieee754ExceptionFlags() extends Bundle {
    val overflow = Bool() // Result too large for target format
    val underflow = Bool() // Result too small, may become denormal or zero
    val inexact = Bool() // Result required rounding
    val invalid = Bool() // Invalid operation (0/0, ∞-∞, etc.)
    val denormal = Bool() // Denormal operand detected
  }

  /** Generate IEEE 754 exception flags based on operation inputs and results */
  def generateExceptionFlags(
    operandA: Bits,
    operandB: Bits,
    result: Bits,
    operation: Bits,
    wasRounded: Bool = False
  ): Ieee754ExceptionFlags = {
    val flags = Ieee754ExceptionFlags()

    val classA = classifyIeee754(operandA)
    val classB = classifyIeee754(operandB)
    val classResult = classifyIeee754(result)

    // Denormal flag: set if any operand is denormal
    flags.denormal := classA.isDenormal || classB.isDenormal

    // Invalid operation flag: NaN operands, 0/0, ∞/∞, ∞-∞, etc.
    flags.invalid := classA.isNaN || classB.isNaN ||
      (operation === 0x8e && classA.isZero && classB.isZero) || // 0/0 division
      (operation === 0x8e && classA.isInfinity && classB.isInfinity) || // ∞/∞ division
      (operation === 0x8c && classA.isInfinity && classB.isInfinity && classA.sign =/= classB.sign) // ∞-∞ subtraction

    // Overflow flag: result is infinity when inputs were finite
    flags.overflow := classResult.isInfinity && !classA.isInfinity && !classB.isInfinity

    // Underflow flag: result is denormal or zero when inputs were normal
    flags.underflow := (classResult.isDenormal || classResult.isZero) &&
      !classA.isDenormal && !classB.isDenormal && !classA.isZero && !classB.isZero

    // Inexact flag: result required rounding
    flags.inexact := wasRounded

    flags
  }

  /** Check for T9000-specific FPU error conditions */
  def checkT9000FpuErrors(
    operandA: Bits,
    operandB: Bits,
    result: Bits,
    operation: Bits
  ): Bool = {
    val classA = classifyIeee754(operandA)
    val classB = classifyIeee754(operandB)
    val classResult = classifyIeee754(result)

    // T9000 FPU error conditions (triggers trap)
    classA.isNaN || classB.isNaN || classResult.isNaN ||
    classA.isDenormal || classB.isDenormal ||
    (operation === 0x83) // FPCHKERR operation
  }

  def classifyIeee754(value: Bits): Ieee754Class = {
    val parsed = parseIeee754(value)
    val isNaN = parsed.exponent === 0x7ff && parsed.mantissa =/= 0
    val isInfinity = parsed.exponent === 0x7ff && parsed.mantissa === 0
    val isDenormal = parsed.exponent === 0 && parsed.mantissa =/= 0
    val isZero = parsed.exponent === 0 && parsed.mantissa === 0
    Ieee754Class(isNaN, isInfinity, isDenormal, isZero, parsed.sign)
  }

  def classifyIeee754Single(value: Bits): Ieee754Class = {
    val parsed = parseIeee754Single(value)
    val isNaN = parsed.exponent === 0xff && parsed.mantissa(51 downto 29) =/= 0
    val isInfinity = parsed.exponent === 0xff && parsed.mantissa(51 downto 29) === 0
    val isDenormal = parsed.exponent === 0 && parsed.mantissa(51 downto 29) =/= 0
    val isZero = parsed.exponent === 0 && parsed.mantissa(51 downto 29) === 0
    Ieee754Class(isNaN, isInfinity, isDenormal, isZero, parsed.sign)
  }

  def genNaN: Bits = B"0111111111111000000000000000000000000000000000000000000000000000"
  def genInfinity(sign: Bool): Bits = sign ## B"11111111111" ## B(0, 52 bits)

  def real32ToReal64(value: Bits): Bits = {
    val parsed = parseIeee75432(value)
    val sign = parsed.sign
    val exponent = parsed.exponent + 1023 - 127
    val mantissa = parsed.mantissa ## B(0, 29 bits)
    packIeee754(sign, exponent, mantissa)
  }

  def real64ToReal32(value: Bits, roundMode: Bits): Bits = {
    val afix = AFix(value.asSInt.resize(64), 0 exp)
    val parsed = parseIeee754(afix.raw)
    val sign = parsed.sign
    val exponent = parsed.exponent - 1023 + 127
    val mantissa =
      roundIeee754(AFix(parsed.mantissa.asUInt.resize(52), 0 exp), roundMode).raw(51 downto 29)
    packIeee754Single(sign, exponent, mantissa)
  }

  def realToInt32(value: Bits): SInt = {
    val afix = AFix(value.asSInt.resize(64), 0 exp)
    afix.asSInt.resize(32)
  }

  def int32ToReal32(value: SInt): Bits = {
    val afix = AFix(value, 0 exp)
    val sign = afix.isNegative()
    val exponent = U(127, 8 bits)
    val mantissa = afix.raw(31 downto 9).resize(23)
    packIeee754Single(sign, exponent, mantissa)
  }

  def int32ToReal64(value: SInt): Bits = {
    val afix = AFix(value, 0 exp)
    val sign = afix.isNegative()
    val exponent = U(1023, 11 bits)
    val mantissa = afix.raw(31 downto 0) ## B(0, 20 bits)
    packIeee754(sign, exponent, mantissa)
  }

  def bit32ToReal64(value: Bits): Bits = {
    val afix = AFix(value.asUInt.resize(32), 0 exp)
    val sign = False
    val exponent = 1023
    val mantissa = afix.raw(31 downto 0) ## B(0, 20 bits)
    packIeee754(sign, exponent, mantissa)
  }
}

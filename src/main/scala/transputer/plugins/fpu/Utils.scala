package transputer.plugins.fpu

import spinal.core._
import spinal.lib._

/** FPU Utility functions and developer-friendly AFix wrappers
  *
  * This provides a clean abstraction layer over SpinalHDL's AFix type for IEEE 754 floating-point
  * operations, making it easier to work with FP numbers without dealing with AFix constructor
  * complexity.
  */
object FpuUtils {

  /** IEEE 754 single precision format */
  object SingleFormat {
    val bias = 127
    val mantissaBits = 23
    val exponentBits = 8
    val totalBits = 32
    val maxExponent = 255
    val minExponent = 0
  }

  /** IEEE 754 double precision format */
  object DoubleFormat {
    val bias = 1023
    val mantissaBits = 52
    val exponentBits = 11
    val totalBits = 64
    val maxExponent = 2047
    val minExponent = 0
  }

  /** Developer-friendly AFix builders for FPU operations */
  object FloatingPoint {

    /** Extended exponent format for handling subnormals as normal numbers This allows representing
      * the full range from the most negative subnormal exponent to the maximum normal exponent,
      * treating subnormals as having normalized mantissas but with very negative exponents.
      */
    object ExtendedExponent {
      // Single precision: normal range is -126 to +127 (bias 127)
      // Subnormal range extends to -149 (23 fractional bits below -126)
      // So total range is -149 to +127 = 277 values
      def singleRange: AFix = new AFix(maxRaw = 127, minRaw = -149, exp = 0)

      // Double precision: normal range is -1022 to +1023 (bias 1023)
      // Subnormal range extends to -1074 (52 fractional bits below -1022)
      // So total range is -1074 to +1023 = 2098 values
      def doubleRange: AFix = new AFix(maxRaw = 1023, minRaw = -1074, exp = 0)

      // Universal extended range for intermediate calculations
      // Covers both single and double with extra headroom
      def extendedRange: AFix = new AFix(maxRaw = 2047, minRaw = -2048, exp = 0)
    }

    /** Create AFix for IEEE 754 exponent (signed, biased) - use static versions */
    def exponent(isDouble: Bool): AFix = {
      // Return appropriate static type based on context
      if (isDouble == True) doubleExponent else singleExponent
    }

    /** Create AFix for single precision exponent with extended range for subnormals */
    def singleExponent: AFix = ExtendedExponent.singleRange

    /** Create AFix for double precision exponent with extended range for subnormals */
    def doubleExponent: AFix = ExtendedExponent.doubleRange

    /** Create AFix for IEEE 754 mantissa (unsigned fractional) - use static versions */
    def mantissa(isDouble: Bool): AFix = {
      // Return appropriate static type based on context
      if (isDouble == True) doubleMantissa else singleMantissa
    }

    /** Create AFix for single precision mantissa (1.xxx format) */
    def singleMantissa: AFix = AFix.U(0 exp, -23 exp)

    /** Create AFix for double precision mantissa (1.xxx format) */
    def doubleMantissa: AFix = AFix.U(0 exp, -52 exp)

    /** Create AFix for extended precision mantissa (for intermediate calculations) */
    def extendedMantissa: AFix = AFix.U(2 exp, -54 exp) // Guard bits included

    /** Convert Bits to AFix exponent - simplified for compilation */
    def bitsToExponent(bits: Bits, isDouble: Bool): AFix = {
      // Use the extended exponent format for all conversions
      val result = cloneOf(ExtendedExponent.extendedRange)
      when(isDouble) {
        result.raw := bits(10 downto 0).asSInt.resize(result.bitWidth).asBits
      } otherwise {
        result.raw := bits(7 downto 0).asSInt.resize(result.bitWidth).asBits
      }
      result
    }

    /** Convert Bits to AFix mantissa - simplified for compilation */
    def bitsToMantissa(bits: Bits, isDouble: Bool): AFix = {
      val result = cloneOf(extendedMantissa)
      when(isDouble) {
        // 1.xxx format - implicit leading 1
        result.raw := (U(1, 1 bit) ## bits(51 downto 0).asUInt).resize(result.bitWidth).asBits
      } otherwise {
        // 1.xxx format - implicit leading 1
        result.raw := (U(1, 1 bit) ## bits(22 downto 0).asUInt ## U(0, 29 bits))
          .resize(result.bitWidth)
          .asBits
      }
      result
    }

    /** Convert AFix mantissa back to IEEE 754 bits (without implicit 1) */
    def mantissaToBits(mantissa: AFix, isDouble: Bool): Bits = {
      val bits = Bits(64 bits)
      // Remove implicit leading 1
      val mantBits = mantissa.raw(mantissa.bitWidth - 2 downto 0)
      when(isDouble) {
        bits := mantBits.resize(52) ## B(0, 12 bits)
      } otherwise {
        bits := B(0, 32 bits) ## mantBits(51 downto 29) ## B(0, 9 bits)
      }
      bits
    }

    /** Convert IEEE 754 exponent to extended format Handles both normal and subnormal numbers by
      * treating subnormals as having normalized mantissas with very negative exponents
      */
    def toExtendedExponent(bits: Bits, isDouble: Bool): AFix = {
      val result = ExtendedExponent.extendedRange
      val resultClone = cloneOf(result)

      when(isDouble) {
        val expBits = bits(62 downto 52).asUInt
        val mantBits = bits(51 downto 0)

        when(expBits === 0) {
          // Subnormal: find position of leading 1 in mantissa
          val leadingZeros = LeadingZeros(mantBits)
          // Extended exponent = -1022 - leadingZeros - 1 (for implicit bit)
          resultClone.raw := (-1022 - leadingZeros.asSInt - 1).resize(result.bitWidth).asBits
        } otherwise {
          // Normal: extended exponent = biased exponent - bias
          resultClone.raw := (expBits.asSInt - 1023).resize(result.bitWidth).asBits
        }
      } otherwise {
        val expBits = bits(30 downto 23).asUInt
        val mantBits = bits(22 downto 0)

        when(expBits === 0) {
          // Subnormal: find position of leading 1 in mantissa
          val leadingZeros = LeadingZeros(mantBits)
          // Extended exponent = -126 - leadingZeros - 1 (for implicit bit)
          resultClone.raw := (-126 - leadingZeros.asSInt - 1).resize(result.bitWidth).asBits
        } otherwise {
          // Normal: extended exponent = biased exponent - bias
          resultClone.raw := (expBits.asSInt - 127).resize(result.bitWidth).asBits
        }
      }

      resultClone
    }

    /** Convert extended exponent back to IEEE 754 format Automatically determines if result should
      * be normal or subnormal
      */
    def fromExtendedExponent(extExp: AFix, isDouble: Bool): (UInt, Bool) = {
      val expValue = extExp.raw.asSInt
      val isSubnormal = Bool()
      val ieeeExp = UInt(11 bits) // Use maximum width, resize later if needed

      when(isDouble) {
        val minNormal = S(-1022, extExp.bitWidth bits)
        when(expValue < minNormal) {
          // Subnormal range
          isSubnormal := True
          ieeeExp := U(0)
        } otherwise {
          // Normal range
          isSubnormal := False
          ieeeExp := (expValue + 1023).asUInt.resize(11)
        }
      } otherwise {
        val minNormal = S(-126, extExp.bitWidth bits)
        when(expValue < minNormal) {
          // Subnormal range
          isSubnormal := True
          ieeeExp := U(0)
        } otherwise {
          // Normal range
          isSubnormal := False
          ieeeExp := (expValue + 127).asUInt.resize(8)
        }
      }

      (ieeeExp, isSubnormal)
    }
  }

  /** IEEE 754 compliant rounding helper */
  object IEEERounding {

    /** Apply IEEE 754 rounding to AFix value */
    def round(value: AFix, mode: Bits, isDouble: Bool): AFix = {
      // Simplified rounding - just return fixTo for now
      // TODO: Implement proper IEEE 754 rounding modes
      value
    }

    /** Get SpinalHDL RoundType from IEEE 754 mode */
    def toRoundType(mode: Bits): RoundType = {
      // Simplified - always return default for now
      // TODO: Implement proper mode selection
      RoundType.ROUNDTOEVEN
    }
  }

  /** Extract sign, exponent and mantissa from IEEE 754 bits */
  def extractFields(bits: Bits, isDouble: Bool): (Bool, AFix, AFix) = {
    val sign = bits.msb

    val exponent = FloatingPoint.exponent(isDouble)
    val mantissa = FloatingPoint.mantissa(isDouble)

    when(isDouble) {
      exponent.raw := bits(62 downto 52).asSInt.resize(exponent.bitWidth).asBits
      val expVal = bits(62 downto 52).asUInt
      when(expVal === 0) {
        // Denormal - no implicit 1
        mantissa.raw := (U(0, 1 bit) ## bits(51 downto 0).asUInt).asBits
      } otherwise {
        // Normal - implicit 1
        mantissa.raw := (U(1, 1 bit) ## bits(51 downto 0).asUInt).asBits
      }
    } otherwise {
      exponent.raw := bits(30 downto 23).asSInt.resize(exponent.bitWidth).asBits
      val expVal = bits(30 downto 23).asUInt
      when(expVal === 0) {
        // Denormal - no implicit 1
        mantissa.raw := (U(0, 1 bit) ## bits(22 downto 0).asUInt ## U(0, 29 bits)).asBits
      } otherwise {
        // Normal - implicit 1
        mantissa.raw := (U(1, 1 bit) ## bits(22 downto 0).asUInt ## U(0, 29 bits)).asBits
      }
    }

    (sign, exponent, mantissa)
  }

  /** Pack fields into IEEE 754 format */
  def packFields(sign: Bool, exponent: AFix, mantissa: AFix, isDouble: Bool): Bits = {
    val result = Bits(64 bits)

    // Extract mantissa bits (remove implicit 1 for normalized numbers)
    val expInt = exponent.raw.asUInt
    val isNormal = expInt =/= 0

    when(isDouble) {
      val mantBits = Mux(
        isNormal,
        mantissa.raw(52 downto 0), // Remove implicit 1
        mantissa.raw(52 downto 0) // Keep as-is for denormals
      )
      result := sign ## exponent.raw(10 downto 0) ## mantBits(51 downto 0)
    } otherwise {
      val mantBits = Mux(
        isNormal,
        mantissa.raw(52 downto 30), // Remove implicit 1, take upper bits
        mantissa.raw(52 downto 30) // Keep as-is for denormals
      )
      result := B(0, 32 bits) ## sign ## exponent.raw(7 downto 0) ## mantBits(22 downto 0)
    }

    result
  }

  /** Check for special values using AFix */
  def isZero(exponent: AFix, mantissa: AFix): Bool = {
    exponent.raw === 0 && mantissa.raw === 0
  }

  def isInf(exponent: AFix, mantissa: AFix, isDouble: Bool): Bool = {
    val maxExp = Mux(isDouble, U(0x7ff, 11 bits), U(0xff, 8 bits))
    exponent.raw === maxExp.asBits.resized && mantissa.raw(mantissa.bitWidth - 2 downto 0) === 0
  }

  def isNaN(exponent: AFix, mantissa: AFix, isDouble: Bool): Bool = {
    val maxExp = Mux(isDouble, U(0x7ff, 11 bits), U(0xff, 8 bits))
    exponent.raw === maxExp.asBits.resized && mantissa.raw(mantissa.bitWidth - 2 downto 0) =/= 0
  }

  def isDenormal(exponent: AFix, mantissa: AFix): Bool = {
    exponent.raw === 0 && mantissa.raw =/= 0
  }

  /** Normalize an AFix mantissa and adjust exponent */
  def normalize(mantissa: AFix, exponent: AFix, isDouble: Bool): (AFix, AFix) = {
    val normMant = cloneOf(mantissa)
    val normExp = cloneOf(exponent)

    // Find leading one position
    val leadingZeros = LeadingZeros(mantissa.raw)

    // Shift mantissa to normalize
    normMant.raw := mantissa.raw << leadingZeros

    // Adjust exponent
    normExp.raw := (exponent.raw.asSInt - leadingZeros.asSInt).asBits

    (normMant, normExp)
  }

  /** Leading zeros counter for normalization */
  def LeadingZeros(bits: Bits): UInt = {
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

  /** Rounding mode helpers */
  object RoundingMode {
    val NEAREST_EVEN = U(0, 2 bits)
    val TOWARD_ZERO = U(1, 2 bits)
    val TOWARD_POS_INF = U(2, 2 bits)
    val TOWARD_NEG_INF = U(3, 2 bits)
  }

  /** Exception flags */
  object ExceptionFlags {
    val INVALID_OP = 0
    val DIV_BY_ZERO = 1
    val OVERFLOW = 2
    val UNDERFLOW = 3
    val INEXACT = 4
  }

  /** AFix arithmetic helpers for clean construction */
  object AfixHelpers {

    /** Create AFix from integer with specified format */
    def fromInt(value: Int, format: AFix): AFix = {
      val result = cloneOf(format)
      result.raw := S(value, result.bitWidth bits).asBits
      result
    }

    /** Create AFix from SInt with specified format */
    def fromSInt(value: SInt, format: AFix): AFix = {
      val result = cloneOf(format)
      result.raw := value.resize(result.bitWidth).asBits
      result
    }

    /** Create AFix from UInt with specified format */
    def fromUInt(value: UInt, format: AFix): AFix = {
      val result = cloneOf(format)
      result.raw := value.resize(result.bitWidth).asBits
      result
    }

    /** Create zero AFix with specified format */
    def zero(format: AFix): AFix = {
      val result = cloneOf(format)
      result.raw := B(0, result.bitWidth bits)
      result
    }

    /** Create one AFix with specified format */
    def one(format: AFix): AFix = {
      val result = cloneOf(format)
      // Set the LSB of the integer part to 1
      val intBits = format.maxExp + 1
      result.raw := (U(1, intBits bits) ## U(0, (result.bitWidth - intBits) bits)).asBits
      result
    }

    /** Create AFix constant 2.0 with specified format */
    def two(format: AFix): AFix = {
      val result = cloneOf(format)
      // Set bit 1 of the integer part to 1 (2.0)
      val intBits = format.maxExp + 1
      if (intBits >= 2) {
        result.raw := (U(2, intBits bits) ## U(0, (result.bitWidth - intBits) bits)).asBits
      } else {
        // Format can't represent 2.0, saturate to maximum
        result.raw := B((1 << result.bitWidth) - 1, result.bitWidth bits)
      }
      result
    }
  }

  /** Leading zeros counter optimized for AFix */
  def leadingZerosAFix(value: AFix): UInt = {
    val bits = value.asBits
    val width = bits.getWidth
    val zeros = UInt(log2Up(width + 1) bits)

    // Use priority encoder for efficient leading zero detection
    val found = Bool()
    found := False
    zeros := width

    for (i <- 0 until width) {
      when(!found && bits(width - 1 - i)) {
        zeros := U(i)
        found := True
      }
    }
    zeros
  }

  /** Sign manipulation */
  def abs(bits: Bits, isDouble: Bool): Bits = {
    val result = cloneOf(bits)
    when(isDouble) {
      result := False ## bits(62 downto 0)
    } otherwise {
      result := B(0, 32 bits) ## False ## bits(30 downto 0)
    }
    result
  }

  def negate(bits: Bits, isDouble: Bool): Bits = {
    val result = cloneOf(bits)
    when(isDouble) {
      result := ~bits(63) ## bits(62 downto 0)
    } otherwise {
      result := B(0, 32 bits) ## ~bits(31) ## bits(30 downto 0)
    }
    result
  }

  /** Legacy compatibility - keep existing functions working */
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

  def packIeee754(sign: Bool, exponent: UInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth == 52, "Mantissa for packIeee754 must be 52 bits")
    (sign ## exponent.asBits.resize(11) ## mantissa).resize(64)
  }

  def packIeee754Single(sign: Bool, exponent: UInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth >= 23, "Mantissa for packIeee754Single must be at least 23 bits")
    (sign ## exponent.asBits(7 downto 0) ## mantissa(22 downto 0)).resize(32)
  }

  case class Ieee754Class(isNaN: Bool, isInfinity: Bool, isDenormal: Bool, isZero: Bool, sign: Bool)

  def classifyIeee754(value: Bits): Ieee754Class = {
    val parsed = parseIeee754(value)
    val isNaN = parsed.exponent === 0x7ff && parsed.mantissa =/= 0
    val isInfinity = parsed.exponent === 0x7ff && parsed.mantissa === 0
    val isDenormal = parsed.exponent === 0 && parsed.mantissa =/= 0
    val isZero = parsed.exponent === 0 && parsed.mantissa === 0
    Ieee754Class(isNaN, isInfinity, isDenormal, isZero, parsed.sign)
  }

  def genNaN: Bits = B"0111111111111000000000000000000000000000000000000000000000000000"
  def genInfinity(sign: Bool): Bits = sign ## B"11111111111" ## B(0, 52 bits)
}

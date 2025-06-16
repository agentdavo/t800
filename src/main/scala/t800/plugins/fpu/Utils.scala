package t800.plugins.fpu

import spinal.core._
import spinal.lib._

object Utils {
  case class Ieee754Format(sign: Bool, exponent: SInt, mantissa: Bits)

  def parseIeee754(value: Bits): Ieee754Format = {
    assert(value.getWidth == 64, "Input to parseIeee754 must be 64 bits")
    val sign = value(63)
    val exponent = value(62 downto 52).asSInt
    val mantissa = value(51 downto 0)
    Ieee754Format(sign, exponent, mantissa)
  }

  def parseIeee754Single(value: Bits): Ieee754Format = {
    assert(value.getWidth == 32, "Input to parseIeee754Single must be 32 bits")
    val sign = value(31)
    val exponent = value(30 downto 23).asSInt
    val mantissa = value(22 downto 0) ## B(0, 29 bits)
    Ieee754Format(sign, exponent, mantissa)
  }

  /**
    * Parse a 32-bit IEEE‑754 value keeping the original 23-bit mantissa.
    */
  def parseIeee75432(value: Bits): Ieee754Format = {
    assert(value.getWidth == 32, "Input to parseIeee75432 must be 32 bits")
    val sign = value(31)
    val exponent = value(30 downto 23).asSInt
    val mantissa = value(22 downto 0)
    Ieee754Format(sign, exponent, mantissa)
  }

  def packIeee754(sign: Bool, exponent: SInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth == 52, "Mantissa for packIeee754 must be 52 bits")
    sign ## exponent.asBits(10 downto 0) ## mantissa
  }

  def packIeee754Single(sign: Bool, exponent: SInt, mantissa: Bits): Bits = {
    assert(mantissa.getWidth >= 23, "Mantissa for packIeee754Single must be at least 23 bits")
    sign ## exponent.asBits(7 downto 0) ## mantissa(22 downto 0)
  }

  def roundIeee754(mantissa: AFix, mode: Bits): AFix = {
    val roundType = mode.mux(
      0 -> RoundType.ROUNDTOEVEN, // fprn
      1 -> RoundType.FLOORTOZERO, // fprz
      2 -> RoundType.CEIL,        // fprp
      3 -> RoundType.FLOOR        // fprm
    )
    mantissa.round(0, roundType)
  }

  case class Ieee754Class(isNaN: Bool, isInfinity: Bool, isDenormal: Bool, isZero: Bool, sign: Bool)

  def classifyIeee754(value: Bits): Ieee754Class = {
    val parsed = parseIeee754(value)
    val isNaN = parsed.exponent === 0x7FF && parsed.mantissa =/= 0
    val isInfinity = parsed.exponent === 0x7FF && parsed.mantissa === 0
    val isDenormal = parsed.exponent === 0 && parsed.mantissa =/= 0
    val isZero = parsed.exponent === 0 && parsed.mantissa === 0
    Ieee754Class(isNaN, isInfinity, isDenormal, isZero, parsed.sign)
  }

  def classifyIeee754Single(value: Bits): Ieee754Class = {
    val parsed = parseIeee754Single(value)
    val isNaN = parsed.exponent === 0xFF && parsed.mantissa(51 downto 29) =/= 0
    val isInfinity = parsed.exponent === 0xFF && parsed.mantissa(51 downto 29) === 0
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
    val afix = AFix(value.asSInt, 64 bit, 0 exp)
    val parsed = parseIeee754(afix.raw)
    val sign = parsed.sign
    val exponent = parsed.exponent - 1023 + 127
    val mantissa = roundIeee754(AFix(parsed.mantissa.asUInt, 52 bit, 0 exp), roundMode).raw(51 downto 29)
    packIeee754Single(sign, exponent, mantissa)
  }

  def realToInt32(value: Bits): SInt = {
    val afix = AFix(value.asSInt, 64 bit, 0 exp)
    afix.asSInt.resize(32)
  }

  def int32ToReal32(value: SInt): Bits = {
    val afix = AFix(value, 0 exp)
    val sign = afix.isNegative()
    val exponent = 127
    val mantissa = afix.raw(31 downto 9).resize(23)
    packIeee754Single(sign, exponent, mantissa)
  }

  def int32ToReal64(value: SInt): Bits = {
    val afix = AFix(value, 0 exp)
    val sign = afix.isNegative()
    val exponent = 1023
    val mantissa = afix.raw(31 downto 0) ## B(0, 20 bits)
    packIeee754(sign, exponent, mantissa)
  }

  def bit32ToReal64(value: Bits): Bits = {
    val afix = AFix(value.asUInt, 32 bit, 0 exp)
    val sign = False
    val exponent = 1023
    val mantissa = afix.raw(31 downto 0) ## B(0, 20 bits)
    packIeee754(sign, exponent, mantissa)
  }
}

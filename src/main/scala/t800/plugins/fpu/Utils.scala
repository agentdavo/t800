package t800.plugins.fpu

import spinal.core._
import spinal.lib._

object Utils {
  case class Ieee754Format(sign: Bool, exponent: SInt, mantissa: Bits)

  def parseIeee754(value: Bits): Ieee754Format = {
    val sign = value(63)
    val exponent = value(62 downto 52).asSInt
    val mantissa = value(51 downto 0)
    Ieee754Format(sign, exponent, mantissa)
  }

  def packIeee754(sign: Bool, exponent: SInt, mantissa: Bits): Bits = {
    sign ## exponent.asBits ## mantissa
  }

  def roundIeee754(mantissa: AFix, mode: Bits): AFix = {
    val roundType = mode.mux(
      0 -> RoundType.ROUNDTOEVEN,
      1 -> RoundType.FLOORTOZERO,
      2 -> RoundType.CEIL,
      3 -> RoundType.FLOOR
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

  def genNaN: Bits = B"0111111111111000000000000000000000000000000000000000000000000000"
  def genInfinity(sign: Bool): Bits = sign ## B"11111111111" ## B(0, 52 bits)

  def real32ToReal64(value: Bits): Bits = {
    val afix = AFix(value.asUInt, 32 bit, 0 exp)
    val parsed = parseIeee754(afix.raw)
    val sign = parsed.sign
    val exponent = parsed.exponent + 1023 - 127
    val mantissa = parsed.mantissa(22 downto 0) @@ B(0, 29 bits)
    packIeee754(sign, exponent, mantissa)
  }

  def real64ToReal32(value: Bits, roundMode: Bits): Bits = {
    val afix = AFix(value.asSInt, 64 bit, 0 exp)
    val parsed = parseIeee754(afix.raw)
    val sign = parsed.sign
    val exponent = parsed.exponent - 1023 + 127
    val mantissa = roundIeee754(AFix(parsed.mantissa.asUInt, 52 bit, 0 exp), roundMode).raw(51 downto 29)
    sign ## exponent.asBits ## mantissa
  }

  def realToInt32(value: Bits): SInt = {
    val afix = AFix(value.asSInt, 64 bit, 0 exp)
    afix.asSInt
  }

  def int32ToReal32(value: SInt): Bits = {
    val afix = AFix(value, 0 exp)
    packIeee754(afix.isNegative(), 127, afix.raw(31 downto 9))
  }
}

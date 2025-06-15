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

  def roundIeee754(mantissa: Bits, mode: Bits): Bits = {
    // Placeholder: Implement nearest, zero, positive, minus modes
    mantissa
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
    val sign = value(31)
    val exponent = value(30 downto 23).asSInt + 1023 - 127
    val mantissa = value(22 downto 0) @@ B(0, 29 bits)
    packIeee754(sign, exponent, mantissa)
  }

  def real64ToReal32(value: Bits, roundMode: Bits): Bits = {
    val parsed = parseIeee754(value)
    val sign = parsed.sign
    val exponent = parsed.exponent - 1023 + 127
    val mantissa = parsed.mantissa(51 downto 29)
    val rounded = roundIeee754(mantissa, roundMode)
    sign ## exponent.asBits ## rounded
  }

  def realToInt32(value: Bits): SInt = {
    // Placeholder
    value.asSInt
  }

  def int32ToReal32(value: SInt): Bits = {
    // Placeholder
    B(0, 32 bits)
  }
}

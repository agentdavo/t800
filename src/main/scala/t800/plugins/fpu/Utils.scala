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

  def roundIeee754(mantissa: Bits): Bits = {
    // Placeholder for rounding logic
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
}

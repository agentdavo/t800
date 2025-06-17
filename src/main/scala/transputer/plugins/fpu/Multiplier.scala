package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import transputer.plugins.fpu.Utils._

class FpuMultiplier extends Area {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val isAbs = in Bool ()
    val isSingle = in Bool () // Single-precision flag
    val roundingMode = in Bits (2 bits)
    val result = out Bits (64 bits)
    val resultAfix = out(AFix(UQ(56, 0)))
    val cycles = out UInt (2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)

  // Mantissas with the implicit leading 1
  val mant1 = ((op1Parsed.exponent === 0) ? B"0" | B"1") ## op1Parsed.mantissa
  val mant2 = ((op2Parsed.exponent === 0) ? B"0" | B"1") ## op2Parsed.mantissa

  val op1Afix = AFix(mant1.asUInt.resize(53), 0 exp)
  val op2Afix = AFix(mant2.asUInt.resize(53), 0 exp)

  when(io.isAbs) {
    io.result := packIeee754(False, op1Parsed.exponent, op1Parsed.mantissa)
    io.resultAfix := op1Afix
    io.cycles := U(1)
  } otherwise {
    val product = op1Afix * op2Afix
    val prodMsb = product.raw(105)

    val shifted = Mux(prodMsb, product >>| 53, product >>| 52)
    val exponent = (op1Parsed.exponent.asSInt + op2Parsed.exponent.asSInt - 1023) + prodMsb.asSInt

    val rounded = AFix(shifted.raw(52 downto 0).asUInt.resize(53), 0 exp).round(0)

    val sign = op1Parsed.sign ^ op2Parsed.sign
    io.result := packIeee754(sign, exponent.asUInt.resize(11), rounded.raw(51 downto 0))
    io.resultAfix := rounded
    io.cycles := Mux(io.isSingle, U(2), U(3))
  }
}

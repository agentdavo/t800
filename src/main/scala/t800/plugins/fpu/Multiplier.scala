package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.plugins.fpu.Utils._

class FpuMultiplier extends Area {
  val io = new Bundle {
    val op1 = in Bits(64 bits)
    val op2 = in Bits(64 bits)
    val isAbs = in Bool()
    val isSingle = in Bool() // Single-precision flag
    val roundingMode = in Bits(2 bits)
    val result = out Bits(64 bits)
    val resultAfix = out AFix(UQ(56 bit, 0 bit))
    val cycles = out UInt(2 bits)
  }

  // Parse IEEE-754 operands
  val op1Parsed = parseIeee754(io.op1)
  val op2Parsed = parseIeee754(io.op2)
  val op1Afix = AFix(op1Parsed.mantissa.asUInt, 56 bit, 0 exp)
  val op2Afix = AFix(op2Parsed.mantissa.asUInt, 56 bit, 0 exp)

  when(io.isAbs) {
    io.result := packIeee754(False, op1Parsed.exponent, op1Parsed.mantissa)
    io.resultAfix := op1Afix
    io.cycles := 1
  } otherwise {
    // Booth recoding (radix-4)
    val boothDigits = Vec(SInt(3 bits), if (io.isSingle) 13 else 27)
    for (i <- 0 until boothDigits.length) {
      val b2i = op2Afix.raw(2 * i)
      val b2i1 = if (i < (if (io.isSingle) 12 else 26)) op2Afix.raw(2 * i + 1) else False
      val b2i2 = if (i < (if (io.isSingle) 11 else 25)) op2Afix.raw(2 * i + 2) else False
      boothDigits(i) := (b2i1.asSInt + b2i.asSInt - (b2i2.asSInt << 1)).resize(3)
    }

    // Partial product generation
    val partialProducts = Vec(AFix(UQ(56 bit, 0 bit)), if (io.isSingle) 14 else 28)
    for (i <- 0 until boothDigits.length) {
      partialProducts(i) := MuxCase(AFix(0, 56 bit, 0 exp), Seq(
        (boothDigits(i) === 0) -> AFix(0, 56 bit, 0 exp),
        (boothDigits(i) === 1) -> (op1Afix <<| (2 * i)),
        (boothDigits(i) === 2) -> (op1Afix <<| (2 * i + 1)),
        (boothDigits(i) === -1) -> (-op1Afix <<| (2 * i)),
        (boothDigits(i) === -2) -> (-op1Afix <<| (2 * i + 1))
      ))
    }
    partialProducts(boothDigits.length) := AFix(0, 56 bit, 0 exp) // Correction bits (placeholder)

    // Carry-save accumulation (twin 7:2 arrays)
    val carrySave1 = Reg(AFix(UQ(56 bit, 0 bit))) init 0
    val carrySave2 = Reg(AFix(UQ(56 bit, 0 bit))) init 0
    val passCount = Reg(UInt(1 bit)) init 0
    when(io.isSingle || passCount === 0) {
      carrySave1 := partialProducts.take(7).reduce(_ + _)
      carrySave2 := partialProducts.drop(7).take(7).reduce(_ + _)
      passCount := passCount + 1
    } elsewhen(passCount === 1) {
      carrySave1 := carrySave1 + carrySave2
      carrySave2 := partialProducts.drop(14).take(14).reduce(_ + _)
      passCount := 0
    }

    // Final carry-propagate addition
    val product = carrySave1 + carrySave2
    val productSign = op1Parsed.sign ^ op2Parsed.sign
    val productExponent = op1Parsed.exponent + op2Parsed.exponent - 1023
    val needShift = product.raw(104)

    // Parallel rounding/normalization
    val p = product
    val pPlusOne = product + 1
    val pPlusTwo = product + 2
    val pShifted = product >>| 1
    val roundType = io.roundingMode.mux(
      0 -> RoundType.ROUNDTOEVEN,
      1 -> RoundType.FLOORTOZERO,
      2 -> RoundType.CEIL,
      3 -> RoundType.FLOOR
    )
    val roundedMantissa = MuxCase(p, Seq(
      (!needShift && !io.isSingle) -> p.round(0, roundType),
      (needShift && !io.isSingle) -> pShifted.round(0, roundType),
      (!needShift && io.isSingle) -> pPlusOne.round(0, roundType),
      (needShift && io.isSingle) -> pPlusTwo.round(0, roundType)
    )).sat(2 pow 52 - 1, 0)
    val finalExponent = productExponent + (needShift ? 1 | 0)

    io.result := packIeee754(productSign, finalExponent, roundedMantissa.raw)
    io.resultAfix := roundedMantissa
    io.cycles := Mux(io.isSingle, 2, 3)
  }
}

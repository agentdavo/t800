package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.Opcode

class FpuVCU extends Component {
  val io = new Bundle {
    val op1 = in Bits (64 bits)
    val op2 = in Bits (64 bits)
    val opcode = in Bits (8 bits)
    val isSpecial = out Bool ()
    val specialResult = out Bits (64 bits)
    val trapEnable = out Bool ()
    val trapType = out UInt (4 bits)
    val comparisonResult = out Bool ()
  }

  private def classify(value: Bits) = new Area {
    val sign = value(63)
    val exponent = value(62 downto 52)
    val mantissa = value(51 downto 0)
    val isNaN = (exponent === 0x7ff) && (mantissa =/= 0)
    val isInfinity = (exponent === 0x7ff) && (mantissa === 0)
    val isDenormal = (exponent === 0) && (mantissa =/= 0)
    val isZero = (exponent === 0) && (mantissa === 0)
  }

  val op1Class = classify(io.op1)
  val op2Class = classify(io.op2)

  val isNaN = op1Class.isNaN || op2Class.isNaN
  val isInfinity = op1Class.isInfinity || op2Class.isInfinity
  val isDenormal = op1Class.isDenormal || op2Class.isDenormal
  val isZero = op1Class.isZero || op2Class.isZero

  private val posInf = B(BigInt("7ff0000000000000", 16), 64 bits)
  private val negInf = B(BigInt("fff0000000000000", 16), 64 bits)
  private val nanVal = B(BigInt("7ff8000000000000", 16), 64 bits)
  private def canonicalNaN: Bits = nanVal
  private def signedInfinity(sign: Bool): Bits = Mux(sign, negInf, posInf)
  private def signedZero(sign: Bool): Bits = sign ## B(0, 63 bits)

  private def normalizeDenormal(value: Bits): Bits = {
    val sign = value(63)
    val mantissa = value(51 downto 0)
    val shift = CountLeadingZeroes(mantissa)
    val exponent = (U(1, 11 bits) - shift.resize(11)).asBits
    val normMantissa = (mantissa |<< shift)(51 downto 0)
    sign ## exponent ## normMantissa
  }

  io.isSpecial := isNaN || isInfinity || isDenormal || isZero
  io.trapEnable := isNaN || isDenormal || (io.opcode === B(0x83, 8 bits) && isInfinity)
  io.trapType := U(0)
  when(io.trapEnable) { io.trapType := U(0x4) }
  io.specialResult := Mux(
    isNaN,
    canonicalNaN,
    Mux(
      isInfinity,
      signedInfinity(op1Class.sign || op2Class.sign),
      Mux(
        isDenormal,
        normalizeDenormal(Mux(op1Class.isDenormal, io.op1, io.op2)),
        signedZero(op1Class.sign || op2Class.sign)
      )
    )
  )

  // Ordering transform for direct UInt comparison
  val signMask = B(BigInt("8000000000000000", 16), 64 bits)
  val op1Ordered = Mux(io.op1(63), ~io.op1.asUInt, io.op1.asUInt ^ signMask.asUInt)
  val op2Ordered = Mux(io.op2(63), ~io.op2.asUInt, io.op2.asUInt ^ signMask.asUInt)
  io.comparisonResult := False
  when(io.opcode === B(0x91, 8 bits)) {
    io.comparisonResult := isNaN
  } elsewhen (io.opcode === B(0x92, 8 bits)) {
    io.comparisonResult := !isNaN && !isInfinity
  } elsewhen (io.opcode === B(0x93, 8 bits)) {
    io.comparisonResult := !isInfinity
  } elsewhen (!io.isSpecial) {
    when(io.opcode === B(0x94, 8 bits)) { io.comparisonResult := op1Ordered > op2Ordered }
    when(io.opcode === B(0x95, 8 bits)) { io.comparisonResult := io.op1.asUInt === io.op2.asUInt }
    when(io.opcode === B(0x97, 8 bits)) { io.comparisonResult := op1Ordered >= op2Ordered }
    when(io.opcode === B(0x9b, 8 bits)) {
      io.comparisonResult := op1Ordered > op2Ordered || op2Ordered > op1Ordered
    }
  }
}

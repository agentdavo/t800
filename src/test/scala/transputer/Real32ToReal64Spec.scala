package t800

import org.scalatest.funsuite.AnyFunSuite

class Real32ToReal64Spec extends AnyFunSuite {
  private def convert(bits: Int): Long = {
    val sign = (bits >>> 31) & 0x1
    val exponent = (bits >>> 23) & 0xff
    val mantissa = bits & 0x7fffff
    val exp64 = exponent + 1023 - 127
    val mant64 = mantissa.toLong << 29
    (sign.toLong << 63) | (exp64.toLong << 52) | mant64
  }

  test("convert several floats") {
    val samples = Seq(
      0x3f800000 -> 0x3ff0000000000000L,
      0xbf800000 -> 0xbff0000000000000L,
      0x41200000 -> 0x4024000000000000L,
      0x00000000 -> 0x3800000000000000L,
      0x7f800000 -> 0x47f0000000000000L
    )
    for ((in, expected) <- samples) {
      assert(convert(in) == expected)
    }
  }
}

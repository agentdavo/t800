package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import transputer.Opcode

case class FpCmd() extends Bundle {
  val op = FpOp()
  val a = Bits(64 bits) // Operand A (FPA)
  val b = Bits(64 bits) // Operand B (FPB)
  val c = Bits(64 bits) // Operand C (FPC)
  val rounding = Bits(2 bits) // Rounding mode
  val isDouble = Bool() // Single/Double precision

  // Convenience methods for execution units
  def sub: Bool = op === FpOp.FPSUB
}

/** T9000 FPU Operations from Tables 6.32-6.37 */
object FpOp extends SpinalEnum(binarySequential) {

  // Table 6.32: Load/Store Operations
  val FPLDBS, FPLDBD = newElement() // Load single/double from workspace
  val FPLDNLS, FPLDNLD = newElement() // Load non-local single/double
  val FPSTSNL, FPSTDNL = newElement() // Store non-local single/double

  // Table 6.33: Arithmetic Operations
  val FPADD, FPSUB = newElement() // Add/Subtract
  val FPMUL = newElement() // Multiply
  val FPDIV = newElement() // Divide
  val FPREMFIRST, FPREMSTEP = newElement() // Remainder operations
  val FPSQRT = newElement() // Square root
  val FPABS, FPNEG = newElement() // Absolute value, Negate
  val FPLDEXP, FPNORM = newElement() // Exponent manipulation

  // Table 6.34: Comparison Operations
  val FPEQ, FPGT, FPLT = newElement() // Equal, Greater than, Less than
  val FPORDERED, FPUNORDERED = newElement() // NaN detection

  // Table 6.35: Conversion Operations
  val FPI32TOR32, FPI32TOR64 = newElement() // Int to float conversions
  val FPR32TOI32, FPR64TOI32 = newElement() // Float to int conversions
  val FPR32TOR64, FPR64TOR32 = newElement() // Precision conversions
  val FPINT, FPNINT = newElement() // Round to integer

  // Table 6.36: Rounding Mode Control
  val FPROUNDN = newElement() // Round to nearest even (00)
  val FPROUNDZ = newElement() // Round toward zero (01)
  val FPROUNDP = newElement() // Round toward +∞ (10)
  val FPROUNDM = newElement() // Round toward -∞ (11)

  // Table 6.37: Control Operations
  val FPUCHK = newElement() // Check exceptions
  val FPUCLRERR = newElement() // Clear error flags
  val FPUSETERR = newElement() // Set error flags
  val FPUSTATUS = newElement() // Store status to memory
  val FPUSTATUSR = newElement() // Load status from memory
  val FPSTTEST = newElement() // Self-test

  // Stack operations (implicit in T9000)
  val FPDUP = newElement() // Duplicate top of stack
  val FPREV = newElement() // Reverse top two items
  val FPPOP = newElement() // Pop from stack

  // Special constants
  val FPLDZERODB = newElement() // Load +0.0 double
  val FPLDZEROSN = newElement() // Load +0.0 single

  // No operation
  val FPNOP = newElement()

  // Legacy/compatibility (for minimal builds)
  val NONE = newElement()

  // Helper method to check if operation is a comparison
  def isComparison(op: FpOp.C): Bool = {
    op === FPEQ || op === FPGT || op === FPLT ||
    op === FPORDERED || op === FPUNORDERED
  }

  // Helper method to check if operation modifies stack
  def isStackOp(op: FpOp.C): Bool = {
    op === FPDUP || op === FPREV || op === FPPOP
  }

  // Helper method to determine if operation uses B register
  def usesBReg(op: FpOp.C): Bool = {
    op === FPADD || op === FPSUB || op === FPMUL || op === FPDIV ||
    op === FPEQ || op === FPGT || op === FPLT
  }
}

/** FPU Response bundle */
case class FpRsp() extends Bundle {
  val result = Bits(64 bits) // Result data
  val exceptions = Bits(5 bits) // IEEE 754 exception flags
  val busy = Bool() // Multi-cycle operation in progress
}

/** Additional command/response bundles for specific units */

/** Adder command */
case class AdderCmd() extends Bundle {
  val a = Bits(64 bits)
  val b = Bits(64 bits)
  val sub = Bool() // True for subtraction
  val rounding = Bits(2 bits)
  val isDouble = Bool()
}

/** Multiplier command */
case class MultiplierCmd() extends Bundle {
  val a = Bits(64 bits)
  val b = Bits(64 bits)
  val rounding = Bits(2 bits)
  val isDouble = Bool()
}

/** Divider command */
case class DividerCmd() extends Bundle {
  val a = Bits(64 bits) // Dividend (or operand for sqrt)
  val b = Bits(64 bits) // Divisor (unused for sqrt)
  val isSqrt = Bool()
  val rounding = Bits(2 bits)
  val isDouble = Bool()
}

/** Memory command */
case class MemoryCmd() extends Bundle {
  val addr = UInt(32 bits)
  val data = Bits(64 bits)
  val isStore = Bool()
  val isDouble = Bool()
}

/** Memory response */
case class MemoryRsp() extends Bundle {
  val data = Bits(64 bits)
  val error = Bool()
}

/** Control command */
case class ControlCmd() extends Bundle {
  val op = FpOp()
  val statusIn = Bits(32 bits)
  val roundingMode = Bits(2 bits)
}

/** Control response */
case class ControlRsp() extends Bundle {
  val statusOut = Bits(32 bits)
}

/** Stack command */
case class StackCmd() extends Bundle {
  val op = FpOp()
}

/** Stack response */
case class StackRsp() extends Bundle {
  val result = Bits(64 bits)
  val stackModified = Bool()
}

/** Special operations command (for operations not handled by main units) */
case class SpecialCmd() extends Bundle {
  val op = FpOp()
  val a = Bits(64 bits)
  val b = Bits(64 bits)
}

/** Comparison command */
case class ComparisonCmd() extends Bundle {
  val op = ComparisonOp()
  val a = Bits(64 bits)
  val b = Bits(64 bits)
  val isDouble = Bool()
}

/** Comparison operations */
object ComparisonOp extends SpinalEnum {
  val EQ, GT, LT, ORDERED, UNORDERED = newElement()
}

/** Comparison response */
case class ComparisonRsp() extends Bundle {
  val result = Bool()
  val invalid = Bool() // Invalid operation exception
}

/** Conversion operations */
object ConversionOp extends SpinalEnum {
  val I32TOR32, I32TOR64 = newElement() // Integer to float
  val R32TOI32, R64TOI32 = newElement() // Float to integer
  val R32TOR64, R64TOR32 = newElement() // Precision conversion
  val INT, NINT = newElement() // Round to integer
}

/** Conversion command */
case class ConversionCmd() extends Bundle {
  val op = ConversionOp()
  val operand = Bits(64 bits)
  val rounding = Bits(2 bits)
  val isDouble = Bool() // For INT/NINT operations
}

/** Conversion response */
case class ConversionRsp() extends Bundle {
  val result = Bits(64 bits)
  val exceptions = Bits(5 bits)
}

/** IEEE 754 Floating-point number representation with extended exponent
  *
  * Uses extended exponent format to handle subnormal numbers as if they were normal, simplifying
  * arithmetic operations. The extended exponent covers the full range from the most negative
  * subnormal to the maximum normal exponent.
  */
case class FpNumber() extends Bundle {
  val sign = Bool()
  val exponent = new AFix(maxRaw = 1023, minRaw = -1022, exp = 0) // Standard IEEE 754 exponent range
  val mantissa = new AFix(maxRaw = 1, minRaw = 0, exp = -52) // Standard IEEE 754 mantissa precision

  // Special value detection using standard IEEE format
  def isZero: Bool = {
    mantissa.raw === 0
  }

  def isInf: Bool = {
    // Check if this represents an infinity in IEEE format
    exponent.raw === 0x7ff && mantissa.raw === 0
  }

  def isNaN: Bool = {
    // Check if this represents a NaN in IEEE format
    exponent.raw === 0x7ff && mantissa.raw =/= 0
  }

  def isSubnormal: Bool = {
    // Check if this represents a subnormal number
    exponent.raw === 0 && mantissa.raw =/= 0
  }

  // Parse from IEEE 754 bits using extended format conversion
  def fromBits(bits: Bits, isDouble: Bool): Unit = {
    sign := bits.msb
    exponent := FpuUtils.FloatingPoint.toExtendedExponent(bits, isDouble)

    // Extract mantissa and normalize subnormals
    when(isDouble) {
      val expBits = bits(62 downto 52).asUInt
      val mantBits = bits(51 downto 0)

      when(expBits === 0) {
        // Subnormal: normalize the mantissa (shift until leading 1 is found)
        val leadingZeros = FpuUtils.LeadingZeros(mantBits)
        val normalizedMant = mantBits << (leadingZeros + 1) // +1 to remove the found leading 1
        mantissa.raw := (U(1, 1 bit) ## normalizedMant ## U(0, mantissa.bitWidth - 53 bits)).asBits
      } otherwise {
        // Normal: mantissa has implicit leading 1
        mantissa.raw := (U(1, 1 bit) ## mantBits ## U(0, mantissa.bitWidth - 53 bits)).asBits
      }
    } otherwise {
      val expBits = bits(30 downto 23).asUInt
      val mantBits = bits(22 downto 0)

      when(expBits === 0) {
        // Subnormal: normalize the mantissa
        val leadingZeros = FpuUtils.LeadingZeros(mantBits)
        val normalizedMant = mantBits << (leadingZeros + 1) // +1 to remove the found leading 1
        mantissa.raw := (U(1, 1 bit) ## normalizedMant ## U(0, mantissa.bitWidth - 24 bits)).asBits
      } otherwise {
        // Normal: mantissa has implicit leading 1
        mantissa.raw := (U(1, 1 bit) ## mantBits ## U(0, mantissa.bitWidth - 24 bits)).asBits
      }
    }
  }

  // Convert back to IEEE 754 bits
  def toBits(isDouble: Bool): Bits = {
    val result = Bits(64 bits)

    // Convert extended exponent back to IEEE format
    val (ieeeExp, isSubnorm) = FpuUtils.FloatingPoint.fromExtendedExponent(exponent, isDouble)

    when(isDouble) {
      when(isSubnorm) {
        // Subnormal: denormalize the mantissa
        val shiftAmount = U(-1022) - exponent.raw.asSInt.asUInt + 1
        val denormMant = mantissa.raw.asUInt >> shiftAmount
        result := sign ## U(0, 11 bits).asBits ## denormMant(51 downto 0).asBits
      } otherwise {
        // Normal: remove implicit leading 1 from mantissa
        val fracBits = mantissa.raw(mantissa.bitWidth - 2 downto mantissa.bitWidth - 53)
        result := sign ## ieeeExp.asBits ## fracBits
      }
    } otherwise {
      when(isSubnorm) {
        // Subnormal: denormalize the mantissa
        val shiftAmount = U(-126) - exponent.raw.asSInt.asUInt + 1
        val denormMant = mantissa.raw.asUInt >> shiftAmount
        result := B(0, 32 bits) ## sign ## U(0, 8 bits).asBits ## denormMant(22 downto 0).asBits
      } otherwise {
        // Normal: remove implicit leading 1 from mantissa
        val fracBits = mantissa.raw(mantissa.bitWidth - 2 downto mantissa.bitWidth - 24)
        result := B(0, 32 bits) ## sign ## ieeeExp.asBits ## fracBits
      }
    }

    result
  }
}

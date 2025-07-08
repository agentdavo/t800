package transputer.plugins.fpu

import spinal.core._
import spinal.lib._

/** FPU Comparison Unit using AFix
  *
  * Implements IEEE 754 compliant comparison operations:
  *   - FPEQ: Equal comparison
  *   - FPGT: Greater than comparison
  *   - FPLT: Less than comparison
  *   - FPORDERED: Check if both operands are ordered (not NaN)
  *   - FPUNORDERED: Check if either operand is unordered (NaN)
  *
  * Features:
  *   - Full NaN handling (quiet and signaling)
  *   - Signed zero comparison (-0.0 == +0.0)
  *   - AFix-based comparison for precision
  *   - Single cycle operation
  */
class FpuComparison extends Component {
  val io = new Bundle {
    val cmd = slave Stream (ComparisonCmd())
    val rsp = master Flow (ComparisonRsp())
  }

  // Parse operands using AFix representation
  val opA = FpNumber()
  val opB = FpNumber()

  opA.fromBits(io.cmd.payload.a, io.cmd.payload.isDouble)
  opB.fromBits(io.cmd.payload.b, io.cmd.payload.isDouble)

  // Comparison logic
  val compareLogic = new Area {
    // Special case flags
    val bothZero = opA.isZero && opB.isZero // -0 == +0
    val eitherNaN = opA.isNaN || opB.isNaN
    val bothInf = opA.isInf && opB.isInf

    // Sign comparison
    val sameSign = opA.sign === opB.sign
    val aIsNeg = opA.sign
    val bIsNeg = opB.sign

    // Magnitude comparison using AFix
    // For negative numbers, smaller magnitude means greater value
    val expDiff = opA.exponent - opB.exponent
    val mantDiff = opA.mantissa - opB.mantissa

    // Raw comparison results
    val aGtbMagnitude = (expDiff > AFix(0)) ||
      (expDiff === AFix(0) && mantDiff > AFix(0))
    val aLtbMagnitude = (expDiff < AFix(0)) ||
      (expDiff === AFix(0) && mantDiff < AFix(0))
    val aEqbMagnitude = (expDiff === AFix(0)) && (mantDiff === AFix(0))

    // Apply sign logic
    val isEqual = Bool()
    val isGreater = Bool()
    val isLess = Bool()
    val isOrdered = Bool()
    val isUnordered = Bool()

    // Handle special cases first
    when(eitherNaN) {
      // Any comparison with NaN is false (except unordered)
      isEqual := False
      isGreater := False
      isLess := False
      isOrdered := False
      isUnordered := True
    } elsewhen (bothZero) {
      // -0.0 == +0.0
      isEqual := True
      isGreater := False
      isLess := False
      isOrdered := True
      isUnordered := False
    } elsewhen (bothInf) {
      // Inf comparison depends on signs
      isEqual := sameSign
      isGreater := !aIsNeg && bIsNeg
      isLess := aIsNeg && !bIsNeg
      isOrdered := True
      isUnordered := False
    } elsewhen (opA.isInf) {
      // A is Inf, B is finite
      isEqual := False
      isGreater := !aIsNeg // +Inf > any finite
      isLess := aIsNeg // -Inf < any finite
      isOrdered := True
      isUnordered := False
    } elsewhen (opB.isInf) {
      // B is Inf, A is finite
      isEqual := False
      isGreater := bIsNeg // any finite > -Inf
      isLess := !bIsNeg // any finite < +Inf
      isOrdered := True
      isUnordered := False
    } otherwise {
      // Normal comparison
      isOrdered := True
      isUnordered := False

      when(sameSign) {
        // Same sign - compare magnitudes
        isEqual := aEqbMagnitude
        when(aIsNeg) {
          // Both negative - reverse magnitude comparison
          isGreater := aLtbMagnitude
          isLess := aGtbMagnitude
        } otherwise {
          // Both positive - normal magnitude comparison
          isGreater := aGtbMagnitude
          isLess := aLtbMagnitude
        }
      } otherwise {
        // Different signs
        isEqual := False
        isGreater := !aIsNeg && bIsNeg // + > -
        isLess := aIsNeg && !bIsNeg // - < +
      }
    }
  }

  // Generate result based on operation
  val result = Bool()
  val invalidOp = Bool()

  switch(io.cmd.payload.op) {
    is(ComparisonOp.EQ) {
      result := compareLogic.isEqual
      // EQ with NaN doesn't signal invalid
      invalidOp := False
    }

    is(ComparisonOp.GT) {
      result := compareLogic.isGreater
      // GT with NaN signals invalid
      invalidOp := compareLogic.isUnordered
    }

    is(ComparisonOp.LT) {
      result := compareLogic.isLess
      // LT with NaN signals invalid
      invalidOp := compareLogic.isUnordered
    }

    is(ComparisonOp.ORDERED) {
      result := compareLogic.isOrdered
      invalidOp := False
    }

    is(ComparisonOp.UNORDERED) {
      result := compareLogic.isUnordered
      invalidOp := False
    }

    default {
      result := False
      invalidOp := False
    }
  }

  // Handle signaling NaN
  val signalingNaN = (opA.isNaN && !io.cmd.payload.a(51)) ||
    (opB.isNaN && !io.cmd.payload.b(51))

  // Always signal invalid for signaling NaN
  val finalInvalid = invalidOp || signalingNaN

  // Single cycle response
  io.cmd.ready := True
  io.rsp.valid := RegNext(io.cmd.valid)
  io.rsp.payload.result := RegNext(result)
  io.rsp.payload.invalid := RegNext(finalInvalid)

  // For debugging/verification
  val debug = if (GenerationFlags.formal) new Area {
    // Assertions for IEEE 754 compliance
    when(opA.isNaN || opB.isNaN) {
      when(io.cmd.payload.op === ComparisonOp.EQ) {
        assert(io.rsp.payload.result === False, "NaN != NaN")
      }
      when(io.cmd.payload.op === ComparisonOp.UNORDERED) {
        assert(io.rsp.payload.result === True, "NaN is unordered")
      }
    }

    when(opA.isZero && opB.isZero) {
      when(io.cmd.payload.op === ComparisonOp.EQ) {
        assert(io.rsp.payload.result === True, "-0 == +0")
      }
    }
  }
  else null
}

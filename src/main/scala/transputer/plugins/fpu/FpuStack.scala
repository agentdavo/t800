package transputer.plugins.fpu

import spinal.core._
import spinal.lib._

/** FPU Stack Operations Unit
  *
  * Implements T9000 FPU stack manipulation operations:
  *   - FPDUP: Duplicate top of stack (FA -> FB, FA -> FA)
  *   - FPREV: Reverse top two items (FA <-> FB)
  *   - FPPOP: Pop from stack (FB -> FA, FC -> FB)
  *   - FPLDZERODB/SN: Load zero constants
  *   - FPABS: Absolute value
  *   - FPNEG: Negation
  *   - FPLDEXP: Load exponent
  *   - FPNORM: Normalize
  *
  * Features:
  *   - Three-register stack manipulation
  *   - Constant generation
  *   - Sign and exponent manipulation
  *   - Single-cycle operations
  */
class FpuStack extends Component {
  val io = new Bundle {
    // Command interface
    val cmd = slave Stream (StackCmd())
    val rsp = master Flow (StackRsp())

    // Register file interface
    val regfile = new Bundle {
      val fpA = in Bits (64 bits)
      val fpB = in Bits (64 bits)
      val fpC = in Bits (64 bits)

      val writeA = out Bool ()
      val writeB = out Bool ()
      val writeC = out Bool ()
      val dataA = out Bits (64 bits)
      val dataB = out Bits (64 bits)
      val dataC = out Bits (64 bits)
    }
  }

  // Default outputs
  io.regfile.writeA := False
  io.regfile.writeB := False
  io.regfile.writeC := False
  io.regfile.dataA := io.regfile.fpA
  io.regfile.dataB := io.regfile.fpB
  io.regfile.dataC := io.regfile.fpC

  io.rsp.valid := False
  io.rsp.payload.assignDontCare()

  // Single cycle ready
  io.cmd.ready := True

  when(io.cmd.valid) {
    val fpA = io.regfile.fpA
    val fpB = io.regfile.fpB
    val fpC = io.regfile.fpC

    switch(io.cmd.payload.op) {
      // Stack manipulation
      is(FpOp.FPDUP) {
        // Duplicate: FA -> FB, FA stays
        io.regfile.writeB := True
        io.regfile.writeC := True
        io.regfile.dataB := fpA
        io.regfile.dataC := fpB

        io.rsp.valid := True
        io.rsp.payload.stackModified := True
      }

      is(FpOp.FPREV) {
        // Reverse: FA <-> FB
        io.regfile.writeA := True
        io.regfile.writeB := True
        io.regfile.dataA := fpB
        io.regfile.dataB := fpA

        io.rsp.valid := True
        io.rsp.payload.stackModified := True
      }

      is(FpOp.FPPOP) {
        // Pop: FB -> FA, FC -> FB, 0 -> FC
        io.regfile.writeA := True
        io.regfile.writeB := True
        io.regfile.writeC := True
        io.regfile.dataA := fpB
        io.regfile.dataB := fpC
        io.regfile.dataC := B(0, 64 bits)

        io.rsp.valid := True
        io.rsp.payload.stackModified := True
      }

      // Constants
      is(FpOp.FPLDZERODB) {
        // Load +0.0 double
        io.regfile.writeA := True
        io.regfile.dataA := B(0, 64 bits)

        io.rsp.valid := True
        io.rsp.payload.result := B(0, 64 bits)
      }

      is(FpOp.FPLDZEROSN) {
        // Load +0.0 single (stored as double)
        io.regfile.writeA := True
        io.regfile.dataA := B(0, 64 bits)

        io.rsp.valid := True
        io.rsp.payload.result := B(0, 64 bits)
      }

      // Sign manipulation
      is(FpOp.FPABS) {
        // Absolute value - clear sign bit
        val result = fpA(62 downto 0) ## B"0"
        io.regfile.writeA := True
        io.regfile.dataA := B"0" ## result(62 downto 0)

        io.rsp.valid := True
        io.rsp.payload.result := B"0" ## result(62 downto 0)
      }

      is(FpOp.FPNEG) {
        // Negation - flip sign bit
        val sign = fpA(63)
        val result = (!sign) ## fpA(62 downto 0)
        io.regfile.writeA := True
        io.regfile.dataA := result

        io.rsp.valid := True
        io.rsp.payload.result := result
      }

      // Exponent manipulation
      is(FpOp.FPLDEXP) {
        // Load exponent from B into A's exponent field
        val expValue = fpB(10 downto 0).asUInt // Integer from B
        val mantissa = fpA(51 downto 0)
        val sign = fpA(63)

        // Check for special cases
        val aExp = fpA(62 downto 52).asUInt
        val isSpecial = aExp === 0x7ff || aExp === 0

        val result = Bits(64 bits)
        when(isSpecial) {
          // Don't modify special values
          result := fpA
        } otherwise {
          // Replace exponent
          val newExp = expValue.min(0x7fe).asBits // Clamp to valid range
          result := sign ## newExp ## mantissa
        }

        io.regfile.writeA := True
        io.regfile.dataA := result

        io.rsp.valid := True
        io.rsp.payload.result := result
      }

      // Normalization
      is(FpOp.FPNORM) {
        // Normalize denormal numbers
        val sign = fpA(63)
        val exponent = fpA(62 downto 52).asUInt
        val mantissa = fpA(51 downto 0)

        val result = Bits(64 bits)

        when(exponent === 0 && mantissa =/= 0) {
          // Denormal - normalize it
          val leadingZeros = LeadingZeros(mantissa)
          val shift = leadingZeros + 1
          val normalizedMant = (mantissa << shift)(51 downto 0)
          val normalizedExp = (U(1) - shift).resize(11)

          when(normalizedExp <= 0) {
            // Still denormal after normalization
            result := fpA
          } otherwise {
            result := sign ## normalizedExp.asBits ## normalizedMant
          }
        } otherwise {
          // Already normal or special
          result := fpA
        }

        io.regfile.writeA := True
        io.regfile.dataA := result

        io.rsp.valid := True
        io.rsp.payload.result := result
      }
    }
  }

  // Pipeline response to match other units
  val responseReg = RegNext(io.rsp.payload)
  val responseValid = RegNext(io.rsp.valid)

  io.rsp.payload := responseReg
  io.rsp.valid := responseValid

  // Helper function
  def LeadingZeros(bits: Bits): UInt = {
    val width = bits.getWidth
    val zeros = UInt(log2Up(width + 1) bits)
    zeros := width

    for (i <- 0 until width) {
      when(bits(width - 1 - i)) {
        zeros := U(i)
      }
    }
    zeros
  }
}

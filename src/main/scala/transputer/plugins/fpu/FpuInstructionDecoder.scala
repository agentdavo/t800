package transputer.plugins.fpu

import spinal.core._
import spinal.lib._

/** FPU Instruction Decoder for T9000
  *
  * Decodes all 48 T9000 FPU instructions from Tables 6.32-6.37 and dispatches them to appropriate
  * execution units.
  */
class FpuInstructionDecoder extends Component {
  val io = new Bundle {
    // Instruction input
    val opcode = in Bits (16 bits) // Full opcode including prefix
    val valid = in Bool ()

    // Decoded instruction
    val decoded = out(FpuInstruction())
    val decodedValid = out Bool ()

    // Execution unit dispatch
    val dispatch = new Bundle {
      val unit = out(ExecutionUnit())
      val cycles = out UInt (5 bits)
      val precision = out Bool () // False=single, True=double
    }
  }

  // Decode tables based on T9000 manual
  val primaryOp = io.opcode(15 downto 12)
  val secondaryOp = io.opcode(11 downto 8)
  val tertiaryOp = io.opcode(7 downto 0)

  // Default outputs
  io.decoded.assignDontCare()
  io.decodedValid := False
  io.dispatch.unit := ExecutionUnit.NONE
  io.dispatch.cycles := U(0, 5 bits)
  io.dispatch.precision := False

  // Main decode logic
  when(io.valid) {
    // Check for FPU prefix (0x2A or 0x2B)
    when(primaryOp === B"0010" && secondaryOp === B"1010") { // 0x2A
      io.decodedValid := True

      switch(tertiaryOp) {
        // Table 6.32: Load/Store Instructions
        is(B"11110000") { // 0x2AF0 - fpldbs
          io.decoded.op := FpOp.FPLDBS
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := False
        }

        is(B"11110001") { // 0x2AF1 - fpldbd
          io.decoded.op := FpOp.FPLDBD
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := True
        }

        is(B"11110010") { // 0x2AF2 - fpldnls
          io.decoded.op := FpOp.FPLDNLS
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := False
        }

        is(B"11110011") { // 0x2AF3 - fpldnld
          io.decoded.op := FpOp.FPLDNLD
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := True
        }

        is(B"11110100") { // 0x2AF4 - fpstsnl
          io.decoded.op := FpOp.FPSTSNL
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := False
        }

        is(B"11110101") { // 0x2AF5 - fpstdnl
          io.decoded.op := FpOp.FPSTDNL
          io.decoded.category := FpuCategory.LOAD_STORE
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 3
          io.dispatch.precision := True
        }

        // Table 6.33: Arithmetic Instructions
        is(B"11110110") { // 0x2AF6 - fpadd
          io.decoded.op := FpOp.FPADD
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"11110111") { // 0x2AF7 - fpsub
          io.decoded.op := FpOp.FPSUB
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"11111000") { // 0x2AF8 - fpmul
          io.decoded.op := FpOp.FPMUL
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.MULTIPLIER
          io.dispatch.cycles := Mux(io.dispatch.precision, U(3, 5 bits), U(2, 5 bits))
        }

        is(B"11111001") { // 0x2AF9 - fpdiv
          io.decoded.op := FpOp.FPDIV
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.DIVIDER
          io.dispatch.cycles := Mux(io.dispatch.precision, U(15, 5 bits), U(7, 5 bits))
        }

        is(B"11111010") { // 0x2AFA - fpremfirst
          io.decoded.op := FpOp.FPREMFIRST
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.DIVIDER
          io.dispatch.cycles := 13
        }

        is(B"11111011") { // 0x2AFB - fpremstep
          io.decoded.op := FpOp.FPREMSTEP
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.DIVIDER
          io.dispatch.cycles := 13
        }

        is(B"11111100") { // 0x2AFC - fpsqrt
          io.decoded.op := FpOp.FPSQRT
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.DIVIDER
          io.dispatch.cycles := Mux(io.dispatch.precision, U(15, 5 bits), U(7, 5 bits))
        }

        // Table 6.34: Comparison Instructions
        is(B"11111101") { // 0x2AFD - fpeq
          io.decoded.op := FpOp.FPEQ
          io.decoded.category := FpuCategory.COMPARISON
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"11111110") { // 0x2AFE - fpgt
          io.decoded.op := FpOp.FPGT
          io.decoded.category := FpuCategory.COMPARISON
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"11111111") { // 0x2AFF - fplt
          io.decoded.op := FpOp.FPLT
          io.decoded.category := FpuCategory.COMPARISON
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }
      }
    }

    // 0x2B prefix instructions
    when(primaryOp === B"0010" && secondaryOp === B"1011") { // 0x2B
      io.decodedValid := True

      switch(tertiaryOp) {
        // Table 6.34 continued: Comparison
        is(B"00000000") { // 0x2B00 - fpordered
          io.decoded.op := FpOp.FPORDERED
          io.decoded.category := FpuCategory.COMPARISON
          io.dispatch.unit := ExecutionUnit.VCU
          io.dispatch.cycles := 2
        }

        is(B"00000001") { // 0x2B01 - fpunordered
          io.decoded.op := FpOp.FPUNORDERED
          io.decoded.category := FpuCategory.COMPARISON
          io.dispatch.unit := ExecutionUnit.VCU
          io.dispatch.cycles := 2
        }

        // Table 6.35: Conversion Instructions
        is(B"00000010") { // 0x2B02 - fpi32tor32
          io.decoded.op := FpOp.FPI32TOR32
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
          io.dispatch.precision := False
        }

        is(B"00000011") { // 0x2B03 - fpi32tor64
          io.decoded.op := FpOp.FPI32TOR64
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
          io.dispatch.precision := True
        }

        is(B"00000100") { // 0x2B04 - fpr32toi32
          io.decoded.op := FpOp.FPR32TOI32
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
          io.dispatch.precision := False
        }

        is(B"00000101") { // 0x2B05 - fpr64toi32
          io.decoded.op := FpOp.FPR64TOI32
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
          io.dispatch.precision := True
        }

        is(B"00000110") { // 0x2B06 - fpr32tor64
          io.decoded.op := FpOp.FPR32TOR64
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00000111") { // 0x2B07 - fpr64tor32
          io.decoded.op := FpOp.FPR64TOR32
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        // Table 6.36: Rounding Mode Instructions
        is(B"00001000") { // 0x2B08 - fproundn
          io.decoded.op := FpOp.FPROUNDN
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00001001") { // 0x2B09 - fproundp
          io.decoded.op := FpOp.FPROUNDP
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00001010") { // 0x2B0A - fproundm
          io.decoded.op := FpOp.FPROUNDM
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00001011") { // 0x2B0B - fproundz
          io.decoded.op := FpOp.FPROUNDZ
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        // Table 6.37: Status and Control Instructions
        is(B"00001100") { // 0x2B0C - fpuchk
          io.decoded.op := FpOp.FPUCHK
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 2
        }

        is(B"00001101") { // 0x2B0D - fpuclrerr
          io.decoded.op := FpOp.FPUCLRERR
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00001110") { // 0x2B0E - fpuseterr
          io.decoded.op := FpOp.FPUSETERR
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00001111") { // 0x2B0F - fpustatus
          io.decoded.op := FpOp.FPUSTATUS
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 2
        }

        is(B"00010000") { // 0x2B10 - fpustatusr
          io.decoded.op := FpOp.FPUSTATUSR
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.MEMORY
          io.dispatch.cycles := 2
        }

        is(B"00010001") { // 0x2B11 - fpsttest
          io.decoded.op := FpOp.FPSTTEST
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 2
        }

        // Miscellaneous Instructions
        is(B"00010010") { // 0x2B12 - fpabs
          io.decoded.op := FpOp.FPABS
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00010011") { // 0x2B13 - fpneg
          io.decoded.op := FpOp.FPNEG
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00010100") { // 0x2B14 - fpnop
          io.decoded.op := FpOp.FPNOP
          io.decoded.category := FpuCategory.CONTROL
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        is(B"00010101") { // 0x2B15 - fpdup
          io.decoded.op := FpOp.FPDUP
          io.decoded.category := FpuCategory.STACK
          io.dispatch.unit := ExecutionUnit.CONTROL
          io.dispatch.cycles := 1
        }

        // Extended precision instructions
        is(B"00010110") { // 0x2B16 - fpldzerodb
          io.decoded.op := FpOp.FPLDZERODB
          io.decoded.category := FpuCategory.CONSTANT
          io.dispatch.unit := ExecutionUnit.VCU
          io.dispatch.cycles := 1
          io.dispatch.precision := True
        }

        is(B"00010111") { // 0x2B17 - fpldzerosn
          io.decoded.op := FpOp.FPLDZEROSN
          io.decoded.category := FpuCategory.CONSTANT
          io.dispatch.unit := ExecutionUnit.VCU
          io.dispatch.cycles := 1
          io.dispatch.precision := False
        }

        is(B"00011000") { // 0x2B18 - fpint
          io.decoded.op := FpOp.FPINT
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00011001") { // 0x2B19 - fpnint
          io.decoded.op := FpOp.FPNINT
          io.decoded.category := FpuCategory.CONVERSION
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00011010") { // 0x2B1A - fpldexp
          io.decoded.op := FpOp.FPLDEXP
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 2
        }

        is(B"00011011") { // 0x2B1B - fpnorm
          io.decoded.op := FpOp.FPNORM
          io.decoded.category := FpuCategory.ARITHMETIC
          io.dispatch.unit := ExecutionUnit.ADDER
          io.dispatch.cycles := 3
        }
      }
    }
  }

  // Set precision based on operation
  io.dispatch.precision := io.decoded.op.mux(
    FpOp.FPLDBD -> True,
    FpOp.FPLDNLD -> True,
    FpOp.FPSTDNL -> True,
    FpOp.FPI32TOR64 -> True,
    FpOp.FPR64TOI32 -> True,
    FpOp.FPLDZERODB -> True,
    default -> False
  )
}

/** FPU Instruction representation */
case class FpuInstruction() extends Bundle {
  val op = FpOp()
  val category = FpuCategory()
}

/** FPU Instruction categories */
object FpuCategory extends SpinalEnum {
  val LOAD_STORE, ARITHMETIC, COMPARISON, CONVERSION = newElement()
  val CONTROL, STACK, CONSTANT = newElement()
}

/** Execution units */
object ExecutionUnit extends SpinalEnum {
  val NONE, ADDER, MULTIPLIER, DIVIDER, VCU, MEMORY, CONTROL = newElement()
}

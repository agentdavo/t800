package t800.plugins.fpu

import spinal.core._
import spinal.lib._

case class FpCmd() extends Bundle {
  val op = FpOp()
  val a = Bits(64 bits) // Operand A (FA)
  val b = Bits(64 bits) // Operand B (FB)
}

object FpOp extends SpinalEnum(binarySequential) {
  
  // Table 11.24: Load/Store Operations
  object LoadStore {
    val FPLDNLSN, FPLDNLDB, FPLDNLSNI, FPLDNLDBI, FPLDZEROSN, FPLDZERODB,
        FPLDNLADDSN, FPLDNLADDDB, FPLDNLMULSN, FPLDNLMULDB, FPSTNLSN, FPSTNLDB,
        FPSTNLI32 = newElement()
  }

  // Table 11.25: General Operations
  object General {
    val FPENTRY, FPREV, FPDUP = newElement()
  }

  // Table 11.26: Rounding Operations
  object Rounding {
    val FPRN, FPRZ, FPRP, FPRM = newElement()
  }

  // Table 11.27: Error Operations
  object Error {
    val FPCHKERR, FPTESTERR, FPSETERR, FPCLRERR = newElement()
  }

  // Table 11.28: Comparison Operations
  object Comparison {
    val FPGT, FPEQ, FPORDERED, FPNAN, FPNOTFINITE, FPCHKI32, FPCHKI64 = newElement()
  }

  // Table 11.29: Conversion Operations
  object Conversion {
    val FPR32TOR64, FPR64TOR32, FPRTOI32, FPI32TOR32, FPI32TOR64, FPB32TOR64,
        FPNOROUND, FPINT = newElement()
  }

  // Table 11.30: Arithmetic Operations
  object Arithmetic {
    val FPADD, FPSUB, FPMUL, FPDIV, FPABS, FPEXPINC32, FPEXPDEC32, FPMULBY2,
        FPDIVBY2 = newElement()
  }

  // Table 11.31: T805 Compatibility Operations
  object T805Compatibility {
    val FPUSQRTFIRST, FPUSQRTSTEP, FPUSQRTLAST, FPREMFIRST, FPREMSTEP = newElement()
  }

  // Table 11.32: Additional Operations
  object Additional {
    val FPREM, FPSQRT, FPRANGE, FPGE, FPLG = newElement()
  }

  val NONE = newElement()

  def fromOpcode(opcode: Bits): FpOp.C = {
    val op = FpOp()
    op := MuxCase(FpOp.NONE, Seq(
      // Load/Store
      (opcode === B"8E") -> LoadStore.FPLDNLSN,
      (opcode === B"8A") -> LoadStore.FPLDNLDB,
      (opcode === B"86") -> LoadStore.FPLDNLSNI,
      (opcode === B"82") -> LoadStore.FPLDNLDBI,
      (opcode === B"9F") -> LoadStore.FPLDZEROSN,
      (opcode === B"A0") -> LoadStore.FPLDZERODB,
      (opcode === B"AA") -> LoadStore.FPLDNLADDSN,
      (opcode === B"A6") -> LoadStore.FPLDNLADDDB,
      (opcode === B"AC") -> LoadStore.FPLDNLMULSN,
      (opcode === B"A8") -> LoadStore.FPLDNLMULDB,
      (opcode === B"88") -> LoadStore.FPSTNLSN,
      (opcode === B"84") -> LoadStore.FPSTNLDB,
      (opcode === B"9E") -> LoadStore.FPSTNLI32,
      // General
      (opcode === B"AB") -> General.FPENTRY,
      (opcode === B"A4") -> General.FPREV,
      (opcode === B"A3") -> General.FPDUP,
      // Rounding
      (opcode === B"D0") -> Rounding.FPRN,
      (opcode === B"06") -> Rounding.FPRZ,
      (opcode === B"04") -> Rounding.FPRP,
      (opcode === B"05") -> Rounding.FPRM,
      // Error
      (opcode === B"83") -> Error.FPCHKERR,
      (opcode === B"9C") -> Error.FPTESTERR,
      (opcode === B"CB") -> Error.FPSETERR,
      (opcode === B"0C") -> Error.FPCLRERR,
      // Comparison
      (opcode === B"94") -> Comparison.FPGT,
      (opcode === B"95") -> Comparison.FPEQ,
      (opcode === B"92") -> Comparison.FPORDERED,
      (opcode === B"91") -> Comparison.FPNAN,
      (opcode === B"93") -> Comparison.FPNOTFINITE,
      (opcode === B"0E") -> Comparison.FPCHKI32,
      (opcode === B"0F") -> Comparison.FPCHKI64,
      // Conversion
      (opcode === B"07") -> Conversion.FPR32TOR64,
      (opcode === B"08") -> Conversion.FPR64TOR32,
      (opcode === B"90" && !isT805Context) -> Conversion.FPRTOI32, // Disambiguate
      (opcode === B"96") -> Conversion.FPI32TOR32,
      (opcode === B"98") -> Conversion.FPI32TOR64,
      (opcode === B"9A") -> Conversion.FPB32TOR64,
      (opcode === B"00") -> Conversion.FPNOROUND,
      (opcode === B"A1") -> Conversion.FPINT,
      // Arithmetic
      (opcode === B"57") -> Arithmetic.FPADD, // Corrected from S7
      (opcode === B"59") -> Arithmetic.FPSUB, // Corrected from S9
      (opcode === B"5B") -> Arithmetic.FPMUL, // Corrected from SB
      (opcode === B"5C") -> Arithmetic.FPDIV, // Corrected from SC
      (opcode === B"DB") -> Arithmetic.FPABS,
      (opcode === B"DA") -> Arithmetic.FPEXPINC32,
      (opcode === B"D9") -> Arithmetic.FPEXPDEC32,
      (opcode === B"D2") -> Arithmetic.FPMULBY2,
      (opcode === B"D1") -> Arithmetic.FPDIVBY2,
      
      // T805 Compatibility
      (opcode === B"01000001") -> T805Compatibility.FPUSQRTFIRST,
      (opcode === B"01000010") -> T805Compatibility.FPUSQRTSTEP,
      (opcode === B"01000011") -> T805Compatibility.FPUSQRTLAST,
      (opcode === B"5F") -> T805Compatibility.FPREMFIRST,
      (opcode === B"90" && isT805Context) -> T805Compatibility.FPREMSTEP, // Disambiguate
      
      // Additional
      (opcode === B"CF") -> Additional.FPREM,
      (opcode === B"D3") -> Additional.FPSQRT,
      (opcode === B"5D") -> Additional.FPRANGE,
      (opcode === B"97") -> Additional.FPGE,
      (opcode === B"9B") -> Additional.FPLG
    ))
    op
  }

  // Placeholder for T805 context disambiguation (e.g., pipeline state or prefix)
  def isT805Context: Bool = False // To be implemented based on pipeline state
}

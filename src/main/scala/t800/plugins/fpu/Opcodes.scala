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
    val FPLDNLSN, FPLDNLDB, FPLDNLSNI, FPLDNLDBI, FPLDZEROSN, FPLDZERODB, FPLDNLADDSN, FPLDNLADDDB,
      FPLDNLMULSN, FPLDNLMULDB, FPSTNLSN, FPSTNLDB, FPSTNLI32 = newElement()
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
    val FPR32TOR64, FPR64TOR32, FPRTOI32, FPI32TOR32, FPI32TOR64, FPB32TOR64, FPNOROUND, FPINT =
      newElement()
  }

  // Table 11.30: Arithmetic Operations
  object Arithmetic {
    val FPADD, FPSUB, FPMUL, FPDIV, FPABS, FPEXPINC32, FPEXPDEC32, FPMULBY2, FPDIVBY2 = newElement()
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

  // Mapping from raw opcodes to FpOp value. The full T800 implementation uses
  // a large MuxCase statement which pulls in additional dependencies. For the
  // minimal build we only require the enumeration definitions, so this helper is
  // left unimplemented.
  def fromOpcode(opcode: Bits): FpOp.C = {
    val op = FpOp()
    op := FpOp.NONE
    op
  }

  // Placeholder for T805 context disambiguation (e.g., pipeline state or prefix)
  def isT805Context: Bool = False // To be implemented based on pipeline state
}

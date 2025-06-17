package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.Opcode

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
  // implemented here using a simple decoder. Unknown opcodes map to [[FpOp.NONE]].
  def fromOpcode(opcode: Bits): FpOp.C = {
    val op = FpOp()
    op := FpOp.NONE
    switch(opcode) {
      // Load / store
      is(Opcode.SecondaryOpcode.FPLDNLSN) { op := FpOp.LoadStore.FPLDNLSN }
      is(Opcode.SecondaryOpcode.FPLDNLDB) { op := FpOp.LoadStore.FPLDNLDB }
      is(Opcode.SecondaryOpcode.FPLDNLSNI) { op := FpOp.LoadStore.FPLDNLSNI }
      is(Opcode.SecondaryOpcode.FPLDNLDBI) { op := FpOp.LoadStore.FPLDNLDBI }
      is(Opcode.SecondaryOpcode.FPLDZEROSN) { op := FpOp.LoadStore.FPLDZEROSN }
      is(Opcode.SecondaryOpcode.FPLDZERODB) { op := FpOp.LoadStore.FPLDZERODB }
      is(Opcode.SecondaryOpcode.FPLDNLADDSN) { op := FpOp.LoadStore.FPLDNLADDSN }
      is(Opcode.SecondaryOpcode.FPLDNLADDDB) { op := FpOp.LoadStore.FPLDNLADDDB }
      is(Opcode.SecondaryOpcode.FPLDNLMULSN) { op := FpOp.LoadStore.FPLDNLMULSN }
      is(Opcode.SecondaryOpcode.FPLDNLMULDB) { op := FpOp.LoadStore.FPLDNLMULDB }
      is(Opcode.SecondaryOpcode.FPSTNLSN) { op := FpOp.LoadStore.FPSTNLSN }
      is(Opcode.SecondaryOpcode.FPSTNLDB) { op := FpOp.LoadStore.FPSTNLDB }
      is(Opcode.SecondaryOpcode.FPSTNLI32) { op := FpOp.LoadStore.FPSTNLI32 }

      // General
      is(Opcode.SecondaryOpcode.FPREV) { op := FpOp.General.FPREV }
      is(Opcode.SecondaryOpcode.FPDUP) { op := FpOp.General.FPDUP }

      // Rounding
      is(Opcode.SecondaryOpcode.FPRN) { op := FpOp.Rounding.FPRN }
      is(Opcode.SecondaryOpcode.FPRZ) { op := FpOp.Rounding.FPRZ }
      is(Opcode.SecondaryOpcode.FPRP) { op := FpOp.Rounding.FPRP }
      is(Opcode.SecondaryOpcode.FPRM) { op := FpOp.Rounding.FPRM }

      // Comparison
      is(Opcode.SecondaryOpcode.FPGT) { op := FpOp.Comparison.FPGT }
      is(Opcode.SecondaryOpcode.FPEQ) { op := FpOp.Comparison.FPEQ }
      is(Opcode.SecondaryOpcode.FPORDERED) { op := FpOp.Comparison.FPORDERED }
      is(Opcode.SecondaryOpcode.FPNAN) { op := FpOp.Comparison.FPNAN }
      is(Opcode.SecondaryOpcode.FPNOTFINITE) { op := FpOp.Comparison.FPNOTFINITE }

      // Conversion
      is(Opcode.SecondaryOpcode.FPR32TOR64) { op := FpOp.Conversion.FPR32TOR64 }
      is(Opcode.SecondaryOpcode.FPR64TOR32) { op := FpOp.Conversion.FPR64TOR32 }
      is(Opcode.SecondaryOpcode.FPRTOI32) { op := FpOp.Conversion.FPRTOI32 }
      is(Opcode.SecondaryOpcode.FPI32TOR32) { op := FpOp.Conversion.FPI32TOR32 }
      is(Opcode.SecondaryOpcode.FPI32TOR64) { op := FpOp.Conversion.FPI32TOR64 }
      is(Opcode.SecondaryOpcode.FPB32TOR64) { op := FpOp.Conversion.FPB32TOR64 }
      is(Opcode.SecondaryOpcode.FPINT) { op := FpOp.Conversion.FPINT }

      // Arithmetic
      is(Opcode.SecondaryOpcode.FPADD) { op := FpOp.Arithmetic.FPADD }
      is(Opcode.SecondaryOpcode.FPSUB) { op := FpOp.Arithmetic.FPSUB }
      is(Opcode.SecondaryOpcode.FPMUL) { op := FpOp.Arithmetic.FPMUL }
      is(Opcode.SecondaryOpcode.FPDIV) { op := FpOp.Arithmetic.FPDIV }
      is(Opcode.SecondaryOpcode.FPABS) { op := FpOp.Arithmetic.FPABS }
      is(Opcode.SecondaryOpcode.FPEXPINC32) { op := FpOp.Arithmetic.FPEXPINC32 }
      is(Opcode.SecondaryOpcode.FPEXPDEC32) { op := FpOp.Arithmetic.FPEXPDEC32 }
      is(Opcode.SecondaryOpcode.FPMULBY2) { op := FpOp.Arithmetic.FPMULBY2 }
      is(Opcode.SecondaryOpcode.FPDIVBY2) { op := FpOp.Arithmetic.FPDIVBY2 }

      // Additional
      is(Opcode.SecondaryOpcode.FPREM) { op := FpOp.Additional.FPREM }
      is(Opcode.SecondaryOpcode.FPSQRT) { op := FpOp.Additional.FPSQRT }
      is(Opcode.SecondaryOpcode.FPRANGE) { op := FpOp.Additional.FPRANGE }
      is(Opcode.SecondaryOpcode.FPGE) { op := FpOp.Additional.FPGE }
      is(Opcode.SecondaryOpcode.FPLG) { op := FpOp.Additional.FPLG }
    }
    op
  }

  // Placeholder for T805 context disambiguation (e.g., pipeline state or prefix)
  def isT805Context: Bool = False // To be implemented based on pipeline state
}

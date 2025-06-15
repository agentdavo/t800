package t800.plugins.fpu

import spinal.core._
import spinal.lib._

case class FpCmd() extends Bundle {
  val op = FpOp()
  val a = Bits(64 bits) // Operand A (FA)
  val b = Bits(64 bits) // Operand B (FB)
}

object FpOp extends SpinalEnum(binarySequential) {
  val NONE, FPLDNLSN, FPLDNLDB, FPLDNLSNI, FPLDNLDBI, FPLDZEROSN, FPLDZERODB,
      FPLDNLADDSN, FPLDNLADDDB, FPLDNLMULSN, FPLDNLMULDB, FPSTNLSN, FPSTNLDB, FPSTNLI32,
      FPENTRY, FPREV, FPDUP, FPRN, FPRZ, FPRP, FPRM, FPCHKERR, FPTESTERR, FPSETERR, FPCLRERR,
      FPGT, FPEQ, FPORDERED, FPNAN, FPNOTFINITE, FPCHKI32, FPCHKI64,
      FPR32TOR64, FPR64TOR32, FPRTOI32, FPI32TOR32, FPI32TOR64, FPB32TOR64, FPNOROUND, FPINT,
      FPADD, FPSUB, FPMUL, FPDIV, FPABS, FPEXPINC32, FPEXPDEC32, FPMULBY2, FPDIVBY2,
      FPUSQRTFIRST, FPUSQRTSTEP, FPUSQRTLAST, FPREMFIRST, FPREMSTEP, FPREM, FPSQRT, FPRANGE,
      FPGE, FPLG = newElement()

  def fromOpcode(opcode: Bits): FpOp.C = {
    val op = FpOp()
    op := MuxCase(FpOp.NONE, Seq(
      (opcode === B"10001110") -> FpOp.FPLDNLSN,
      (opcode === B"10001010") -> FpOp.FPLDNLDB,
      (opcode === B"10000110") -> FpOp.FPLDNLSNI,
      (opcode === B"10000010") -> FpOp.FPLDNLDBI,
      (opcode === B"10011111") -> FpOp.FPLDZEROSN,
      (opcode === B"10100000") -> FpOp.FPLDZERODB,
      (opcode === B"10101010") -> FpOp.FPLDNLADDSN,
      (opcode === B"10100110") -> FpOp.FPLDNLADDDB,
      (opcode === B"10101100") -> FpOp.FPLDNLMULSN,
      (opcode === B"10101000") -> FpOp.FPLDNLMULDB,
      (opcode === B"10001000") -> FpOp.FPSTNLSN,
      (opcode === B"10000100") -> FpOp.FPSTNLDB,
      (opcode === B"10011110") -> FpOp.FPSTNLI32,
      (opcode === Opcodes.Secondary.FPENTRY) -> FpOp.FPENTRY,
      (opcode === Opcodes.Secondary.FPREV) -> FpOp.FPREV,
      (opcode === Opcodes.Secondary.FPDUP) -> FpOp.FPDUP,
      (opcode === Opcodes.Secondary.FPRN) -> FpOp.FPRN,
      (opcode === Opcodes.Secondary.FPRZ) -> FpOp.FPRZ,
      (opcode === Opcodes.Secondary.FPRP) -> FpOp.FPRP,
      (opcode === Opcodes.Secondary.FPRM) -> FpOp.FPRM,
      (opcode === Opcodes.Secondary.FPCHKERR) -> FpOp.FPCHKERR,
      (opcode === Opcodes.Secondary.FPTESTERR) -> FpOp.FPTESTERR,
      (opcode === Opcodes.Secondary.FPSETERR) -> FpOp.FPSETERR,
      (opcode === Opcodes.Secondary.FPCLRERR) -> FpOp.FPCLRERR,
      (opcode === Opcodes.Secondary.FPGT) -> FpOp.FPGT,
      (opcode === Opcodes.Secondary.FPEQ) -> FpOp.FPEQ,
      (opcode === Opcodes.Secondary.FPORDERED) -> FpOp.FPORDERED,
      (opcode === Opcodes.Secondary.FPNAN) -> FpOp.FPNAN,
      (opcode === Opcodes.Secondary.FPNOTFINITE) -> FpOp.FPNOTFINITE,
      (opcode === Opcodes.Secondary.FPCHKI32) -> FpOp.FPCHKI32,
      (opcode === Opcodes.Secondary.FPCHKI64) -> FpOp.FPCHKI64,
      (opcode === Opcodes.Secondary.FPR32TOR64) -> FpOp.FPR32TOR64,
      (opcode === Opcodes.Secondary.FPR64TOR32) -> FpOp.FPR64TOR32,
      (opcode === Opcodes.Secondary.FPRTOI32) -> FpOp.FPRTOI32,
      (opcode === Opcodes.Secondary.FPI32TOR32) -> FpOp.FPI32TOR32,
      (opcode === Opcodes.Secondary.FPI32TOR64) -> FpOp.FPI32TOR64,
      (opcode === Opcodes.Secondary.FPB32TOR64) -> FpOp.FPB32TOR64,
      (opcode === Opcodes.Secondary.FPNOROUND) -> FpOp.FPNOROUND,
      (opcode === Opcodes.Secondary.FPINT) -> FpOp.FPINT,
      (opcode === Opcodes.Secondary.FPADD) -> FpOp.FPADD,
      (opcode === Opcodes.Secondary.FPSUB) -> FpOp.FPSUB,
      (opcode === Opcodes.Secondary.FPMUL) -> FpOp.FPMUL,
      (opcode === Opcodes.Secondary.FPDIV) -> FpOp.FPDIV,
      (opcode === Opcodes.Secondary.FPABS) -> FpOp.FPABS,
      (opcode === Opcodes.Secondary.FPEXPINC32) -> FpOp.FPEXPINC32,
      (opcode === Opcodes.Secondary.FPEXPDEC32) -> FpOp.FPEXPDEC32,
      (opcode === Opcodes.Secondary.FPMULBY2) -> FpOp.FPMULBY2,
      (opcode === Opcodes.Secondary.FPDIVBY2) -> FpOp.FPDIVBY2,
      (opcode === B"01000001") -> FpOp.FPUSQRTFIRST,
      (opcode === B"01000010") -> FpOp.FPUSQRTSTEP,
      (opcode === B"01000011") -> FpOp.FPUSQRTLAST,
      (opcode === Opcodes.Secondary.FPREMFIRST) -> FpOp.FPREMFIRST,
      (opcode === Opcodes.Secondary.FPREMSTEP) -> FpOp.FPREMSTEP,
      (opcode === Opcodes.Secondary.FPREM) -> FpOp.FPREM,
      (opcode === Opcodes.Secondary.FPSQRT) -> FpOp.FPSQRT,
      (opcode === Opcodes.Secondary.FPRANGE) -> FpOp.FPRANGE,
      (opcode === Opcodes.Secondary.FPGE) -> FpOp.FPGE,
      (opcode === Opcodes.Secondary.FPLG) -> FpOp.FPLG
    ))
    op
  }
}

package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import t800.Global

trait FpuService {
  def push(operand: Bits): Unit // Push to FA/FB
  def pop(): Bits // Pop from FA
  def execute(opcode: Bits, operands: Vec[Bits]): Bits // Execute FPU operation
  def isBusy: Bool // FPU busy (multi-cycle ops)
  def setRoundingMode(mode: Bits): Unit // Set rounding mode (fprn, fprz, etc.)
  def getErrorFlags: Bits // Get error flags (overflow, invalid, etc.)
  def clearErrorFlags: Unit // Clear error flags
}

trait FpuControlSrv {
  def specialValueDetected: Bool // VCU special value flag
  def specialResult: Bits // VCU result (NaN, infinity, denormal)
  def trapEnable: Bool // IEEE-754 exception trap
  def trapType: UInt // Trap type (e.g., overflow, invalid)
  def roundingMode: Bits // Current rounding mode
}

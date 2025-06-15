package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import t800.Global

trait FpuService {
  def push(operand: Bits): Unit // Push to FA/FB
  def pushAfix(operand: AFix): Unit // Push AFix operand
  def pop(): Bits // Pop from FA
  def popAfix(): AFix // Pop as AFix
  def execute(opcode: Bits, operands: Vec[Bits]): Bits // Execute FPU operation
  def executeAfix(opcode: Bits, operands: Vec[AFix]): AFix // Execute with AFix
  def isBusy: Bool // FPU busy
  def setRoundingMode(mode: Bits): Unit // fprn, fprz, etc.
  def getErrorFlags: Bits // Error flags
  def clearErrorFlags: Unit // Clear error flags
}

trait FpuControlSrv {
  def specialValueDetected: Bool // VCU special value flag
  def specialResult: Bits // VCU result
  def trapEnable: Bool // IEEE-754 trap
  def trapType: UInt // Trap type
  def roundingMode: Bits // Rounding mode
}

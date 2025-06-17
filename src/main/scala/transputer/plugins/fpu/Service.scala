package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import transputer.Global

// Forward simple command and operation definitions from `Opcodes.scala`.
// This avoids duplicate type names when both files are included in the build.
import transputer.plugins.fpu.{FpCmd, FpOp}

trait FpuSrv {
  def pipe: Flow[FpCmd]
  def rsp: Flow[UInt]
  def send(op: FpOp.C, a: UInt, b: UInt): Unit = {
    pipe.valid := True
    pipe.payload.op := op
    pipe.payload.a := a.asBits
    pipe.payload.b := b.asBits
  }
  def resultValid: Bool = rsp.valid
  def result: UInt = rsp.payload
}

trait FpuOpsSrv {
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
  def setRoundingMode(mode: Bits): Unit // Update rounding mode
  def getErrorFlags: Bits // Retrieve current error flags
  def clearErrorFlags: Unit // Clear error flags
  def isFpuBusy(opcode: Bits): Bool // FPU pipeline busy
}

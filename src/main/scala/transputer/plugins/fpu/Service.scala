package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import transputer.Global

// Forward simple command and operation definitions from `Opcodes.scala`.
// This avoids duplicate type names when both files are included in the build.
import transputer.plugins.fpu.{FpCmd, FpOp}

/** Service interface for T9000 FPU with hybrid signal-method architecture.
  *
  * This interface uses a hybrid approach:
  *   - Direct signal access for performance-critical FPU register operations
  *   - Method interfaces for complex FPU operations and pipeline control
  *   - Proper SpinalHDL signal ownership patterns
  */
trait FpuService {
  // ========================================
  // DIRECT SIGNAL ACCESS (for performance-critical operations)
  // ========================================
  // These are mutable signals owned by the FpuPlugin
  // They can be read directly and assigned from other plugins
  def FPA: Bits // Direct access to FPAreg (FP stack top)
  def FPB: Bits // Direct access to FPBreg (FP stack second)
  def FPC: Bits // Direct access to FPCreg (FP stack third)
  def FPStatus: Bits // Direct access to FPstatusReg
  def roundingMode: Bits // Direct access to current rounding mode
  def errorFlags: Bits // Direct access to IEEE 754 error flags
  def isBusy: Bool // Direct access to FPU busy status

  // ========================================
  // METHOD INTERFACES (for complex operations with pipeline control)
  // ========================================
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
  def setRoundingMode(mode: Bits): Unit

  // Signal update triggers (called when direct signal assignment occurs)
  def updateRegisters(): Unit // Sync signals back to register file
}

trait FpuOpsService {
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

trait FpuControlService {
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

package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import transputer.Global

// Forward simple command and operation definitions from `Opcodes.scala`.
// This avoids duplicate type names when both files are included in the build.
import transputer.plugins.fpu.{FpCmd, FpOp}

/** Service interface for T9000 FPU with trap handling and shadow register support.
  *
  * This interface provides comprehensive support for T9000 FPU features:
  *   - IEEE 754 compliant exception handling with trap enables
  *   - Shadow register preservation via THDS (Trap Handler Data Structure)
  *   - Multi-cycle operation management (division, square root)
  *   - Context switching and debugging support
  *   - Performance-critical direct signal access
  *   - Pipeline integration for complex operations
  *
  * Based on T9000 Transputer Instruction Set Manual (t9000ism.pdf) sections 10-11 and T9000
  * Hardware Reference Manual (t9000hrm.pdf) sections 3.5, 8.
  */
trait FpuService {
  // ========================================
  // DIRECT SIGNAL ACCESS (performance-critical operations)
  // ========================================
  // Primary FPU registers (T9000ism.pdf section 11.3, page 141)
  def FPA: Bits // FPAreg - 64-bit FP stack top
  def FPB: Bits // FPBreg - 64-bit FP stack second
  def FPC: Bits // FPCreg - 64-bit FP stack third
  def FPStatus: Bits // FPStatusReg - 32-bit status register

  // Shadow registers for trap handling (stored in THDS)
  // These preserve FPU state during traps, context switches, debugging
  def shadowFPA: Bits // Shadow copy of FPAreg
  def shadowFPB: Bits // Shadow copy of FPBreg
  def shadowFPC: Bits // Shadow copy of FPCreg
  def shadowFPStatus: Bits // Shadow copy of FPStatusReg

  // IEEE 754 exception flags (FPStatus bits 0-4, t9000ism.pdf page 139)
  def invalidFlag: Bool // Invalid operation (bit 0)
  def divZeroFlag: Bool // Divide by zero (bit 1)
  def overflowFlag: Bool // Overflow (bit 2)
  def underflowFlag: Bool // Underflow (bit 3)
  def inexactFlag: Bool // Inexact result (bit 4)

  // Trap enables (FPStatus bits 5-9, t9000ism.pdf page 139)
  def invalidTrapEn: Bool // Invalid trap enable (bit 5)
  def divZeroTrapEn: Bool // Divide by zero trap enable (bit 6)
  def overflowTrapEn: Bool // Overflow trap enable (bit 7)
  def underflowTrapEn: Bool // Underflow trap enable (bit 8)
  def inexactTrapEn: Bool // Inexact trap enable (bit 9)

  // Control and status
  def roundingMode: Bits // Rounding mode (FPStatus bits 30-31)
  def comparisonResult: Bool // Comparison result (FPStatus bit 12)
  def isBusy: Bool // FPU busy with multi-cycle operation
  def isTrapped: Bool // FPU has pending trap

  // ========================================
  // TRAP HANDLING AND SHADOW REGISTERS
  // ========================================
  // Save current FPU state to shadow registers (for THDS)
  def saveToShadow(): Unit

  // Restore FPU state from shadow registers
  def restoreFromShadow(): Unit

  // Check if any exceptions are pending and traps enabled
  def checkExceptions(): Bool

  // Trigger trap if exceptions are pending (fpuchk instruction)
  def triggerTrap(): Unit

  // Clear all exception flags (fpuclrerr instruction)
  def clearExceptions(): Unit

  // Set specific exception flags (fpuseterr instruction)
  def setException(exceptionMask: Bits): Unit

  // ========================================
  // PIPELINE AND OPERATION INTERFACES
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

  // ========================================
  // CONTEXT SWITCHING SUPPORT
  // ========================================
  // Save FPU state to memory (fpustatus instruction, t9000ism.pdf page 314)
  def saveStatusToMemory(address: UInt): Unit

  // Restore FPU state from memory (fpustatusr instruction, t9000ism.pdf page 316)
  def restoreStatusFromMemory(address: UInt): Unit

  // Signal update triggers
  def updateRegisters(): Unit // Sync signals back to register file
}

/** Enhanced FPU operations service with T9000 specifics */
trait FpOpsService {
  // Stack operations (T9000ism.pdf section 11.3, page 141)
  def push(operand: Bits): Unit // Push to FA/FB
  def pushAfix(operand: AFix): Unit // Push AFix operand
  def pop(): Bits // Pop from FA
  def popAfix(): AFix // Pop as AFix
  def dup(): Unit // FPDUP - duplicate top of stack
  def rev(): Unit // FPREV - reverse top two elements

  // Operation execution with exception handling
  def execute(opcode: Bits, operands: Vec[Bits]): Bits // Execute FPU operation
  def executeAfix(opcode: Bits, operands: Vec[AFix]): AFix // Execute with AFix
  def executeWithTrapCheck(opcode: Bits, operands: Vec[Bits]): Bits // Execute with trap checking

  // Status and control
  def isBusy: Bool // FPU busy with multi-cycle operation
  def setRoundingMode(mode: Bits): Unit // Set rounding mode (fprn, fprz, fprp, fprm)
  def getErrorFlags: Bits // Get IEEE 754 exception flags
  def clearErrorFlags: Unit // Clear all exception flags
  def checkForTraps(): Bool // Check if any traps are pending
}

/** Vacuum Cleaner Unit (VCU) service for special value detection */
trait VcuService {
  // Special value detection (t9kfpdsn.pdf page 3)
  def isNaN(value: Bits): Bool // NaN detection
  def isInfinity(value: Bits): Bool // Infinity detection
  def isZero(value: Bits): Bool // Zero detection
  def isDenormal(value: Bits): Bool // Denormalized number detection

  // Special value handling
  def specialValueDetected: Bool // Any special value detected
  def specialValueType: UInt // Type of special value (NaN=0, Inf=1, Zero=2, Denorm=3)
  def specialResult: Bits // Result for special value operations

  // Exception generation
  def generateInvalidException(): Unit // Generate invalid operation exception
  def generateOverflowException(): Unit // Generate overflow exception
  def generateUnderflowException(): Unit // Generate underflow exception
}

/** T9000 trap handling service for FPU integration */
trait FpuTrapService {
  // Trap detection and handling (t9000hrm.pdf section 3.5, page 69)
  def trapPending: Bool // Any FP trap is pending
  def trapType: UInt // Type of pending trap (0=invalid, 1=divzero, 2=overflow, etc.)
  def trapVector: UInt // Trap vector address

  // THDS (Trap Handler Data Structure) integration
  def saveToThds(thdsAddress: UInt): Unit // Save FPU state to THDS
  def restoreFromThds(thdsAddress: UInt): Unit // Restore FPU state from THDS
  def updateThdsSlots(): Unit // Update THDS FP register slots

  // Trap instructions (t9000ism.pdf pages 310-316)
  def fpuchk(): Unit // Check for pending FP exceptions and trap
  def fpuclrerr(): Unit // Clear FP error flags
  def fpuseterr(mask: Bits): Unit // Set FP error flags
  def fpustatus(address: UInt): Unit // Store FP status to memory
  def fpustatusr(address: UInt): Unit // Restore FP status from memory

  // Multi-cycle operation support
  def pauseForTrap(): Unit // Pause current multi-cycle operation for trap
  def resumeAfterTrap(): Unit // Resume multi-cycle operation after trap
}

/** Divider/Rooter specific service for shared logic engine */
trait DividerRooterService {
  // Shared operation control (t9kfpdsn.pdf pages 11-12)
  def isDivisionActive: Bool // Division operation in progress
  def isSqrtActive: Bool // Square root operation in progress
  def isIdle: Bool // Divider/Rooter is idle

  // SRT algorithm state
  def currentIteration: UInt // Current iteration count
  def maxIterations: UInt // Maximum iterations for current precision
  def converged: Bool // Algorithm has converged

  // Shared hardware control
  def selectDivisionMode(): Unit // Configure for division
  def selectSqrtMode(): Unit // Configure for square root
  def performIteration(): Unit // Perform one SRT iteration
  def finalizeResult(): Unit // Finalize and normalize result

  // Area optimization status
  def getSharedMultiplierUsage: Bool // Shared multiplier in use
  def getSharedAdderUsage: Bool // Shared adder in use
}

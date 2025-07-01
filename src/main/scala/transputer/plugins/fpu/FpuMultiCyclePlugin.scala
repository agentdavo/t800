package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global
import transputer.plugins.core.pipeline.PipelineStageService

/** T9000 FPU Multi-cycle Plugin implementing long-latency FP operations.
  *
  * This plugin extends the basic FPU to support multi-cycle operations like:
  *   - FPDIV: Floating-point division (24-54 cycles)
  *   - FPSQRT: Floating-point square root (24-54 cycles)
  *   - FPREM: Floating-point remainder
  *
  * Uses the CtrlLane API to create an FPU execution lane that can stall the pipeline while long
  * operations complete.
  */
class FpuMultiCyclePlugin extends FiberPlugin {
  override def getDisplayName(): String = "FpuMultiCyclePlugin"
  setName("fpuMultiCycle")

  // Multi-cycle operation states
  object FpuState extends SpinalEnum {
    val IDLE, DIVIDING, SQRT, REMAINDER = newElement()
  }

  // FPU lane for multi-cycle operations
  class FpuLane(val laneName: String = "fpu") {
    val upIsCancel = Bool()
    val downIsCancel = Bool()

    // Multi-cycle operation control
    val state = Reg(FpuState()) init FpuState.IDLE
    val cycleCount = Reg(UInt(6 bits)) init 0
    val operandA = Reg(UInt(64 bits))
    val operandB = Reg(UInt(64 bits))
    val result = Reg(UInt(64 bits))
    val resultReady = Reg(Bool()) init False

    // Operation latencies (configurable)
    val DIV_LATENCY = 24
    val SQRT_LATENCY = 24
    val REM_LATENCY = 30

    def startDivision(a: UInt, b: UInt): Unit = {
      state := FpuState.DIVIDING
      operandA := a
      operandB := b
      cycleCount := 0
      resultReady := False
    }

    def startSqrt(a: UInt): Unit = {
      state := FpuState.SQRT
      operandA := a
      cycleCount := 0
      resultReady := False
    }

    def startRemainder(a: UInt, b: UInt): Unit = {
      state := FpuState.REMAINDER
      operandA := a
      operandB := b
      cycleCount := 0
      resultReady := False
    }

    // Control lane ready status
    def isReady: Bool = !isStalling

    // Stall pipeline when operation in progress
    def isStalling: Bool = state =/= FpuState.IDLE && !resultReady

    // Update state machine
    val updateLogic = new Area {
      switch(state) {
        is(FpuState.DIVIDING) {
          cycleCount := cycleCount + 1
          when(cycleCount === DIV_LATENCY) {
            // Simplified division result (would be actual divider output)
            result := operandA / operandB
            resultReady := True
          }
        }

        is(FpuState.SQRT) {
          cycleCount := cycleCount + 1
          when(cycleCount === SQRT_LATENCY) {
            // Simplified sqrt result (would be actual sqrt unit output)
            result := operandA >> 1 // Placeholder
            resultReady := True
          }
        }

        is(FpuState.REMAINDER) {
          cycleCount := cycleCount + 1
          when(cycleCount === REM_LATENCY) {
            // Simplified remainder result
            result := operandA % operandB
            resultReady := True
          }
        }

        default {
          // IDLE state
        }
      }

      // Clear state when result consumed
      when(resultReady && isReady) {
        state := FpuState.IDLE
        resultReady := False
      }
    }
  }

  // Service interface for multi-cycle FPU operations
  trait FpuMultiCycleService {
    def startDivision(a: UInt, b: UInt): Unit
    def startSqrt(a: UInt): Unit
    def startRemainder(a: UInt, b: UInt): Unit
    def isComplete: Bool
    def getResult: UInt
  }

  during setup new Area {
    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] setup start")

    addService(new FpuMultiCycleService {
      override def startDivision(a: UInt, b: UInt): Unit = fpuLane.startDivision(a, b)
      override def startSqrt(a: UInt): Unit = fpuLane.startSqrt(a)
      override def startRemainder(a: UInt, b: UInt): Unit = fpuLane.startRemainder(a, b)
      override def isComplete: Bool = fpuLane.resultReady
      override def getResult: UInt = fpuLane.result
    })

    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] setup end")
  }

  var fpuLane: FpuLane = null

  during build new Area {
    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] build start")

    // Get pipeline stages
    val pipe = host[PipelineStageService]

    // Create FPU lane in Memory stage (stage 4)
    fpuLane = new FpuLane("fpuMulti")
    fpuLane.upIsCancel := False
    fpuLane.downIsCancel := False

    // Stall pipeline when FPU operation in progress
    pipe.memory.haltWhen(fpuLane.isStalling)

    // Simple state tracking without complex lane API
    // TODO: Implement proper CtrlLane integration when available

    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] Multi-cycle FPU configured")
    println(
      s"[${FpuMultiCyclePlugin.this.getDisplayName()}] - Division: ${fpuLane.DIV_LATENCY} cycles"
    )
    println(
      s"[${FpuMultiCyclePlugin.this.getDisplayName()}] - Square root: ${fpuLane.SQRT_LATENCY} cycles"
    )
    println(
      s"[${FpuMultiCyclePlugin.this.getDisplayName()}] - Remainder: ${fpuLane.REM_LATENCY} cycles"
    )
    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] - Pipeline stalls during operations")
    println(s"[${FpuMultiCyclePlugin.this.getDisplayName()}] build end")
  }
}

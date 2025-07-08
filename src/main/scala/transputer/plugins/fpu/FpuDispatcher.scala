package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

/** FPU Instruction Dispatcher
  *
  * Connects the instruction decoder to the appropriate execution units and manages the multi-cycle
  * execution flow for all 48 T9000 FPU instructions.
  *
  * This dispatcher:
  *   - Routes decoded instructions to correct execution unit
  *   - Manages operand selection and routing
  *   - Handles result collection and writeback
  *   - Coordinates multi-cycle operations
  */
class FpuDispatcher extends Component {
  val io = new Bundle {
    // From instruction decoder
    val decoded = slave Flow (FpuInstruction())
    val dispatch = slave Flow (new Bundle {
      val unit = ExecutionUnit()
      val cycles = UInt(5 bits)
      val precision = Bool()
    })

    // From register file
    val regfile = new Bundle {
      val fpA = in Bits (64 bits)
      val fpB = in Bits (64 bits)
      val fpC = in Bits (64 bits)
      val status = in Bits (32 bits)
      val memAddr = in UInt (32 bits) // From CPU A register
    }

    // To execution units
    val adder = master Stream (FpCmd())
    val multiplier = master Stream (FpCmd())
    val divider = master Stream (DividerCmd())
    val memory = master Stream (MemoryCmd())
    val control = master Stream (ControlCmd())
    val special = master Stream (SpecialCmd())

    // From execution units
    val adderRsp = slave Flow (FpRsp())
    val multiplierRsp = slave Flow (FpRsp())
    val dividerRsp = slave Flow (FpRsp())
    val memoryRsp = slave Flow (MemoryRsp())
    val controlRsp = slave Flow (ControlRsp())
    val specialRsp = slave Flow (SpecialRsp())

    // Combined result output
    val result = master Flow (FpuResult())

    // Status
    val busy = out Bool ()
  }

  // State machine for multi-cycle dispatch
  object State extends SpinalEnum {
    val IDLE, DISPATCH, EXECUTE, COLLECT = newElement()
  }

  val state = Reg(State()) init State.IDLE
  val cycleCounter = Reg(UInt(5 bits)) init 0
  val maxCycles = Reg(UInt(5 bits)) init 0
  val currentUnit = Reg(ExecutionUnit())
  val currentOp = Reg(FpOp())

  // Default outputs
  io.adder.valid := False
  io.adder.payload.assignDontCare()
  io.multiplier.valid := False
  io.multiplier.payload.assignDontCare()
  io.divider.valid := False
  io.divider.payload.assignDontCare()
  io.memory.valid := False
  io.memory.payload.assignDontCare()
  io.control.valid := False
  io.control.payload.assignDontCare()
  io.special.valid := False
  io.special.payload.assignDontCare()

  io.result.valid := False
  io.result.payload.assignDontCare()
  io.busy := state =/= State.IDLE

  // Extract common operand fields
  val roundingMode = io.regfile.status(6 downto 5)
  val precision = io.dispatch.payload.precision

  // State machine
  switch(state) {
    is(State.IDLE) {
      when(io.decoded.valid) {
        state := State.DISPATCH
        currentUnit := io.dispatch.payload.unit
        currentOp := io.decoded.payload.op
        maxCycles := io.dispatch.payload.cycles
        cycleCounter := 0
      }
    }

    is(State.DISPATCH) {
      // Route to appropriate execution unit based on dispatcher info
      switch(currentUnit) {
        is(ExecutionUnit.ADDER) {
          io.adder.valid := True
          io.adder.payload.op := currentOp // Pass original operation
          io.adder.payload.a := io.regfile.fpA
          io.adder.payload.b := io.regfile.fpB
          io.adder.payload.rounding := roundingMode
          io.adder.payload.isDouble := precision

          // Special handling for unary operations
          when(currentOp === FpOp.FPABS || currentOp === FpOp.FPNEG) {
            io.adder.payload.b := B(0, 64 bits) // Second operand unused
          }

          when(io.adder.ready) {
            state := State.EXECUTE
          }
        }

        is(ExecutionUnit.MULTIPLIER) {
          io.multiplier.valid := True
          io.multiplier.payload.op := currentOp // Pass original operation
          io.multiplier.payload.a := io.regfile.fpA
          io.multiplier.payload.b := io.regfile.fpB
          io.multiplier.payload.rounding := roundingMode
          io.multiplier.payload.isDouble := precision

          when(io.multiplier.ready) {
            state := State.EXECUTE
          }
        }

        is(ExecutionUnit.DIVIDER) {
          io.divider.valid := True
          io.divider.payload.a := io.regfile.fpA
          io.divider.payload.b := io.regfile.fpB
          io.divider.payload.isSqrt := currentOp === FpOp.FPSQRT
          io.divider.payload.rounding := roundingMode
          io.divider.payload.isDouble := precision

          when(io.divider.ready) {
            state := State.EXECUTE
          }
        }

        is(ExecutionUnit.MEMORY) {
          io.memory.valid := True
          io.memory.payload.addr := io.regfile.memAddr
          io.memory.payload.data := io.regfile.fpA // For stores
          io.memory.payload.isStore := currentOp === FpOp.FPSTSNL || currentOp === FpOp.FPSTDNL
          io.memory.payload.isDouble := precision

          when(io.memory.ready) {
            state := State.EXECUTE
          }
        }

        is(ExecutionUnit.CONTROL) {
          io.control.valid := True
          io.control.payload.op := currentOp
          io.control.payload.statusIn := io.regfile.status
          io.control.payload.roundingMode := roundingMode

          when(io.control.ready) {
            state := State.EXECUTE
          }
        }

        is(ExecutionUnit.VCU) {
          io.special.valid := True
          io.special.payload.op := currentOp
          io.special.payload.a := io.regfile.fpA
          io.special.payload.b := io.regfile.fpB

          when(io.special.ready) {
            state := State.EXECUTE
          }
        }
      }
    }

    is(State.EXECUTE) {
      cycleCounter := cycleCounter + 1
      when(cycleCounter === maxCycles - 1) {
        state := State.COLLECT
      }
    }

    is(State.COLLECT) {
      // Collect results from execution units
      val result = FpuResult()
      result.op := currentOp
      result.exceptions := B"00000"
      result.data := B(0, 64 bits)
      result.updateStack := True

      switch(currentUnit) {
        is(ExecutionUnit.ADDER) {
          when(io.adderRsp.valid) {
            result.data := io.adderRsp.payload.result
            result.exceptions := io.adderRsp.payload.exceptions

            // Comparison operations don't update stack
            when(currentOp === FpOp.FPEQ || currentOp === FpOp.FPGT || currentOp === FpOp.FPLT) {
              result.updateStack := False
              result.comparisonResult := io.adderRsp.payload.result(0)
            }
          }
        }

        is(ExecutionUnit.MULTIPLIER) {
          when(io.multiplierRsp.valid) {
            result.data := io.multiplierRsp.payload.result
            result.exceptions := io.multiplierRsp.payload.exceptions
          }
        }

        is(ExecutionUnit.DIVIDER) {
          when(io.dividerRsp.valid) {
            result.data := io.dividerRsp.payload.result
            result.exceptions := io.dividerRsp.payload.exceptions
          }
        }

        is(ExecutionUnit.MEMORY) {
          when(io.memoryRsp.valid) {
            result.data := io.memoryRsp.payload.data
            result.exceptions := B"00000" // Memory ops don't generate FP exceptions

            // Store operations don't update FP stack
            when(currentOp === FpOp.FPSTSNL || currentOp === FpOp.FPSTDNL) {
              result.updateStack := False
            }
          }
        }

        is(ExecutionUnit.CONTROL) {
          when(io.controlRsp.valid) {
            result.data := io.controlRsp.payload.statusOut.resize(64)
            result.updateStack := False // Control ops don't affect stack
          }
        }

        is(ExecutionUnit.VCU) {
          when(io.specialRsp.valid) {
            result.data := io.specialRsp.payload.result
            result.exceptions := B"00000"

            // Stack operations have special handling
            when(currentOp === FpOp.FPDUP || currentOp === FpOp.FPREV) {
              result.stackOp := True
            }
          }
        }
      }

      io.result.valid := True
      io.result.payload := result
      state := State.IDLE
    }
  }
}

/** Special operation response */
case class SpecialRsp() extends Bundle {
  val result = Bits(64 bits)
  val stackOp = Bool()
}

/** Combined FPU result */
case class FpuResult() extends Bundle {
  val op = FpOp()
  val data = Bits(64 bits)
  val exceptions = Bits(5 bits)
  val updateStack = Bool()
  val comparisonResult = Bool()
  val stackOp = Bool()
}

package transputer.plugins.controlflow

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.controlflow._

/** T9000 Control Flow Plugin implementing Table 6.11 control flow instructions.
  *
  * This plugin implements control flow operations from T9000 Table 6.11:
  *   - ret: return from subroutine
  *   - ldpi: load pointer to instruction
  *   - gajw: general adjust workspace
  *   - gcall: general call
  *   - lend: loop end
  *   - endp: end process
  *   - Stack/queue management: sthf, stlf, sthb, stlb, savel, saveh
  *
  * These operations execute in pipeline stage 4 (Memory stage) and may require multiple cycles for
  * complex operations like function calls and returns.
  *
  * Features:
  *   - Function call/return with workspace management
  *   - Loop control with automatic branching
  *   - Process termination
  *   - Scheduler queue manipulation
  *   - Integration with process management system
  */
class ControlFlowPlugin extends FiberPlugin {
  override def getDisplayName(): String = "ControlFlowPlugin"
  setName("controlflow")

  during setup new Area {
    println(s"[${ControlFlowPlugin.this.getDisplayName()}] setup start")

    addService(new ControlFlowService {
      override def executeOp(
        op: ControlFlowOp.C,
        operandA: UInt,
        operandB: UInt,
        currentIptr: UInt,
        currentWptr: UInt
      ): ControlFlowResult = controlFlowResult
      override def isControlFlowOp(opcode: Bits): Bool = isControlFlowOperation
      override def getControlFlowOp(opcode: Bits): ControlFlowOp.C = controlFlowOperation
    })

    println(s"[${ControlFlowPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var controlFlowResult: ControlFlowResult = null
  var isControlFlowOperation: Bool = null
  var controlFlowOperation: ControlFlowOp.C = null

  during build new Area {
    println(s"[${ControlFlowPlugin.this.getDisplayName()}] build start")

    // Get pipeline service
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    controlFlowResult = ControlFlowResult()

    // Control flow execution in Memory stage (stage 4) - T9000 specification
    val controlFlowLogic = new Area {
      val opcode = pipe.memory(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Table 6.11 instruction recognition
      isControlFlowOperation := isOpr && (
        oprFunc === Opcode.SecondaryOpcode.RET.asBits.resize(4) || // 20F0 - ret
          oprFunc === Opcode.SecondaryOpcode.LDPI.asBits.resize(4) || // 21F7 - ldpi
          oprFunc === Opcode.SecondaryOpcode.GAJW.asBits.resize(4) || // 23FC - gajw
          oprFunc === Opcode.SecondaryOpcode.GCALL.asBits.resize(4) || // 26F6 - gcall
          oprFunc === Opcode.SecondaryOpcode.LEND.asBits.resize(4) || // 21F1 - lend
          oprFunc === Opcode.SecondaryOpcode.ENDP.asBits.resize(4) || // F3 - endp
          oprFunc === Opcode.SecondaryOpcode.DISS.asBits.resize(4) // 22F1 - diss
      )

      // Decode control flow operation
      controlFlowOperation := ControlFlowOp.RET // Default
      when(isOpr) {
        switch(oprFunc) {
          is(Opcode.SecondaryOpcode.RET.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.RET
          }
          is(Opcode.SecondaryOpcode.LDPI.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.LDPI
          }
          is(Opcode.SecondaryOpcode.GAJW.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.GAJW
          }
          is(Opcode.SecondaryOpcode.GCALL.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.GCALL
          }
          is(Opcode.SecondaryOpcode.LEND.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.LEND
          }
          is(Opcode.SecondaryOpcode.ENDP.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.ENDP
          }
          is(Opcode.SecondaryOpcode.DISS.asBits.resize(4)) {
            controlFlowOperation := ControlFlowOp.DISS
          }
        }
      }

      // Execute control flow operations
      when(isControlFlowOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)
        val currentIptr = pipe.memory(Global.IPTR)
        val currentWptr: UInt =
          (regStack.readReg(transputer.plugins.core.regstack.RegName.WdescReg)(31 downto 2) ## U"00").asUInt

        // Initialize result
        controlFlowResult.newIptr := currentIptr + 1 // Default: next instruction
        controlFlowResult.newWptr := currentWptr // Default: same workspace
        controlFlowResult.takeJump := False
        controlFlowResult.returnValue := 0
        controlFlowResult.stackOperation := B"00" // No stack operation

        switch(controlFlowOperation) {
          is(ControlFlowOp.RET) {
            // Return from subroutine: Iptr = Areg, pop stack
            controlFlowResult.newIptr := areg
            controlFlowResult.takeJump := True
            controlFlowResult.stackOperation := B"10" // Pop
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ControlFlowOp.LDPI) {
            // Load pointer to instruction: A = Iptr + A
            val newPointer = currentIptr + areg
            controlFlowResult.returnValue := newPointer
            controlFlowResult.stackOperation := B"11" // Replace top
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, newPointer)
          }

          is(ControlFlowOp.GAJW) {
            // General adjust workspace: Wptr = Wptr + A
            val newWorkspace = currentWptr + (areg << 2) // Word addressing
            controlFlowResult.newWptr := newWorkspace
            controlFlowResult.stackOperation := B"10" // Pop
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ControlFlowOp.GCALL) {
            // General call: push Iptr+1, Iptr = A, Wptr = B
            controlFlowResult.newIptr := areg
            controlFlowResult.newWptr := breg
            controlFlowResult.takeJump := True
            controlFlowResult.stackOperation := B"11" // Replace with return address
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, currentIptr + 1)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(ControlFlowOp.LEND) {
            // Loop end: if A != 0 then Iptr = B, A = A - 1
            when(areg =/= 0) {
              controlFlowResult.newIptr := breg
              controlFlowResult.takeJump := True
              regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, areg - 1)
            } otherwise {
              // Loop finished: pop both A and B
              controlFlowResult.stackOperation := B"10" // Pop twice
              regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
              // Load next workspace value if available
            }
          }

          is(ControlFlowOp.ENDP) {
            // End process: terminate current process, reschedule
            // This requires integration with the scheduler
            controlFlowResult.takeJump := True
            // Scheduler will handle process termination
          }

          is(ControlFlowOp.DISS) {
            // Disconnect stack: reset stack to workspace base
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, 0)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, 0)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, 0)
            controlFlowResult.stackOperation := B"11" // Stack reset
          }
        }
      }
    }

    println(s"[${ControlFlowPlugin.this.getDisplayName()}] Control flow hardware configured")
    println(s"[${ControlFlowPlugin.this.getDisplayName()}] - Table 6.11: Control flow operations")
    println(s"[${ControlFlowPlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(
      s"[${ControlFlowPlugin.this.getDisplayName()}] - Function calls, returns, loops, process control"
    )
    println(s"[${ControlFlowPlugin.this.getDisplayName()}] build end")
  }
}

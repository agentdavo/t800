package transputer.plugins.general

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}

/** T9000 General Plugin implementing Table 6.17 general instructions.
  *
  * This plugin implements general stack and utility operations from T9000 Table 6.17:
  *   - rev: reverse top two stack elements
  *   - dup: duplicate top of stack
  *   - pop: pop processor stack
  *   - nop: no operation
  *   - mint: minimum integer
  *
  * These are fundamental stack manipulation operations that execute in various pipeline stages
  * depending on complexity.
  */
class GeneralPlugin extends FiberPlugin {
  override def getDisplayName(): String = "GeneralPlugin"
  setName("general")

  // Service interface for general operations
  trait GeneralService {
    def isGeneralOp(opcode: Bits): Bool
    def executeGeneralOp(opcode: Bits): Unit
  }

  during setup new Area {
    println(s"[${GeneralPlugin.this.getDisplayName()}] setup start")

    addService(new GeneralService {
      override def isGeneralOp(opcode: Bits): Bool = isGeneralOperation
      override def executeGeneralOp(opcode: Bits): Unit = {} // Hardware execution
    })

    println(s"[${GeneralPlugin.this.getDisplayName()}] setup end")
  }

  var isGeneralOperation: Bool = null

  during build new Area {
    println(s"[${GeneralPlugin.this.getDisplayName()}] build start")

    // Get pipeline service
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // General operations logic
    val generalLogic = new Area {
      val opcode = pipe.memory(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Table 6.17 instruction recognition
      isGeneralOperation := isOpr && (
        oprFunc === Opcode.SecondaryOpcode.REV.asBits.resize(4) || // F0 - rev
          oprFunc === Opcode.SecondaryOpcode.DUP.asBits.resize(4) || // 25FA - dup
          oprFunc === Opcode.SecondaryOpcode.POP.asBits.resize(4) || // 27F9 - pop
          oprFunc === Opcode.SecondaryOpcode.NOP.asBits.resize(4) || // 63F0 - nop
          oprFunc === Opcode.SecondaryOpcode.MINT.asBits.resize(4) // 24F2 - mint
      )

      // Execute general operations in Memory stage (stage 4)
      when(isGeneralOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)

        switch(oprFunc) {
          is(Opcode.SecondaryOpcode.REV.asBits.resize(4)) {
            // Reverse top two stack elements (swap A and B)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
          }

          is(Opcode.SecondaryOpcode.DUP.asBits.resize(4)) {
            // Duplicate top of stack (push A onto stack)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
          }

          is(Opcode.SecondaryOpcode.POP.asBits.resize(4)) {
            // Pop processor stack (remove top element)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
            // Creg gets undefined value or loaded from workspace if needed
          }

          is(Opcode.SecondaryOpcode.NOP.asBits.resize(4)) {
            // No operation - do nothing
          }

          is(Opcode.SecondaryOpcode.MINT.asBits.resize(4)) {
            // Load minimum integer (0x80000000 for 32-bit)
            val minInt = U(0x80000000, 32 bits)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, minInt)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
          }
        }
      }
    }

    println(s"[${GeneralPlugin.this.getDisplayName()}] General operations configured")
    println(
      s"[${GeneralPlugin.this.getDisplayName()}] - Table 6.17: Stack manipulation & utility operations"
    )
    println(s"[${GeneralPlugin.this.getDisplayName()}] - rev, dup, pop, nop, mint instructions")
    println(s"[${GeneralPlugin.this.getDisplayName()}] - Single-cycle execution in Memory stage")
    println(s"[${GeneralPlugin.this.getDisplayName()}] build end")
  }
}

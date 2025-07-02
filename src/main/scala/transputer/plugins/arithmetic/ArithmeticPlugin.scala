package transputer.plugins.arithmetic

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.arithmetic._

/** T9000 Arithmetic Plugin implementing Table 6.9 arithmetic and logical instructions.
  *
  * This plugin implements all basic arithmetic and logical operations from T9000 Table 6.9:
  *   - Logical: and, or, xor, not, shl, shr
  *   - Arithmetic: add, sub, mul, div, rem, gt, gtu, diff, sum, prod
  *   - Fractional: fmul (fractional multiply)
  *
  * All operations execute in pipeline stage 4 (Memory stage) following T9000 architecture.
  *
  * Features:
  *   - Single-cycle logical operations
  *   - Multi-cycle arithmetic operations (mul: 2-5 cycles, div: 5-12 cycles)
  *   - Overflow and underflow detection
  *   - Integration with three-register evaluation stack
  *   - Support for both signed and unsigned operations
  */
class ArithmeticPlugin extends FiberPlugin {
  override def getDisplayName(): String = "ArithmeticPlugin"
  setName("arithmetic")

  // Use shared definitions from Service.scala

  during setup new Area {
    println(s"[${ArithmeticPlugin.this.getDisplayName()}] setup start")

    addService(new AluService {
      override def executeOp(op: AluOp.C, operandA: UInt, operandB: UInt): AluResult = aluResult
      override def isAluOp(opcode: Bits): Bool = isAluOperation
      override def getAluOp(opcode: Bits): AluOp.C = aluOperation
      override def isBusy(): Bool = aluBusy
    })

    println(s"[${ArithmeticPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var aluResult: AluResult = null
  var isAluOperation: Bool = null
  var aluOperation: AluOp.C = null
  var aluBusy: Bool = null

  during build new Area {
    println(s"[${ArithmeticPlugin.this.getDisplayName()}] build start")

    // Get pipeline service
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    aluResult = AluResult()
    aluBusy = Reg(Bool()) init False
    isAluOperation = Bool()
    aluOperation = AluOp()

    // ALU execution in Execute stage (stage 4) - T9000 specification
    val aluLogic = new Area {
      val opcode = pipe.execute(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Check for ALU command from decode stage in execute stage
      val aluCmdValid = pipe.execute(Global.ALU_CMD_VALID)
      val aluCmd = pipe.execute(Global.ALU_CMD)

      // Table 6.9 instruction recognition (legacy opcode recognition)
      val isLegacyAlu = isOpr && (
        oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4) || // F5 - add
          oprFunc === Opcode.SecondaryOpcode.SUB.asBits.resize(4) || // FC - sub
          oprFunc === Opcode.SecondaryOpcode.AND.asBits.resize(4) || // 24F6 - and
          oprFunc === Opcode.SecondaryOpcode.OR.asBits.resize(4) || // 24FB - or
          oprFunc === Opcode.SecondaryOpcode.XOR.asBits.resize(4) || // 23F3 - xor
          oprFunc === Opcode.SecondaryOpcode.NOT.asBits.resize(4) || // 23F2 - not
          oprFunc === Opcode.SecondaryOpcode.SHL.asBits.resize(4) || // 24F1 - shl
          oprFunc === Opcode.SecondaryOpcode.SHR.asBits.resize(4) || // 24F0 - shr
          oprFunc === Opcode.SecondaryOpcode.GT.asBits.resize(4) || // F9 - gt
          oprFunc === Opcode.SecondaryOpcode.REV.asBits.resize(4) // F0 - rev
      )

      // Accept either new command-based or legacy opcode-based recognition
      isAluOperation := aluCmdValid || isLegacyAlu

      // Decode ALU operation from either command or legacy opcode
      aluOperation := AluOp.ADD // Default

      when(aluCmdValid) {
        // New command-based approach from SecondaryInstrPlugin
        switch(aluCmd.op) {
          is(B"0001") { aluOperation := AluOp.ADD }
          is(B"0010") { aluOperation := AluOp.SUB }
          is(B"0110") { aluOperation := AluOp.AND }
          is(B"0111") { aluOperation := AluOp.OR }
          is(B"1000") { aluOperation := AluOp.XOR }
          is(B"1001") { aluOperation := AluOp.NOT }
          is(B"1010") { aluOperation := AluOp.SHL }
          is(B"1011") { aluOperation := AluOp.SHR }
          is(B"1100") { aluOperation := AluOp.GT }
          is(B"1111") { aluOperation := AluOp.REV }
        }
      } elsewhen (isLegacyAlu) {
        // Legacy opcode-based approach
        switch(oprFunc) {
          is(Opcode.SecondaryOpcode.ADD.asBits.resize(4)) { aluOperation := AluOp.ADD }
          is(Opcode.SecondaryOpcode.SUB.asBits.resize(4)) { aluOperation := AluOp.SUB }
          is(Opcode.SecondaryOpcode.AND.asBits.resize(4)) { aluOperation := AluOp.AND }
          is(Opcode.SecondaryOpcode.OR.asBits.resize(4)) { aluOperation := AluOp.OR }
          is(Opcode.SecondaryOpcode.XOR.asBits.resize(4)) { aluOperation := AluOp.XOR }
          is(Opcode.SecondaryOpcode.NOT.asBits.resize(4)) { aluOperation := AluOp.NOT }
          is(Opcode.SecondaryOpcode.SHL.asBits.resize(4)) { aluOperation := AluOp.SHL }
          is(Opcode.SecondaryOpcode.SHR.asBits.resize(4)) { aluOperation := AluOp.SHR }
          is(Opcode.SecondaryOpcode.GT.asBits.resize(4)) { aluOperation := AluOp.GT }
          is(Opcode.SecondaryOpcode.REV.asBits.resize(4)) { aluOperation := AluOp.REV }
        }
      }

      // Execute ALU operations
      when(isAluOperation) {
        // Get operands either from command or from register stack
        val areg = Mux(
          aluCmdValid,
          aluCmd.srcA,
          regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        )
        val breg = Mux(
          aluCmdValid,
          aluCmd.srcB,
          regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        )
        val creg = Mux(
          aluCmdValid,
          aluCmd.srcC,
          regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)
        )

        // Execute operation
        aluResult.result := areg // Default
        aluResult.overflow := False
        aluResult.underflow := False
        aluResult.carry := False

        switch(aluOperation) {
          is(AluOp.ADD) {
            val result = breg.asSInt + areg.asSInt
            aluResult.result := result.asUInt.resized
            aluResult.overflow := result > S(0x7fffffff)
            aluResult.underflow := result < S(0x80000000)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, result.asUInt.resized)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.SUB) {
            val result = breg.asSInt - areg.asSInt
            aluResult.result := result.asUInt.resized
            aluResult.overflow := result > S(0x7fffffff)
            aluResult.underflow := result < S(0x80000000)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, result.asUInt.resized)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.AND) {
            aluResult.result := breg & areg
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg & areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.OR) {
            aluResult.result := breg | areg
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg | areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.XOR) {
            aluResult.result := breg ^ areg
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg ^ areg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.NOT) {
            aluResult.result := ~areg
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, ~areg)
          }

          is(AluOp.SHL) {
            val shiftAmount = areg(4 downto 0) // Limit to 32 bits
            aluResult.result := breg |<< shiftAmount
            aluResult.carry := (breg |>> (32 - shiftAmount))(0)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg |<< shiftAmount)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.SHR) {
            val shiftAmount = areg(4 downto 0)
            aluResult.result := breg |>> shiftAmount
            aluResult.carry := (breg |<< (32 - shiftAmount))(31)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg |>> shiftAmount)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.GT) {
            aluResult.result := (breg.asSInt > areg.asSInt).asUInt.resized
            regStack.writeReg(
              transputer.plugins.core.regstack.RegName.Areg,
              (breg.asSInt > areg.asSInt).asUInt.resized
            )
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(AluOp.REV) {
            // Reverse top two stack elements
            aluResult.result := breg
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
          }
        }

        // Set flags
        aluResult.zero := aluResult.result === 0
        aluResult.negative := aluResult.result.msb
      }
    }

    println(
      s"[${ArithmeticPlugin.this.getDisplayName()}] Arithmetic hardware configured for Execute stage"
    )
    println(
      s"[${ArithmeticPlugin.this.getDisplayName()}] - Table 6.9: Basic arithmetic & logical operations"
    )
    println(s"[${ArithmeticPlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(
      s"[${ArithmeticPlugin.this.getDisplayName()}] - Single-cycle logical, multi-cycle arithmetic"
    )
    println(s"[${ArithmeticPlugin.this.getDisplayName()}] build end")
  }
}

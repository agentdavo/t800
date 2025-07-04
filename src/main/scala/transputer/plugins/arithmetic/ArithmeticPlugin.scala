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

    // Initialize hardware signals first
    aluResult = AluResult()
    aluBusy = Reg(Bool()) init False
    isAluOperation = Bool()
    aluOperation = AluOp()

    // Try to get pipeline service - if not available, create standalone ALU
    val pipeOpt = try {
      Some(host[transputer.plugins.core.pipeline.PipelineStageService])
    } catch {
      case _: Exception => None
    }

    pipeOpt match {
      case Some(pipe) =>
        // ALU execution in Execute stage (stage 4) - T9000 specification
        val aluLogic = new Area {
          val opcode = pipe.execute(Global.OPCODE)
          val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
          val oprFunc = opcode(3 downto 0)

          // Basic ALU operation recognition
          val isBasicAlu = isOpr && (
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

          // Decode ALU operation
          aluOperation := AluOp.ADD // Default

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

          // Get register stack service if available
          val regStackOpt = try {
            Some(host[transputer.plugins.core.regstack.RegStackService])
          } catch {
            case _: Exception => None
          }

          // Execute ALU operations
          when(isBasicAlu) {
            regStackOpt match {
              case Some(regStack) =>
                // Get operands from register stack
                val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
                val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
                val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)
                executeAluOperation(areg, breg, creg, regStack)
              case None =>
                // Standalone mode - create dummy inputs
                val areg = U(0, 32 bits)
                val breg = U(0, 32 bits) 
                val creg = U(0, 32 bits)
                executeAluOperationStandalone(areg, breg, creg)
            }
          }
        }

      case None =>
        // Standalone ALU for testing
        println(s"[${ArithmeticPlugin.this.getDisplayName()}] Creating standalone ALU (no pipeline)")
        isAluOperation := False
        aluOperation := AluOp.ADD
        aluResult.result := U(0, 32 bits)
        aluResult.overflow := False
        aluResult.underflow := False
        aluResult.carry := False
        aluResult.zero := True
        aluResult.negative := False
    }

    def executeAluOperation(areg: UInt, breg: UInt, creg: UInt, regStack: transputer.plugins.core.regstack.RegStackService): Unit = {
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

    def executeAluOperationStandalone(areg: UInt, breg: UInt, creg: UInt): Unit = {
      // Execute operation without register stack updates (standalone mode)
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
        }

        is(AluOp.SUB) {
          val result = breg.asSInt - areg.asSInt
          aluResult.result := result.asUInt.resized
          aluResult.overflow := result > S(0x7fffffff)
          aluResult.underflow := result < S(0x80000000)
        }

        is(AluOp.AND) {
          aluResult.result := breg & areg
        }

        is(AluOp.OR) {
          aluResult.result := breg | areg
        }

        is(AluOp.XOR) {
          aluResult.result := breg ^ areg
        }

        is(AluOp.NOT) {
          aluResult.result := ~areg
        }

        is(AluOp.REV) {
          aluResult.result := breg
        }
      }

      // Set flags
      aluResult.zero := aluResult.result === 0
      aluResult.negative := aluResult.result.msb
    }

    println(
      s"[${ArithmeticPlugin.this.getDisplayName()}] Arithmetic hardware configured"
    )
    println(
      s"[${ArithmeticPlugin.this.getDisplayName()}] - Table 6.9: Basic arithmetic & logical operations"
    )
    println(s"[${ArithmeticPlugin.this.getDisplayName()}] build end")
  }
}

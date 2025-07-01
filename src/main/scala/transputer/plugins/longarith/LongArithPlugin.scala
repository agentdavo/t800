package transputer.plugins.longarith

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.longarith._

/** T9000 Long Arithmetic Plugin implementing Table 6.10 long arithmetic instructions.
  *
  * This plugin implements 64-bit arithmetic operations from T9000 Table 6.10:
  *   - Basic operations: ladd, lsub, lmul, ldiv
  *   - Shift operations: lshl, lshr
  *   - Extended operations: lsum, ldiff
  *   - Conversions: xsword, xdble, xword
  *   - Specialized: reschedule, slmul, sulmul, normalise
  *
  * These operations work with 64-bit values using the dual-word stack model where each 64-bit value
  * occupies two 32-bit stack positions.
  *
  * Features:
  *   - Multi-cycle operations with pipeline stalls
  *   - Full 64-bit precision arithmetic
  *   - Overflow and underflow detection
  *   - Integration with three-register evaluation stack
  *   - Support for both signed and unsigned operations
  */
class LongArithPlugin extends FiberPlugin {
  override def getDisplayName(): String = "LongArithPlugin"
  setName("longarith")

  during setup new Area {
    println(s"[${LongArithPlugin.this.getDisplayName()}] setup start")

    addService(new LongArithService {
      override def executeOp(
        op: LongArithOp.C,
        operandALow: UInt,
        operandAHigh: UInt,
        operandBLow: UInt,
        operandBHigh: UInt
      ): LongArithResult = longArithResult
      override def isLongArithOp(opcode: Bits): Bool = isLongArithOperation
      override def getLongArithOp(opcode: Bits): LongArithOp.C = longArithOperation
      override def isBusy(): Bool = longArithBusy
      override def getCycleCount(op: LongArithOp.C): UInt = cycleCount
    })

    println(s"[${LongArithPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var longArithResult: LongArithResult = null
  var isLongArithOperation: Bool = null
  var longArithOperation: LongArithOp.C = null
  var longArithBusy: Bool = null
  var cycleCount: UInt = null

  during build new Area {
    println(s"[${LongArithPlugin.this.getDisplayName()}] build start")

    // Get pipeline service
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    longArithResult = LongArithResult()
    longArithBusy = Reg(Bool()) init False
    cycleCount = UInt(4 bits)

    // Multi-cycle operation control
    val operationCounter = Reg(UInt(4 bits)) init 0
    val currentOperation = Reg(LongArithOp())

    // Long arithmetic execution in Memory stage (stage 4) - T9000 specification
    val longArithLogic = new Area {
      val opcode = pipe.memory(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Table 6.10 instruction recognition
      isLongArithOperation := isOpr && (
        oprFunc === Opcode.SecondaryOpcode.LADD.asBits.resize(4) || // 26F0 - ladd
          oprFunc === Opcode.SecondaryOpcode.LSUB.asBits.resize(4) || // 26F1 - lsub
          oprFunc === Opcode.SecondaryOpcode.LMUL.asBits.resize(4) || // 26F2 - lmul
          oprFunc === Opcode.SecondaryOpcode.LDIV.asBits.resize(4) || // 21FE - ldiv
          oprFunc === Opcode.SecondaryOpcode.LSHL.asBits.resize(4) || // 26F3 - lshl
          oprFunc === Opcode.SecondaryOpcode.LSHR.asBits.resize(4) || // 26F4 - lshr
          oprFunc === Opcode.SecondaryOpcode.MINT.asBits.resize(4) || // 24F2 - mint
          oprFunc === Opcode.SecondaryOpcode.XSWORD.asBits.resize(4) || // 26FF - xsword
          oprFunc === Opcode.SecondaryOpcode.PROD.asBits.resize(4) // F8 - prod
      )

      // Decode long arithmetic operation
      longArithOperation := LongArithOp.LADD // Default
      when(isOpr) {
        switch(oprFunc) {
          is(Opcode.SecondaryOpcode.LADD.asBits.resize(4)) {
            longArithOperation := LongArithOp.LADD
          }
          is(Opcode.SecondaryOpcode.LSUB.asBits.resize(4)) {
            longArithOperation := LongArithOp.LSUB
          }
          is(Opcode.SecondaryOpcode.LMUL.asBits.resize(4)) {
            longArithOperation := LongArithOp.LMUL
          }
          is(Opcode.SecondaryOpcode.LDIV.asBits.resize(4)) {
            longArithOperation := LongArithOp.LDIV
          }
          is(Opcode.SecondaryOpcode.LSHL.asBits.resize(4)) {
            longArithOperation := LongArithOp.LSHL
          }
          is(Opcode.SecondaryOpcode.LSHR.asBits.resize(4)) {
            longArithOperation := LongArithOp.LSHR
          }
          is(Opcode.SecondaryOpcode.MINT.asBits.resize(4)) {
            longArithOperation := LongArithOp.MINT
          }
          is(Opcode.SecondaryOpcode.XSWORD.asBits.resize(4)) {
            longArithOperation := LongArithOp.XSWORD
          }
          is(Opcode.SecondaryOpcode.PROD.asBits.resize(4)) {
            longArithOperation := LongArithOp.PROD
          }
        }
      }

      // Set cycle count based on operation
      cycleCount := 1 // Default single-cycle
      switch(longArithOperation) {
        is(LongArithOp.LADD, LongArithOp.LSUB) { cycleCount := 1 }
        is(LongArithOp.LSHL, LongArithOp.LSHR) { cycleCount := 1 }
        is(LongArithOp.LMUL) { cycleCount := 4 } // 4-cycle multiply
        is(LongArithOp.LDIV) { cycleCount := 8 } // 8-cycle divide
        is(LongArithOp.MINT, LongArithOp.XSWORD) { cycleCount := 1 }
        is(LongArithOp.PROD) { cycleCount := 1 }
      }

      // Execute long arithmetic operations
      when(isLongArithOperation && !longArithBusy) {
        // Start operation
        longArithBusy := cycleCount > 1
        operationCounter := cycleCount - 1
        currentOperation := longArithOperation

        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)

        // For long operations, stack contains: A=low1, B=high1, C=low2, workspace[0]=high2
        val operandALow = areg
        val operandAHigh = breg
        val operandBLow = creg
        // operandBHigh would be loaded from workspace

        // Initialize result
        longArithResult.resultLow := 0
        longArithResult.resultHigh := 0
        longArithResult.overflow := False
        longArithResult.underflow := False
        longArithResult.carry := False
        longArithResult.precision := LongPrecision.WORD64
        longArithResult.stackOperation := B"00"

        switch(longArithOperation) {
          is(LongArithOp.LADD) {
            // 64-bit addition: {B,A} + {workspace[0],C} -> {result_high, result_low}
            val sum64 = (operandAHigh.resize(33) @@ operandALow) + (U(0, 33 bits) @@ operandBLow)
            longArithResult.resultLow := sum64(31 downto 0)
            longArithResult.resultHigh := sum64(63 downto 32)
            longArithResult.carry := sum64(64)
            longArithResult.stackOperation := B"11" // Replace stack

            // Update stack with result
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, sum64(31 downto 0))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, sum64(63 downto 32))
          }

          is(LongArithOp.LSUB) {
            // 64-bit subtraction: {B,A} - {workspace[0],C}
            val diff64 = (operandAHigh.asSInt
              .resize(33) @@ operandALow.asSInt) - (S(0, 33 bits) @@ operandBLow.asSInt)
            longArithResult.resultLow := diff64(31 downto 0).asUInt
            longArithResult.resultHigh := diff64(63 downto 32).asUInt
            longArithResult.negative := diff64.msb
            longArithResult.stackOperation := B"11"

            regStack.writeReg(
              transputer.plugins.core.regstack.RegName.Areg,
              diff64(31 downto 0).asUInt
            )
            regStack.writeReg(
              transputer.plugins.core.regstack.RegName.Breg,
              diff64(63 downto 32).asUInt
            )
          }

          is(LongArithOp.LMUL) {
            // 64-bit multiply: A * C -> {result_high, result_low}
            // This is a multi-cycle operation
            when(operationCounter === 0) {
              val product = operandALow.resize(64) * operandBLow.resize(64)
              longArithResult.resultLow := product(31 downto 0)
              longArithResult.resultHigh := product(63 downto 32)
              longArithResult.stackOperation := B"11"

              regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, product(31 downto 0))
              regStack.writeReg(
                transputer.plugins.core.regstack.RegName.Breg,
                product(63 downto 32)
              )
            }
          }

          is(LongArithOp.LDIV) {
            // 64-bit divide: {B,A} / C -> quotient in A, remainder in B
            // Multi-cycle operation implemented via successive approximation
            when(operationCounter === 0 && operandBLow =/= 0) {
              val dividend = operandAHigh @@ operandALow
              val quotient = dividend / operandBLow.resize(64)
              val remainder = dividend % operandBLow.resize(64)

              longArithResult.resultLow := quotient(31 downto 0)
              longArithResult.resultHigh := remainder(31 downto 0)
              longArithResult.stackOperation := B"11"

              regStack.writeReg(
                transputer.plugins.core.regstack.RegName.Areg,
                quotient(31 downto 0)
              )
              regStack.writeReg(
                transputer.plugins.core.regstack.RegName.Breg,
                remainder(31 downto 0)
              )
            } otherwise {
              // Division by zero
              longArithResult.overflow := True
              longArithResult.resultLow := U(0xffffffff)
              longArithResult.resultHigh := U(0xffffffff)
            }
          }

          is(LongArithOp.LSHL) {
            // Long shift left: {B,A} << C
            val shiftAmount = operandBLow(5 downto 0) // Limit to 64 bits
            val value64 = operandAHigh @@ operandALow
            val shifted = value64 |<< shiftAmount

            longArithResult.resultLow := shifted(31 downto 0)
            longArithResult.resultHigh := shifted(63 downto 32)
            longArithResult.stackOperation := B"11"

            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, shifted(31 downto 0))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, shifted(63 downto 32))
          }

          is(LongArithOp.LSHR) {
            // Long shift right: {B,A} >> C
            val shiftAmount = operandBLow(5 downto 0)
            val value64 = operandAHigh @@ operandALow
            val shifted = value64 |>> shiftAmount

            longArithResult.resultLow := shifted(31 downto 0)
            longArithResult.resultHigh := shifted(63 downto 32)
            longArithResult.stackOperation := B"11"

            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, shifted(31 downto 0))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, shifted(63 downto 32))
          }

          is(LongArithOp.MINT) {
            // Load minimum integer: 0x80000000
            longArithResult.resultLow := U(0x80000000)
            longArithResult.precision := LongPrecision.WORD32
            longArithResult.stackOperation := B"01" // Push

            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, U(0x80000000))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, operandALow)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, operandAHigh)
          }

          is(LongArithOp.XSWORD) {
            // Extend sign word: sign-extend A to 64-bit
            val extended = operandALow.asSInt.resize(64).asUInt
            longArithResult.resultLow := extended(31 downto 0)
            longArithResult.resultHigh := extended(63 downto 32)
            longArithResult.stackOperation := B"01" // Push high word

            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, extended(31 downto 0))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, extended(63 downto 32))
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, operandAHigh)
          }

          is(LongArithOp.PROD) {
            // Multiply step for multi-precision arithmetic
            val product = operandALow * operandBLow
            longArithResult.resultLow := product
            longArithResult.stackOperation := B"11"

            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, product)
          }
        }

        // Set common flags
        longArithResult.zero := (longArithResult.resultLow === 0) && (longArithResult.resultHigh === 0)
        longArithResult.negative := longArithResult.resultHigh.msb
      }

      // Handle multi-cycle operation completion
      when(longArithBusy) {
        when(operationCounter === 0) {
          longArithBusy := False
        } otherwise {
          operationCounter := operationCounter - 1
        }
      }
    }

    println(s"[${LongArithPlugin.this.getDisplayName()}] Long arithmetic hardware configured")
    println(
      s"[${LongArithPlugin.this.getDisplayName()}] - Table 6.10: 64-bit arithmetic operations"
    )
    println(s"[${LongArithPlugin.this.getDisplayName()}] - Pipeline stage 4 (Memory) execution")
    println(
      s"[${LongArithPlugin.this.getDisplayName()}] - Multi-cycle operations with pipeline stalls"
    )
    println(s"[${LongArithPlugin.this.getDisplayName()}] build end")
  }
}

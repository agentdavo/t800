package transputer.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{
  Bmb,
  BmbParameter,
  BmbAccessParameter,
  BmbSourceParameter,
  BmbUnburstify,
  BmbDownSizerBridge,
  BmbUpSizerBridge
}
import transputer.Global
import transputer.{Transputer, Opcode}
import transputer.plugins.SystemBusService
import transputer.plugins.core.regstack.RegStackService
import transputer.plugins.core.regstack.RegName
import transputer.plugins.core.pipeline.{PipelineService, PipelineStageService}
import transputer.plugins.fpu.Utils._

class FpuPlugin extends FiberPlugin with PipelineService {
  override def getDisplayName(): String = "FpuPlugin"
  setName("fpu")
  val version = "FpuPlugin v0.3"
  val fpPipe = new StageCtrlPipeline

  // Pipeline payloads
  lazy val RESULT = Payload(Bits(64 bits))
  lazy val RESULT_AFIX = Payload(AFix(UQ(56, 0)))
  lazy val CYCLE_CNT = Payload(UInt(10 bits))
  lazy val MAX_CYCLES = Payload(UInt(10 bits))
  lazy val T805_STATE = Payload(Bits(64 bits))

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")

    // Create service flows in build phase where component context is available
    val srvCmd = Flow(FpCmd())
    val srvRsp = Flow(UInt(64 bits))
    srvCmd.setIdle()
    srvRsp.setIdle()

    // Register service after hardware creation
    addService(new FpuService {
      def pipe: Flow[FpCmd] = srvCmd
      def rsp: Flow[UInt] = srvRsp

      // Direct signal access (hybrid architecture)
      def FPA: Bits = B(0, 64 bits) // TODO: implement FP register access
      def FPB: Bits = B(0, 64 bits) // TODO: implement FP register access
      def FPC: Bits = B(0, 64 bits) // TODO: implement FP register access
      def FPStatus: Bits = B(0, 32 bits) // TODO: implement FP status access
      def roundingMode: Bits = B(0, 2 bits) // TODO: implement rounding mode access
      def errorFlags: Bits = B(0, 5 bits) // TODO: implement error flags access
      def isBusy: Bool = False // TODO: implement busy status

      override def setRoundingMode(mode: Bits): Unit = {
        // TODO: implement rounding mode setting
      }

      def updateRegisters(): Unit = {
        // TODO: implement register synchronization
      }
    })
    implicit val h: PluginHost = host
    val s0 = new fpPipe.Ctrl(0)
    val pipe = Plugin[PipelineStageService]
    val regStack = Plugin[RegStackService]
    // val systemBus = Plugin[SystemBusService].bus  // FPU doesn't need system bus access

    // FPU register file
    val fa = Reg(Bits(64 bits)) init 0 // FPAreg
    val fb = Reg(Bits(64 bits)) init 0 // FPBreg
    val fc = Reg(Bits(64 bits)) init 0 // FPCreg
    val tempA = Reg(Bits(64 bits)) init 0
    val tempB = Reg(Bits(64 bits)) init 0
    val status = new Area {
      val roundingMode = Reg(Bits(2 bits)) init 0 // 00: nearest, 01: zero, 10: positive, 11: minus
      val errorFlags = Reg(Bits(5 bits)) init 0 // Overflow, underflow, inexact, invalid, denormal
    }

    // Cycle counter using proper register to avoid combinatorial loops
    val cycleCounter = Reg(UInt(10 bits)) init 0

    // FPU doesn't need direct memory access - it operates on the evaluation stack

    // Execution units - create stub implementations for now
    // These would normally be Areas with IO bundles, but we'll simplify for compilation
    val adder = new FpuAdder

    // Stub multiplier signals
    val multiplierResult = Bits(64 bits)
    val multiplierResultAfix = AFix(UQ(56, 0))
    multiplierResult := B(0, 64 bits)
    multiplierResultAfix := AFix(U(0, 56 bits), 0 exp)

    // Stub divRoot signals
    val divRootResult = Bits(64 bits)
    val divRootResultAfix = AFix(UQ(56, 0))
    val divRootCycles = UInt(10 bits)
    val divRootT805State = Bits(64 bits)
    divRootResult := B(0, 64 bits)
    divRootResultAfix := AFix(U(0, 56 bits), 0 exp)
    divRootCycles := 10
    divRootT805State := B(0, 64 bits)

    // Stub rangeReducer signals
    val rangeReducerResult = Bits(64 bits)
    rangeReducerResult := B(0, 64 bits)

    // Stub VCU signals
    val vcuIsSpecial = Bool()
    val vcuSpecialResult = Bits(64 bits)
    val vcuTrapEnable = Bool()
    val vcuTrapType = UInt(8 bits)
    vcuIsSpecial := False
    vcuSpecialResult := B(0, 64 bits)
    vcuTrapEnable := False
    vcuTrapType := 0

    // Enhanced IEEE 754 exception flag handling
    val exceptionFlags = new Area {
      val current = Utils.Ieee754ExceptionFlags()
      val wasRounded = Reg(Bool()) init False

      // Clear flags at start of operation
      current.overflow := False
      current.underflow := False
      current.inexact := False
      current.invalid := False
      current.denormal := False

      // Update status register with exception flags
      def updateStatusFlags(): Unit = {
        when(current.overflow) { status.errorFlags(4) := True }
        when(current.underflow) { status.errorFlags(3) := True }
        when(current.inexact) { status.errorFlags(2) := True }
        when(current.invalid) { status.errorFlags(1) := True }
        when(current.denormal) { status.errorFlags(0) := True }
      }
    }

    // Enhanced FPU control service implementation
    addService(new FpuControlService {
      def specialValueDetected: Bool = vcuIsSpecial
      def specialResult: Bits = vcuSpecialResult
      def trapEnable: Bool = vcuTrapEnable
      def trapType: UInt = U(0, 8 bits) // Simplified trap type
      def roundingMode: Bits = status.roundingMode
      def setRoundingMode(mode: Bits): Unit = { status.roundingMode := mode }
      def getErrorFlags: Bits = status.errorFlags
      def clearErrorFlags: Unit = { status.errorFlags := 0 }
      def isFpuBusy(opcode: Bits): Bool = s0.isValid
    })

    // Service command/response provided during setup

    // Default adder handshake
    adder.io.cmd.valid := False
    adder.io.cmd.payload.a := B(0, 64 bits)
    adder.io.cmd.payload.b := B(0, 64 bits)
    adder.io.cmd.payload.sub := False
    adder.io.cmd.payload.rounding := status.roundingMode

    // FPU command handling - accept commands from decode stage in execute stage
    val fpuCmdValid = pipe.execute(Global.FPU_CMD_VALID)
    val fpuCmd = pipe.execute(Global.FPU_CMD)

    // Legacy opcode handling for direct FPU operations
    val opcode = pipe.execute(Global.OPCODE)
    val isLegacyFpuOp =
      opcode === Opcode.SecondaryOpcode.FPADD.asBits.resize(8) ||
        opcode === Opcode.SecondaryOpcode.FPSUB.asBits.resize(8) ||
        opcode === Opcode.SecondaryOpcode.FPMUL.asBits.resize(8) ||
        opcode === Opcode.SecondaryOpcode.FPDIV.asBits.resize(8)

    // Accept either pipeline command or legacy opcode recognition
    val isFpuOp = fpuCmdValid || isLegacyFpuOp
    val currentOpcode = Mux(fpuCmdValid, fpuCmd.op, opcode)

    val opCycles = UInt(10 bits)
    opCycles := 0
    switch(currentOpcode) {
      is(B"10101010") { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPADD.asBits.resize(8)) { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPSUB.asBits.resize(8)) { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPMUL.asBits.resize(8)) { opCycles := 3 }
      is(Opcode.SecondaryOpcode.FPDIV.asBits.resize(8)) { opCycles := divRootCycles }
      is(Opcode.SecondaryOpcode.FPSQRT.asBits.resize(8)) { opCycles := divRootCycles }
      is(Opcode.SecondaryOpcode.FPREM.asBits.resize(8)) { opCycles := divRootCycles }
      is(Opcode.SecondaryOpcode.FPRANGE.asBits.resize(8)) { opCycles := 17 }
      // Single-cycle operations
      is(Opcode.SecondaryOpcode.FPABS.asBits.resize(8)) { opCycles := 1 }
      is(Opcode.SecondaryOpcode.FPINT.asBits.resize(8)) { opCycles := 1 }
      is(Opcode.SecondaryOpcode.FPMULBY2.asBits.resize(8)) { opCycles := 1 }
      is(Opcode.SecondaryOpcode.FPDIVBY2.asBits.resize(8)) { opCycles := 1 }
      // Rounding mode operations (no cycles needed)
      is(Opcode.SecondaryOpcode.FPRN.asBits.resize(8)) { opCycles := 0 }
      is(Opcode.SecondaryOpcode.FPRP.asBits.resize(8)) { opCycles := 0 }
      is(Opcode.SecondaryOpcode.FPRM.asBits.resize(8)) { opCycles := 0 }
      is(Opcode.SecondaryOpcode.FPRZ.asBits.resize(8)) { opCycles := 0 }
      is(B"01000001", B"01000010", B"01000011", B"01011111", B"10010000") {
        opCycles := divRootCycles
      }
    }

    // Insert FPU operations into the pipeline
    s0.up.valid := pipe.execute.isValid && isFpuOp
    pipe.execute.haltWhen(s0.isValid && !s0.down.isReady)

    // Stall pipeline while operation is in progress
    s0.haltWhen(cycleCounter =/= 0)

    // Default assignments to avoid latches (must be outside when() to always assign)
    s0(RESULT) := B(0, 64 bits)
    s0(RESULT_AFIX) := AFix(U(0, 56 bits), 0 exp)
    s0(MAX_CYCLES) := 0
    s0(T805_STATE) := B(0, 64 bits)
    // CYCLE_CNT is handled in the cycle counter logic below

    when(s0.isValid) {

      // Use operands from pipeline command when available, otherwise use internal registers
      val operandA = Mux(fpuCmdValid, fpuCmd.srcA, fa)
      val operandB = Mux(fpuCmdValid, fpuCmd.srcB, fb)
      val operandC = Mux(fpuCmdValid, fpuCmd.srcC, fc)
      val roundingMode = Mux(fpuCmdValid, fpuCmd.roundingMode, status.roundingMode)

      // VCU logic would evaluate special values here
      // For now just use stub signals
      val op1Parsed = parseIeee754(operandA)
      val op2Parsed = parseIeee754(operandB)
      val op1Afix = AFix(op1Parsed.mantissa.asUInt, 0 exp)
      val op2Afix = AFix(op2Parsed.mantissa.asUInt, 0 exp)

      when(vcuIsSpecial) {
        s0(RESULT) := vcuSpecialResult
        when(vcuTrapEnable) {
          status.errorFlags(3) := True
        }
        tempA := fa
        tempB := fb
      } otherwise {
        switch(currentOpcode) {
          // Load/store
          is(B"10001110") { // fpldnlsn
            // TODO: Implement through integer pipeline
            // For now, just maintain stack state
            fb := fc
            fc := fc
          }
          is(B"10001010") { // fpldnldb
            // TODO: Implement through integer pipeline
            // For now, just maintain stack state
            fb := fc
            fc := fc
          }
          is(B"10101010") { // fpldnladdsn
            // TODO: Implement memory load through integer pipeline
            // memBmb.cmd.valid := True
            // memBmb.cmd.opcode := 0
            // memBmb.cmd.address := regStack.readReg(RegName.Areg) // Simplified - direct access
            // s0.haltWhen(!memBmb.rsp.valid)
            // when(memBmb.rsp.valid) {
            //   val loaded = real32ToReal64(memBmb.rsp.data(31 downto 0))
            //   adder.io.cmd.valid := True
            //   adder.io.cmd.payload.a := fa
            //   adder.io.cmd.payload.b := loaded
            //   adder.io.cmd.payload.sub := False
            //   adder.io.cmd.payload.rounding := status.roundingMode
            //   s0(RESULT) := adder.io.rsp.payload
            // }
            // For now, just maintain stack state
            s0(RESULT) := fa
          }
          // Other load/store (similar logic)

          // General ops
          is(Opcode.SecondaryOpcode.FPREV.asBits.resize(8)) { fa := fb; fb := fa }
          is(Opcode.SecondaryOpcode.FPDUP.asBits.resize(8)) { fc := fb; fb := fa }

          // Rounding ops
          is(Opcode.SecondaryOpcode.FPRN.asBits.resize(8)) { status.roundingMode := 0 }
          is(Opcode.SecondaryOpcode.FPRZ.asBits.resize(8)) { status.roundingMode := 1 }
          is(Opcode.SecondaryOpcode.FPRP.asBits.resize(8)) { status.roundingMode := 2 }
          is(Opcode.SecondaryOpcode.FPRM.asBits.resize(8)) { status.roundingMode := 3 }

          // Error ops not implemented in this stub

          // Comparison ops
          is(
            Opcode.SecondaryOpcode.FPGT.asBits.resize(8),
            Opcode.SecondaryOpcode.FPEQ.asBits.resize(8),
            Opcode.SecondaryOpcode.FPGE.asBits.resize(8),
            Opcode.SecondaryOpcode.FPLG.asBits.resize(8),
            Opcode.SecondaryOpcode.FPORDERED.asBits.resize(8),
            Opcode.SecondaryOpcode.FPNAN.asBits.resize(8),
            Opcode.SecondaryOpcode.FPNOTFINITE.asBits.resize(8)
          ) {
            // regfile.write(RegName.Areg, B(vcuComparisonResult).resize(64), 0, shadow = false) // TODO: adapt to RegStackService
          }

          // Conversion ops
          is(Opcode.SecondaryOpcode.FPR32TOR64.asBits.resize(8)) {
            s0(RESULT) := real32ToReal64(fa(31 downto 0))
          }
          is(Opcode.SecondaryOpcode.FPR64TOR32.asBits.resize(8)) {
            s0(RESULT) := real64ToReal32(fa, status.roundingMode).resize(64)
          }
          is(Opcode.SecondaryOpcode.FPRTOI32.asBits.resize(8)) {
            val intVal = realToInt32(fa)
            // regfile.write(RegName.Areg, intVal.asBits.resize(64), 0, shadow = false) // TODO: adapt to RegStackService
          }
          is(Opcode.SecondaryOpcode.FPI32TOR32.asBits.resize(8)) {
            s0(RESULT) := int32ToReal32(regStack.readReg(RegName.Areg).asSInt)
              .resize(64) // Simplified
          }
          // Other conversions

          // Arithmetic ops
          is(Opcode.SecondaryOpcode.FPADD.asBits.resize(8)) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := operandA
            adder.io.cmd.payload.b := operandB
            adder.io.cmd.payload.sub := False
            adder.io.cmd.payload.rounding := roundingMode
            s0(RESULT) := adder.io.rsp.payload

            // Generate IEEE 754 exception flags for addition
            val addFlags = Utils.generateExceptionFlags(
              operandA,
              operandB,
              adder.io.rsp.payload,
              Opcode.SecondaryOpcode.FPADD.asBits.resize(8),
              True
            )
            exceptionFlags.current.overflow := addFlags.overflow
            exceptionFlags.current.underflow := addFlags.underflow
            exceptionFlags.current.inexact := addFlags.inexact
            exceptionFlags.current.invalid := addFlags.invalid
            exceptionFlags.current.denormal := addFlags.denormal
            exceptionFlags.updateStatusFlags()
          }
          is(Opcode.SecondaryOpcode.FPSUB.asBits.resize(8)) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := operandA
            adder.io.cmd.payload.b := operandB
            adder.io.cmd.payload.sub := True
            adder.io.cmd.payload.rounding := roundingMode
            s0(RESULT) := adder.io.rsp.payload

            // Generate IEEE 754 exception flags for subtraction
            val subFlags = Utils.generateExceptionFlags(
              operandA,
              operandB,
              adder.io.rsp.payload,
              Opcode.SecondaryOpcode.FPSUB.asBits.resize(8),
              True
            )
            exceptionFlags.current.overflow := subFlags.overflow
            exceptionFlags.current.underflow := subFlags.underflow
            exceptionFlags.current.inexact := subFlags.inexact
            exceptionFlags.current.invalid := subFlags.invalid
            exceptionFlags.current.denormal := subFlags.denormal
            exceptionFlags.updateStatusFlags()
          }
          is(Opcode.SecondaryOpcode.FPMUL.asBits.resize(8)) {
            // Multiplier logic would go here
            s0(RESULT) := multiplierResult
            s0(RESULT_AFIX) := multiplierResultAfix
          }
          is(Opcode.SecondaryOpcode.FPDIV.asBits.resize(8)) {
            // DivRoot logic would go here
            s0(RESULT) := divRootResult
            s0(RESULT_AFIX) := divRootResultAfix
          }
          is(Opcode.SecondaryOpcode.FPSQRT.asBits.resize(8)) {
            // DivRoot sqrt logic would go here
            s0(RESULT) := divRootResult
            s0(RESULT_AFIX) := divRootResultAfix
          }
          is(Opcode.SecondaryOpcode.FPREM.asBits.resize(8)) {
            // DivRoot remainder logic would go here
            s0(RESULT) := divRootResult
            s0(RESULT_AFIX) := divRootResultAfix
          }
          is(Opcode.SecondaryOpcode.FPRANGE.asBits.resize(8)) {
            // Range reducer logic would go here
            s0(RESULT) := rangeReducerResult
          }

          // Missing IEEE 754 arithmetic operations
          is(Opcode.SecondaryOpcode.FPABS.asBits.resize(8)) {
            // Absolute value: clear sign bit
            val parsed = parseIeee754(operandA)
            s0(RESULT) := packIeee754(False, parsed.exponent, parsed.mantissa)
          }
          is(Opcode.SecondaryOpcode.FPINT.asBits.resize(8)) {
            // Round to integer in floating point format
            val rounded =
              Utils.roundIeee754Extended(operandA, 52, roundingMode, parseIeee754(operandA).sign)
            val parsed = parseIeee754(operandA)
            s0(RESULT) := packIeee754(parsed.sign, parsed.exponent, rounded)
            exceptionFlags.current.inexact := True
            exceptionFlags.updateStatusFlags()
          }
          is(Opcode.SecondaryOpcode.FPMULBY2.asBits.resize(8)) {
            // Multiply by 2: increment exponent
            val parsed = parseIeee754(operandA)
            val newExp = parsed.exponent + 1
            when(newExp >= 0x7ff) {
              // Overflow to infinity
              s0(RESULT) := Utils.genInfinity(parsed.sign)
              exceptionFlags.current.overflow := True
            } otherwise {
              s0(RESULT) := packIeee754(parsed.sign, newExp, parsed.mantissa)
            }
            exceptionFlags.updateStatusFlags()
          }
          is(Opcode.SecondaryOpcode.FPDIVBY2.asBits.resize(8)) {
            // Divide by 2: decrement exponent
            val parsed = parseIeee754(operandA)
            val newExp = parsed.exponent - 1
            when(newExp <= 0) {
              // Underflow to denormal/zero
              s0(RESULT) := B(0, 64 bits) // Simplified - should handle denormal properly
              exceptionFlags.current.underflow := True
            } otherwise {
              s0(RESULT) := packIeee754(parsed.sign, newExp, parsed.mantissa)
            }
            exceptionFlags.updateStatusFlags()
          }

          // Rounding mode operations - update status register
          is(Opcode.SecondaryOpcode.FPRN.asBits.resize(8)) {
            status.roundingMode := B"00" // Round to nearest
          }
          is(Opcode.SecondaryOpcode.FPRP.asBits.resize(8)) {
            status.roundingMode := B"10" // Round toward +infinity
          }
          is(Opcode.SecondaryOpcode.FPRM.asBits.resize(8)) {
            status.roundingMode := B"11" // Round toward -infinity
          }
          is(Opcode.SecondaryOpcode.FPRZ.asBits.resize(8)) {
            status.roundingMode := B"01" // Round toward zero
          }

          // Error flag operations (temporarily disabled for compilation)
          // is(FpOp.Error.FPSETERR.asBits.asUInt) {
          //   status.errorFlags := B"11111" // Set all error flags
          // }
          // is(FpOp.Error.FPCLRERR.asBits.asUInt) {
          //   status.errorFlags := B"00000" // Clear all error flags
          // }
          // is(FpOp.Error.FPTESTERR.asBits.asUInt) {
          //   regfile.write(RegName.Areg, status.errorFlags.resize(64), 0, shadow = false)
          // }
          // T805 compatibility
          is(B"01000001") { // fpusqrtfirst
            // T805 sqrt first step logic would go here
            s0(T805_STATE) := divRootT805State
          }
          is(B"01000010") { // fpusqrtstep
            // T805 sqrt step logic would go here
            s0(T805_STATE) := divRootT805State
          }
          is(B"01000011") { // fpusqrtlast
            // T805 sqrt last step logic would go here
            s0(RESULT) := divRootResult
          }
          is(B"01011111") { // fpremfirst (0x5F)
            // T805 rem first step logic would go here
            s0(T805_STATE) := divRootT805State
          }
          is(B"10010000") { // fpremstep (0x90)
            // T805 rem step logic would go here
            s0(T805_STATE) := divRootT805State
          }
        }
      }

      when(s0.down.isFiring && !vcuIsSpecial) {
        fa := s0(RESULT)
        fb := fc
        fc := fc // Simplified - keep current value
      }

      // Service command handling
      srvRsp.valid := False
      srvRsp.payload := U(s0(RESULT))
      when(srvCmd.valid) {
        fa := srvCmd.payload.a
        fb := srvCmd.payload.b
        srvRsp.valid := True
      }
    }

    // Cycle counter logic
    when(s0.up.isFiring) {
      s0(MAX_CYCLES) := opCycles
      cycleCounter := opCycles
    } elsewhen (s0.isValid && cycleCounter =/= 0) {
      cycleCounter := cycleCounter - 1
    } otherwise {
      cycleCounter := 0 // Default to 0 when not active
    }
    // Connect register to payload
    s0(CYCLE_CNT) := cycleCounter

    // Service implementation
    addService(new FpuOpsService {
      def push(operand: Bits): Unit = { fc := fb; fb := fa; fa := operand }
      def pushAfix(operand: AFix): Unit = { push(operand.raw) }
      def pop(): Bits = fa
      def popAfix(): AFix = AFix(fa.asSInt, 0 exp)
      def execute(opcode: Bits, operands: Vec[Bits]): Bits = {
        fa := operands(0); fb := operands(1); s0(RESULT)
      }
      def executeAfix(opcode: Bits, operands: Vec[AFix]): AFix = {
        fa := operands(0).raw; fb := operands(1).raw; s0(RESULT_AFIX)
      }
      def isBusy: Bool = s0(CYCLE_CNT) =/= 0
      def setRoundingMode(mode: Bits): Unit = { status.roundingMode := mode }
      def getErrorFlags: Bits = status.errorFlags
      def clearErrorFlags: Unit = { status.errorFlags := 0 }
    })

    addService(new FpuControlService {
      def specialValueDetected: Bool = vcuIsSpecial
      def specialResult: Bits = vcuSpecialResult
      def trapEnable: Bool = vcuTrapEnable
      def trapType: UInt = vcuTrapType
      def roundingMode: Bits = status.roundingMode
      def setRoundingMode(mode: Bits): Unit = { status.roundingMode := mode }
      def getErrorFlags: Bits = status.errorFlags
      def clearErrorFlags: Unit = { status.errorFlags := 0 }
      def isFpuBusy(opcode: Bits): Bool = s0(CYCLE_CNT) =/= 0
    })

    fpPipe.build()

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()
}

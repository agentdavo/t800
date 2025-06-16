package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbUnburstify, BmbDownSizerBridge}
import t800.{Global, T800, Opcodes}
import t800.plugins.{RegfileSrv, SystemBusSrv, TrapHandlerSrv}
import t800.plugins.registers.RegName
import t800.plugins.pipeline.{PipelineSrv, PipelineStageSrv}
import t800.plugins.fpu.Utils._

class FpuPlugin extends FiberPlugin with PipelineSrv {
  val version = "FpuPlugin v0.3"
  private val retain = Retainer()
  private val fpPipe = new StageCtrlPipeline

  // Pipeline payloads
  lazy val RESULT      = Payload(Bits(64 bits))
  lazy val RESULT_AFIX = Payload(AFix(UQ(56 bit, 0 bit)))
  lazy val CYCLE_CNT   = Payload(UInt(10 bits))
  lazy val MAX_CYCLES  = Payload(UInt(10 bits))
  lazy val T805_STATE  = Payload(Bits(64 bits))

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    println(s"[${this.getDisplayName()}] setup end")
  }

  lazy val logic = during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val s0 = fpPipe.ctrl(0)
    val pipe = Plugin[PipelineStageSrv]
    val regfile = Plugin[RegfileSrv]
    val systemBus = Plugin[SystemBusSrv].bus
    val trap = Plugin[TrapHandlerSrv]

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

    // BMB interface
    val memParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 64
      ),
      sourceCount = 1
    )
    val memBmb = Bmb(memParam)
    val unburstify = BmbUnburstify(memParam)
    val downSizer = BmbDownSizerBridge(
      inputParameter = T800.systemBusParam,
      outputParameter = memParam
    )
    memBmb >> unburstify.io.input >> downSizer.io.input >> systemBus

    // Execution units
    val adder = new FpuAdder
    val multiplier = new FpuMultiplier
    val divRoot = new FpuDivRoot
    val rangeReducer = new FpuRangeReducer
    val vcu = new FpuVCU

    // Default adder handshake
    adder.io.cmd.valid := False
    adder.io.cmd.payload.a := B(0, 64 bits)
    adder.io.cmd.payload.b := B(0, 64 bits)
    adder.io.cmd.payload.sub := False
    adder.io.cmd.payload.rounding := status.roundingMode



    // Opcode handling
    val opcode = pipe.execute(Global.OPCODE)
    val isFpuOp = opcode(7 downto 4).isOneOf(
      Opcodes.SecondaryEnum.FPADD, Opcodes.SecondaryEnum.FPSUB, Opcodes.SecondaryEnum.FPMUL,
      Opcodes.SecondaryEnum.FPDIV, Opcodes.SecondaryEnum.FPSQRT, Opcodes.SecondaryEnum.FPREM,
      Opcodes.SecondaryEnum.FPRANGE, Opcodes.SecondaryEnum.FPABS, Opcodes.SecondaryEnum.FPEXPINC32,
      Opcodes.SecondaryEnum.FPEXPDEC32, Opcodes.SecondaryEnum.FPMULBY2, Opcodes.SecondaryEnum.FPDIVBY2,
      Opcodes.SecondaryEnum.FPGE, Opcodes.SecondaryEnum.FPLG, Opcodes.SecondaryEnum.FPENTRY,
      Opcodes.SecondaryEnum.FPREV, Opcodes.SecondaryEnum.FPDUP, Opcodes.SecondaryEnum.FPRN,
      Opcodes.SecondaryEnum.FPRZ, Opcodes.SecondaryEnum.FPRP, Opcodes.SecondaryEnum.FPRM,
      Opcodes.SecondaryEnum.FPCHKERR, Opcodes.SecondaryEnum.FPTESTERR, Opcodes.SecondaryEnum.FPSETERR,
      Opcodes.SecondaryEnum.FPCLRERR, Opcodes.SecondaryEnum.FPGT, Opcodes.SecondaryEnum.FPEQ,
      Opcodes.SecondaryEnum.FPORDERED, Opcodes.SecondaryEnum.FPNAN, Opcodes.SecondaryEnum.FPNOTFINITE,
      Opcodes.SecondaryEnum.FPCHKI32, Opcodes.SecondaryEnum.FPCHKI64, Opcodes.SecondaryEnum.FPR32TOR64,
      Opcodes.SecondaryEnum.FPR64TOR32, Opcodes.SecondaryEnum.FPRTOI32, Opcodes.SecondaryEnum.FPI32TOR32,
      Opcodes.SecondaryEnum.FPI32TOR64, Opcodes.SecondaryEnum.FPB32TOR64, Opcodes.SecondaryEnum.FPNOROUND,
      Opcodes.SecondaryEnum.FPINT, Opcodes.SecondaryEnum.FPUSQRTFIRST, Opcodes.SecondaryEnum.FPUSQRTSTEP,
      Opcodes.SecondaryEnum.FPUSQRTLAST, Opcodes.SecondaryEnum.FPREMFIRST, Opcodes.SecondaryEnum.FPREMSTEP
    ) || opcode(7 downto 0).isOneOf(
      B"10001110", B"10001010", B"10000110", B"10000010", B"10011111", B"10100000",
      B"10101010", B"10100110", B"10101100", B"10101000", B"10001000", B"10000100",
      B"10011110"
    )

    s0.valid := pipe.execute.isValid && isFpuOp
    s0.up.ready := s0.down.ready
    s0.down.valid := s0.up.valid
    pipe.execute.haltWhen(s0.isValid && !s0.down.ready)

    s0.haltWhen(s0(CYCLE_CNT) < s0(MAX_CYCLES))
    s0.down.ready := True

    when(s0.isValid) {
      vcu.io.op1 := fa
      vcu.io.op2 := fb
      vcu.io.opcode := opcode
      val op1Parsed = parseIeee754(fa)
      val op2Parsed = parseIeee754(fb)
      val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
      val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

      when(vcu.io.isSpecial) {
        s0(RESULT) := vcu.io.specialResult
        when(vcu.io.trapEnable) {
          trap.trapType := vcu.io.trapType.asBits
          trap.trapAddr := pipe.execute(Fetch.FETCH_PC)
          status.errorFlags(3) := True
        }
        tempA := fa
        tempB := fb
      } otherwise {
        switch(opcode) {
          // Load/store
          is(B"10001110") { // fpldnlsn
            memBmb.cmd.valid := True
            memBmb.cmd.opcode := 0
            memBmb.cmd.address := regfile.read(RegName.Areg, 0).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              fa := real32ToReal64(memBmb.rsp.data(31 downto 0))
              fb := fc
              fc := regfile.read(RegName.FPCreg, 0).asBits
            }
          }
          is(B"10001010") { // fpldnldb
            memBmb.cmd.valid := True
            memBmb.cmd.opcode := 0
            memBmb.cmd.address := regfile.read(RegName.Areg, 0).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              fa := memBmb.rsp.data(63 downto 0)
              fb := fc
              fc := regfile.read(RegName.FPCreg, 0).asBits
            }
          }
          is(B"10101010") { // fpldnladdsn
            memBmb.cmd.valid := True
            memBmb.cmd.opcode := 0
            memBmb.cmd.address := regfile.read(RegName.Areg, 0).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              val loaded = real32ToReal64(memBmb.rsp.data(31 downto 0))
              adder.io.cmd.valid := True
              adder.io.cmd.payload.a := fa
              adder.io.cmd.payload.b := loaded
              adder.io.cmd.payload.sub := False
              adder.io.cmd.payload.rounding := status.roundingMode
              s0(RESULT) := adder.io.rsp.payload
              s0(MAX_CYCLES) := 2
              s0(CYCLE_CNT) := 0
            }
          }
          // Other load/store (similar logic)

          // General ops
          is(Opcodes.SecondaryEnum.FPENTRY) {}
          is(Opcodes.SecondaryEnum.FPREV) { fa := fb; fb := fa }
          is(Opcodes.SecondaryEnum.FPDUP) { fc := fb; fb := fa }

          // Rounding ops
          is(Opcodes.SecondaryEnum.FPRN) { status.roundingMode := 0 }
          is(Opcodes.SecondaryEnum.FPRZ) { status.roundingMode := 1 }
          is(Opcodes.SecondaryEnum.FPRP) { status.roundingMode := 2 }
          is(Opcodes.SecondaryEnum.FPRM) { status.roundingMode := 3 }

          // Error ops
          is(Opcodes.SecondaryEnum.FPCHKERR) {
            when(status.errorFlags.orR) {
              trap.trapType := vcu.io.trapType.asBits
              trap.trapAddr := pipe.execute(Fetch.FETCH_PC)
            }
          }
          is(Opcodes.SecondaryEnum.FPTESTERR) {
            regfile.write(RegName.Areg, status.errorFlags.orR.asUInt, 0, shadow = false)
            status.errorFlags := 0
          }
          is(Opcodes.SecondaryEnum.FPSETERR) { status.errorFlags := 0x1F }
          is(Opcodes.SecondaryEnum.FPCLRERR) { status.errorFlags := 0 }

          // Comparison ops
          is(Opcodes.SecondaryEnum.FPGT, Opcodes.SecondaryEnum.FPEQ,
             Opcodes.SecondaryEnum.FPGE, Opcodes.SecondaryEnum.FPLG,
             Opcodes.SecondaryEnum.FPORDERED, Opcodes.SecondaryEnum.FPNAN,
             Opcodes.SecondaryEnum.FPNOTFINITE) {
            regfile.write(RegName.Areg, vcu.io.comparisonResult.asUInt, 0, shadow = false)
          }

          // Conversion ops
          is(Opcodes.SecondaryEnum.FPR32TOR64) { s0(RESULT) := real32ToReal64(fa(31 downto 0)) }
          is(Opcodes.SecondaryEnum.FPR64TOR32) { s0(RESULT) := real64ToReal32(fa, status.roundingMode) }
          is(Opcodes.SecondaryEnum.FPRTOI32) {
            val intVal = realToInt32(fa)
            regfile.write(RegName.Areg, intVal.asUInt, 0, shadow = false)
          }
          is(Opcodes.SecondaryEnum.FPI32TOR32) {
            s0(RESULT) := int32ToReal32(regfile.read(RegName.Areg, 0).asSInt)
          }
          // Other conversions

          // Arithmetic ops
          is(Opcodes.SecondaryEnum.FPADD) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := fa
            adder.io.cmd.payload.b := fb
            adder.io.cmd.payload.sub := False
            adder.io.cmd.payload.rounding := status.roundingMode
            s0(RESULT) := adder.io.rsp.payload
                        s0(MAX_CYCLES) := 2
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPSUB) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := fa
            adder.io.cmd.payload.b := fb
            adder.io.cmd.payload.sub := True
            adder.io.cmd.payload.rounding := status.roundingMode
            s0(RESULT) := adder.io.rsp.payload
                        s0(MAX_CYCLES) := 2
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPMUL) {
            multiplier.io.op1 := fa
            multiplier.io.op2 := fb
            s0(RESULT) := multiplier.io.result
            s0(RESULT_AFIX) := multiplier.io.resultAfix
                        s0(MAX_CYCLES) := 3
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPDIV) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := False
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
                        s0(MAX_CYCLES) := 15
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPSQRT) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isRem := False
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
                        s0(MAX_CYCLES) := 15
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPREM) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := True
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
                        s0(MAX_CYCLES) := 529
            s0(CYCLE_CNT) := 0
          }
          is(Opcodes.SecondaryEnum.FPRANGE) {
            rangeReducer.io.op := fa
            s0(RESULT) := rangeReducer.io.result
                        s0(MAX_CYCLES) := 17
            s0(CYCLE_CNT) := 0
          }
          // T805 compatibility
          is(B"01000001") { // fpusqrtfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805First := True
            s0(T805_STATE) := divRoot.io.t805State
                        s0(MAX_CYCLES) := divRoot.io.cycles
            s0(CYCLE_CNT) := 0
          }
          is(B"01000010") { // fpusqrtstep
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Step := True
            s0(T805_STATE) := divRoot.io.t805State
                        s0(MAX_CYCLES) := divRoot.io.cycles
            s0(CYCLE_CNT) := 0
          }
          is(B"01000011") { // fpusqrtlast
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Last := True
                        s0(MAX_CYCLES) := divRoot.io.cycles
            s0(CYCLE_CNT) := 0
          }
          is(B"5F") { // fpremfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805First := True
            s0(T805_STATE) := divRoot.io.t805State
                        s0(MAX_CYCLES) := divRoot.io.cycles
            s0(CYCLE_CNT) := 0
          }
          is(B"90") { // fpremstep (context dependent)
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805Step := True
            s0(T805_STATE) := divRoot.io.t805State
                        s0(MAX_CYCLES) := divRoot.io.cycles
            s0(CYCLE_CNT) := 0
          }
        }
      }

      when(s0.down.isFiring && !vcu.io.isSpecial) {
        fa := s0(RESULT)
        fb := fc
        fc := regfile.read(RegName.FPCreg, 0).asBits
      }
    }

    // Cycle counter
    when(s0(CYCLE_CNT) < s0(MAX_CYCLES)) {
      s0(CYCLE_CNT) := s0(CYCLE_CNT) + 1
    } otherwise {
      s0(CYCLE_CNT) := 0
    }

    // Service implementation
    addService(new FpuOpsSrv {
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
      def isBusy: Bool = s0(CYCLE_CNT) < s0(MAX_CYCLES)
      def setRoundingMode(mode: Bits): Unit = { status.roundingMode := mode }
      def getErrorFlags: Bits = status.errorFlags
      def clearErrorFlags: Unit = { status.errorFlags := 0 }
    })

    addService(new FpuControlSrv {
      def specialValueDetected: Bool = vcu.io.isSpecial
      def specialResult: Bits = vcu.io.specialResult
      def trapEnable: Bool = vcu.io.trapEnable
      def trapType: UInt = vcu.io.trapType
      def roundingMode: Bits = status.roundingMode
    })

    fpPipe.build()

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()
}

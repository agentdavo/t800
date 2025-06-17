package t800.plugins.fpu

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
  BmbDownSizerBridge
}
import t800.{Global, T800, Opcode}
import t800.plugins.SystemBusSrv
import t800.plugins.registers.RegfileSrv
import t800.plugins.registers.RegName
import t800.plugins.pipeline.{PipelineSrv, PipelineStageSrv}
import t800.plugins.fpu.Utils._

class FpuPlugin extends FiberPlugin with PipelineSrv {
  val version = "FpuPlugin v0.3"
  private val retain = Retainer()
  val fpPipe = new StageCtrlPipeline

  // Pipeline payloads
  lazy val RESULT = Payload(Bits(64 bits))
  lazy val RESULT_AFIX = Payload(AFix(UQ(56, 0)))
  lazy val CYCLE_CNT = Payload(UInt(10 bits))
  lazy val MAX_CYCLES = Payload(UInt(10 bits))
  lazy val T805_STATE = Payload(Bits(64 bits))

  private var srvCmd: Flow[FpCmd] = null
  private var srvRsp: Flow[UInt] = null

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
    srvCmd = Flow(FpCmd())
    srvRsp = Flow(UInt(64 bits))
    srvCmd.setIdle()
    srvRsp.setIdle()
    addService(new FpuSrv {
      def pipe: Flow[FpCmd] = srvCmd
      def rsp: Flow[UInt] = srvRsp
    })
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val s0 = new fpPipe.Ctrl(0)
    val pipe = Plugin[PipelineStageSrv]
    val regfile = Plugin[RegfileSrv]
    val systemBus = Plugin[SystemBusSrv].bus

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
    val memParam = BmbAccessParameter(
      addressWidth = Global.ADDR_BITS,
      dataWidth = 64
    ).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 0)).toBmbParameter()
    val memBmb = Bmb(memParam)
    val unburstify = BmbUnburstify(memParam)
    val downSizer = BmbDownSizerBridge(
      inputParameter = T800.systemBusParam,
      outputParameter = memParam
    )
    memBmb >> unburstify.io.input
    unburstify.io.output >> downSizer.io.input
    downSizer.io.output >> systemBus

    // Execution units
    val adder = new FpuAdder
    val multiplier = new FpuMultiplier
    val divRoot = new FpuDivRoot
    val rangeReducer = new FpuRangeReducer
    val vcu = new FpuVCU

    // Service command/response provided during setup

    // Default adder handshake
    adder.io.cmd.valid := False
    adder.io.cmd.payload.a := B(0, 64 bits)
    adder.io.cmd.payload.b := B(0, 64 bits)
    adder.io.cmd.payload.sub := False
    adder.io.cmd.payload.rounding := status.roundingMode

    // Opcode handling
    val opcode = pipe.execute(Global.OPCODE)
    val isFpuOp =
      opcode === Opcode.SecondaryOpcode.FPADD.asBits ||
        opcode === Opcode.SecondaryOpcode.FPSUB.asBits ||
        opcode === Opcode.SecondaryOpcode.FPMUL.asBits ||
        opcode === Opcode.SecondaryOpcode.FPDIV.asBits

    val opCycles = UInt(10 bits)
    opCycles := 0
    switch(opcode) {
      is(B"10101010") { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPADD) { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPSUB) { opCycles := 2 }
      is(Opcode.SecondaryOpcode.FPMUL) { opCycles := 3 }
      is(Opcode.SecondaryOpcode.FPDIV) { opCycles := divRoot.io.cycles }
      is(Opcode.SecondaryOpcode.FPSQRT) { opCycles := divRoot.io.cycles }
      is(Opcode.SecondaryOpcode.FPREM) { opCycles := divRoot.io.cycles }
      is(Opcode.SecondaryOpcode.FPRANGE) { opCycles := 17 }
      is(B"01000001", B"01000010", B"01000011", B"5F", B"90") { opCycles := divRoot.io.cycles }
    }

    s0.up.valid := pipe.execute.isValid && isFpuOp
    s0.up.ready := s0.down.isReady
    s0.down.valid := s0.up.isValid
    pipe.execute.haltWhen(s0.isValid && !s0.down.isReady)

    s0.haltWhen(s0(CYCLE_CNT) =/= 0)
    s0.down.ready := True

    when(s0.isValid) {
      vcu.io.op1 := fa
      vcu.io.op2 := fb
      vcu.io.opcode := opcode
      val op1Parsed = parseIeee754(fa)
      val op2Parsed = parseIeee754(fb)
      val op1Afix = AFix(op1Parsed.mantissa.asUInt, 0 exp)
      val op2Afix = AFix(op2Parsed.mantissa.asUInt, 0 exp)

      when(vcu.io.isSpecial) {
        s0(RESULT) := vcu.io.specialResult
        when(vcu.io.trapEnable) {
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
            memBmb.cmd.address := regfile.read(RegName.Areg, 0, shadow = false).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              fa := real32ToReal64(memBmb.rsp.data(31 downto 0))
              fb := fc
              fc := regfile.read(RegName.FPCreg, 0, shadow = false).asBits
            }
          }
          is(B"10001010") { // fpldnldb
            memBmb.cmd.valid := True
            memBmb.cmd.opcode := 0
            memBmb.cmd.address := regfile.read(RegName.Areg, 0, shadow = false).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              fa := memBmb.rsp.data(63 downto 0)
              fb := fc
              fc := regfile.read(RegName.FPCreg, 0, shadow = false).asBits
            }
          }
          is(B"10101010") { // fpldnladdsn
            memBmb.cmd.valid := True
            memBmb.cmd.opcode := 0
            memBmb.cmd.address := regfile.read(RegName.Areg, 0, shadow = false).asUInt
            s0.haltWhen(!memBmb.rsp.valid)
            when(memBmb.rsp.valid) {
              val loaded = real32ToReal64(memBmb.rsp.data(31 downto 0))
              adder.io.cmd.valid := True
              adder.io.cmd.payload.a := fa
              adder.io.cmd.payload.b := loaded
              adder.io.cmd.payload.sub := False
              adder.io.cmd.payload.rounding := status.roundingMode
              s0(RESULT) := adder.io.rsp.payload
            }
          }
          // Other load/store (similar logic)

          // General ops
          is(Opcode.SecondaryOpcode.FPREV) { fa := fb; fb := fa }
          is(Opcode.SecondaryOpcode.FPDUP) { fc := fb; fb := fa }

          // Rounding ops
          is(Opcode.SecondaryOpcode.FPRN) { status.roundingMode := 0 }
          is(Opcode.SecondaryOpcode.FPRZ) { status.roundingMode := 1 }
          is(Opcode.SecondaryOpcode.FPRP) { status.roundingMode := 2 }
          is(Opcode.SecondaryOpcode.FPRM) { status.roundingMode := 3 }

          // Error ops not implemented in this stub

          // Comparison ops
          is(
            Opcode.SecondaryOpcode.FPGT,
            Opcode.SecondaryOpcode.FPEQ,
            Opcode.SecondaryOpcode.FPGE,
            Opcode.SecondaryOpcode.FPLG,
            Opcode.SecondaryOpcode.FPORDERED,
            Opcode.SecondaryOpcode.FPNAN,
            Opcode.SecondaryOpcode.FPNOTFINITE
          ) {
            regfile.write(RegName.Areg, B(vcu.io.comparisonResult).resize(64), 0, shadow = false)
          }

          // Conversion ops
          is(Opcode.SecondaryOpcode.FPR32TOR64) { s0(RESULT) := real32ToReal64(fa(31 downto 0)) }
          is(Opcode.SecondaryOpcode.FPR64TOR32) {
            s0(RESULT) := real64ToReal32(fa, status.roundingMode)
          }
          is(Opcode.SecondaryOpcode.FPRTOI32) {
            val intVal = realToInt32(fa)
            regfile.write(RegName.Areg, intVal.asBits.resize(64), 0, shadow = false)
          }
          is(Opcode.SecondaryOpcode.FPI32TOR32) {
            s0(RESULT) := int32ToReal32(regfile.read(RegName.Areg, 0, shadow = false).asSInt)
          }
          // Other conversions

          // Arithmetic ops
          is(Opcode.SecondaryOpcode.FPADD) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := fa
            adder.io.cmd.payload.b := fb
            adder.io.cmd.payload.sub := False
            adder.io.cmd.payload.rounding := status.roundingMode
            s0(RESULT) := adder.io.rsp.payload
          }
          is(Opcode.SecondaryOpcode.FPSUB) {
            adder.io.cmd.valid := True
            adder.io.cmd.payload.a := fa
            adder.io.cmd.payload.b := fb
            adder.io.cmd.payload.sub := True
            adder.io.cmd.payload.rounding := status.roundingMode
            s0(RESULT) := adder.io.rsp.payload
          }
          is(Opcode.SecondaryOpcode.FPMUL) {
            multiplier.io.op1 := fa
            multiplier.io.op2 := fb
            s0(RESULT) := multiplier.io.result
            s0(RESULT_AFIX) := multiplier.io.resultAfix
          }
          is(Opcode.SecondaryOpcode.FPDIV) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := False
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
          }
          is(Opcode.SecondaryOpcode.FPSQRT) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isRem := False
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
          }
          is(Opcode.SecondaryOpcode.FPREM) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := True
            s0(RESULT) := divRoot.io.result
            s0(RESULT_AFIX) := divRoot.io.resultAfix
          }
          is(Opcode.SecondaryOpcode.FPRANGE) {
            rangeReducer.io.op := fa
            s0(RESULT) := rangeReducer.io.result
          }
          // T805 compatibility
          is(B"01000001") { // fpusqrtfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805First := True
            s0(T805_STATE) := divRoot.io.t805State
          }
          is(B"01000010") { // fpusqrtstep
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Step := True
            s0(T805_STATE) := divRoot.io.t805State
          }
          is(B"01000011") { // fpusqrtlast
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Last := True

          }
          is(B"5F") { // fpremfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805First := True
            s0(T805_STATE) := divRoot.io.t805State
          }
          is(B"90") { // fpremstep (context dependent)
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805Step := True
            s0(T805_STATE) := divRoot.io.t805State
          }
        }
      }

      when(s0.down.isFiring && !vcu.io.isSpecial) {
        fa := s0(RESULT)
        fb := fc
        fc := regfile.read(RegName.FPCreg, 0, shadow = false).asBits
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

    // Cycle counter
    when(s0.up.isFiring) {
      s0(MAX_CYCLES) := opCycles
      s0(CYCLE_CNT) := opCycles
    } elsewhen (s0.isValid && s0(CYCLE_CNT) =/= 0) {
      s0(CYCLE_CNT) := s0(CYCLE_CNT) - 1
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
      def isBusy: Bool = s0(CYCLE_CNT) =/= 0
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

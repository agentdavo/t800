package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbUnburstify, BmbDownSizerBridge}
import t800.{Global, T800, Opcodes}
import t800.plugins.{PipelineSrv, RegfileService, SystemBusSrv, TrapHandlerSrv}
import t800.plugins.registers.RegName
import t800.plugins.pipeline.PipelineService
import t800.plugins.fpu.Utils._

class FpuPlugin extends FiberPlugin with PipelineService {
  val version = "FpuPlugin v0.3"
  private val retain = Retainer()

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
    val pipe = Plugin[PipelineSrv]
    val regfile = Plugin[RegfileService]
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
    adder.io.roundingMode := status.roundingMode
    val multiplier = new FpuMultiplier
    val divRoot = new FpuDivRoot
    val rangeReducer = new FpuRangeReducer
    val vcu = new FpuVCU

    // Microcode state
    val microcode = new Area {
      val state = Reg(UInt(3 bits)) init 0
      val cycles = Reg(UInt(10 bits)) init 0
      val maxCycles = Reg(UInt(10 bits)) init 0
      val isMultiCycle = Reg(Bool()) init False
      val t805State = Reg(Bits(64 bits)) init 0 // For fpusqrtfirst, fpremfirst
    }

    // Opcode handling
    val opcode = pipe.execute(Global.OPCODE)
    val isFpuOp = opcode(7 downto 4).isOneOf(
      Opcodes.SecondaryEnum.FPADD,
      Opcodes.SecondaryEnum.FPSUB,
      Opcodes.SecondaryEnum.FPMUL,
      Opcodes.SecondaryEnum.FPDIV,
      Opcodes.SecondaryEnum.FPSQRT,
      Opcodes.SecondaryEnum.FPREM,
      Opcodes.SecondaryEnum.FPRANGE,
      Opcodes.SecondaryEnum.FPABS,
      Opcodes.SecondaryEnum.FPEXPINC32,
      Opcodes.SecondaryEnum.FPEXPDEC32,
      Opcodes.SecondaryEnum.FPMULBY2,
      Opcodes.SecondaryEnum.FPDIVBY2,
      Opcodes.SecondaryEnum.FPGE,
      Opcodes.SecondaryEnum.FPLG,
      Opcodes.SecondaryEnum.FPENTRY,
      Opcodes.SecondaryEnum.FPREV,
      Opcodes.SecondaryEnum.FPDUP,
      Opcodes.SecondaryEnum.FPRN,
      Opcodes.SecondaryEnum.FPRZ,
      Opcodes.SecondaryEnum.FPRP,
      Opcodes.SecondaryEnum.FPRM,
      Opcodes.SecondaryEnum.FPCHKERR,
      Opcodes.SecondaryEnum.FPTESTERR,
      Opcodes.SecondaryEnum.FPSETERR,
      Opcodes.SecondaryEnum.FPCLRERR,
      Opcodes.SecondaryEnum.FPGT,
      Opcodes.SecondaryEnum.FPEQ,
      Opcodes.SecondaryEnum.FPORDERED,
      Opcodes.SecondaryEnum.FPNAN,
      Opcodes.SecondaryEnum.FPNOTFINITE,
      Opcodes.SecondaryEnum.FPCHKI32,
      Opcodes.SecondaryEnum.FPCHKI64,
      Opcodes.SecondaryEnum.FPR32TOR64,
      Opcodes.SecondaryEnum.FPR64TOR32,
      Opcodes.SecondaryEnum.FPRTOI32,
      Opcodes.SecondaryEnum.FPI32TOR32,
      Opcodes.SecondaryEnum.FPI32TOR64,
      Opcodes.SecondaryEnum.FPB32TOR64,
      Opcodes.SecondaryEnum.FPNOROUND,
      Opcodes.SecondaryEnum.FPINT,
      Opcodes.SecondaryEnum.FPUSQRTFIRST,
      Opcodes.SecondaryEnum.FPUSQRTSTEP,
      Opcodes.SecondaryEnum.FPUSQRTLAST,
      Opcodes.SecondaryEnum.FPREMFIRST,
      Opcodes.SecondaryEnum.FPREMSTEP
    ) || opcode(7 downto 0).isOneOf(
      B"10001110",
      B"10001010",
      B"10000110",
      B"10000010",
      B"10011111",
      B"10100000",
      B"10101010",
      B"10100110",
      B"10101100",
      B"10101000",
      B"10001000",
      B"10000100",
      B"10011110"
    )

    val result = Reg(Bits(64 bits)) init 0
    val resultAfix = Reg(AFix(UQ(56 bit, 0 bit))) init 0 // Extended for mul/div
    val busy = Reg(Bool()) init False

    when(isFpuOp && pipe.execute.isValid && !busy) {
      vcu.io.op1 := fa
      vcu.io.op2 := fb
      vcu.io.opcode := opcode
      val op1Parsed = parseIeee754(fa)
      val op2Parsed = parseIeee754(fb)
      val op1Afix = AFix(op1Parsed.mantissa.asUInt, 52 bit, 0 exp)
      val op2Afix = AFix(op2Parsed.mantissa.asUInt, 52 bit, 0 exp)

      when(vcu.io.isSpecial) {
        result := vcu.io.specialResult
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
            when(memBmb.rsp.valid) {
              val loaded = real32ToReal64(memBmb.rsp.data(31 downto 0))
              adder.io.op1 := fa
              adder.io.op2 := loaded
              adder.io.isAdd := True
              adder.io.isExpInc := False
              adder.io.isExpDec := False
              adder.io.isMulBy2 := False
              adder.io.isDivBy2 := False
              result := adder.io.result
              busy := adder.io.cycles > 0
              microcode.maxCycles := 2
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
          is(Opcodes.SecondaryEnum.FPSETERR) { status.errorFlags := 0x1f }
          is(Opcodes.SecondaryEnum.FPCLRERR) { status.errorFlags := 0 }

          // Comparison ops
          is(
            Opcodes.SecondaryEnum.FPGT,
            Opcodes.SecondaryEnum.FPEQ,
            Opcodes.SecondaryEnum.FPGE,
            Opcodes.SecondaryEnum.FPLG,
            Opcodes.SecondaryEnum.FPORDERED,
            Opcodes.SecondaryEnum.FPNAN,
            Opcodes.SecondaryEnum.FPNOTFINITE
          ) {
            regfile.write(RegName.Areg, vcu.io.comparisonResult.asUInt, 0, shadow = false)
          }

          // Conversion ops
          is(Opcodes.SecondaryEnum.FPR32TOR64) { result := real32ToReal64(fa(31 downto 0)) }
          is(Opcodes.SecondaryEnum.FPR64TOR32) { result := real64ToReal32(fa, status.roundingMode) }
          is(Opcodes.SecondaryEnum.FPRTOI32) {
            val intVal = realToInt32(fa)
            regfile.write(RegName.Areg, intVal.asUInt, 0, shadow = false)
          }
          is(Opcodes.SecondaryEnum.FPI32TOR32) {
            result := int32ToReal32(regfile.read(RegName.Areg, 0).asSInt)
          }
          // Other conversions

          // Arithmetic ops
          is(Opcodes.SecondaryEnum.FPADD) {
            adder.io.op1 := fa
            adder.io.op2 := fb
            adder.io.isAdd := True
            adder.io.isExpInc := False
            adder.io.isExpDec := False
            adder.io.isMulBy2 := False
            adder.io.isDivBy2 := False
            result := adder.io.result
            busy := adder.io.cycles > 0
            microcode.maxCycles := 2
          }
          is(Opcodes.SecondaryEnum.FPSUB) {
            adder.io.op1 := fa
            adder.io.op2 := fb
            adder.io.isAdd := False
            adder.io.isExpInc := False
            adder.io.isExpDec := False
            adder.io.isMulBy2 := False
            adder.io.isDivBy2 := False
            result := adder.io.result
            busy := adder.io.cycles > 0
            microcode.maxCycles := 2
          }
          is(Opcodes.SecondaryEnum.FPMUL) {
            multiplier.io.op1 := fa
            multiplier.io.op2 := fb
            result := multiplier.io.result
            busy := multiplier.io.cycles > 0
            microcode.maxCycles := 3
          }
          is(Opcodes.SecondaryEnum.FPDIV) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := False
            result := divRoot.io.result
            busy := divRoot.io.cycles > 0
            microcode.maxCycles := 15
            microcode.isMultiCycle := True
          }
          is(Opcodes.SecondaryEnum.FPSQRT) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isRem := False
            result := divRoot.io.result
            busy := divRoot.io.cycles > 0
            microcode.maxCycles := 15
            microcode.isMultiCycle := True
          }
          is(Opcodes.SecondaryEnum.FPREM) {
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isSqrt := False
            divRoot.io.isRem := True
            result := divRoot.io.result
            busy := divRoot.io.cycles > 0
            microcode.maxCycles := 529
            microcode.isMultiCycle := True
          }
          is(Opcodes.SecondaryEnum.FPRANGE) {
            rangeReducer.io.op := fa
            result := rangeReducer.io.result
            busy := rangeReducer.io.cycles > 0
            microcode.maxCycles := 17
            microcode.isMultiCycle := True
          }
          // T805 compatibility
          is(B"01000001") { // fpusqrtfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805First := True
            microcode.t805State := divRoot.io.t805State
            microcode.isMultiCycle := True
            microcode.maxCycles := divRoot.io.cycles
            busy := divRoot.io.cycles > 0
          }
          is(B"01000010") { // fpusqrtstep
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Step := True
            microcode.t805State := divRoot.io.t805State
            microcode.isMultiCycle := True
            microcode.maxCycles := divRoot.io.cycles
            busy := divRoot.io.cycles > 0
          }
          is(B"01000011") { // fpusqrtlast
            divRoot.io.op1 := fa
            divRoot.io.op2 := fa
            divRoot.io.isSqrt := True
            divRoot.io.isT805Last := True
            microcode.isMultiCycle := True
            microcode.maxCycles := divRoot.io.cycles
            busy := divRoot.io.cycles > 0
          }
          is(B"5F") { // fpremfirst
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805First := True
            microcode.t805State := divRoot.io.t805State
            microcode.isMultiCycle := True
            microcode.maxCycles := divRoot.io.cycles
            busy := divRoot.io.cycles > 0
          }
          is(B"90") { // fpremstep (context dependent)
            divRoot.io.op1 := fa
            divRoot.io.op2 := fb
            divRoot.io.isRem := True
            divRoot.io.isT805Step := True
            microcode.t805State := divRoot.io.t805State
            microcode.isMultiCycle := True
            microcode.maxCycles := divRoot.io.cycles
            busy := divRoot.io.cycles > 0
          }
        }
      }

      when(pipe.execute.down.isFiring && !vcu.io.isSpecial) {
        fa := result
        fb := fc
        fc := regfile.read(RegName.FPCreg, 0).asBits
      }
    }

    // Microcode execution
    when(microcode.isMultiCycle && busy) {
      microcode.cycles := microcode.cycles + 1
      when(microcode.cycles >= microcode.maxCycles) {
        busy := False
        microcode.cycles := 0
      }
    }

    // Service implementation
    addService(new FpuService {
      def push(operand: Bits): Unit = { fc := fb; fb := fa; fa := operand }
      def pushAfix(operand: AFix): Unit = { push(operand.raw) }
      def pop(): Bits = fa
      def popAfix(): AFix = AFix(fa.asSInt, 0 exp)
      def execute(opcode: Bits, operands: Vec[Bits]): Bits = {
        fa := operands(0); fb := operands(1); result
      }
      def executeAfix(opcode: Bits, operands: Vec[AFix]): AFix = {
        fa := operands(0).raw; fb := operands(1).raw; resultAfix
      }
      def isBusy: Bool = busy
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

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()
}

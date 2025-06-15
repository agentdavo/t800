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

class FpuPlugin extends FiberPlugin with PipelineService {
  val version = "FpuPlugin v0.1"
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
    val systemBus = Plugin[SystemBusSrv].bus // 128-bit BMB
    val trap = Plugin[TrapHandlerSrv]

    // FPU register file
    val fa = Reg(Bits(64 bits)) init 0 // FPAreg
    val fb = Reg(Bits(64 bits)) init 0 // FPBreg
    val fc = Reg(Bits(64 bits)) init 0 // FPCreg
    val tempA = Reg(Bits(64 bits)) init 0 // For multi-pass (denormals)
    val tempB = Reg(Bits(64 bits)) init 0 // For multi-pass (denormals)

    // BMB interface (64-bit for IEEE-754 double-precision)
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
    val vcu = new FpuVCU

    // Stack operations
    val opcode = pipe.execute(Global.OPCODE)
    val isFpuOp = opcode(7 downto 4).isOneOf(
      Opcodes.SecondaryEnum.FPADD,
      Opcodes.SecondaryEnum.FPSUB,
      Opcodes.SecondaryEnum.FPMUL,
      Opcodes.SecondaryEnum.FPDIV,
      Opcodes.SecondaryEnum.FPSQRT
    )
    val result = Reg(Bits(64 bits)) init 0
    val busy = Reg(Bool()) init False

    when(isFpuOp && pipe.execute.isValid && !busy) {
      // Read operands from FA, FB
      val op1 = fa
      val op2 = fb

      // VCU check for special values
      vcu.io.op1 := op1
      vcu.io.op2 := op2
      when(vcu.io.isSpecial) {
        result := vcu.io.specialResult
        when(vcu.io.trapEnable) {
          trap.trapType := 0x4 // IEEE-754 exception
          trap.trapAddr := pipe.execute(Fetch.FETCH_PC)
        }
        tempA := op1 // Store for multi-pass
        tempB := op2
      } otherwise {
        // Dispatch to execution units
        switch(opcode(7 downto 4)) {
          is(Opcodes.SecondaryEnum.FPADD) {
            adder.io.op1 := op1
            adder.io.op2 := op2
            adder.io.isAdd := True
            result := adder.io.result
            busy := adder.io.cycles > 0
          }
          is(Opcodes.SecondaryEnum.FPSUB) {
            adder.io.op1 := op1
            adder.io.op2 := op2
            adder.io.isAdd := False
            result := adder.io.result
            busy := adder.io.cycles > 0
          }
          is(Opcodes.SecondaryEnum.FPMUL) {
            multiplier.io.op1 := op1
            multiplier.io.op2 := op2
            result := multiplier.io.result
            busy := multiplier.io.cycles > 0
          }
          is(Opcodes.SecondaryEnum.FPDIV) {
            divRoot.io.op1 := op1
            divRoot.io.op2 := op2
            divRoot.io.isSqrt := False
            result := divRoot.io.result
            busy := divRoot.io.cycles > 0
          }
          is(Opcodes.SecondaryEnum.FPSQRT) {
            divRoot.io.op1 := op1
            divRoot.io.op2 := op1
            divRoot.io.isSqrt := True
            result := divRoot.io.result
            busy := divRoot.io.cycles > 0
          }
        }
      }

      // Update stack
      when(pipe.execute.down.isFiring && !vcu.io.isSpecial) {
        fa := result
        fb := fc
        fc := regfile.read(RegName.FPCreg, 0).asBits
      }
    }

    // Service implementation
    addService(new FpuService {
      def push(operand: Bits): Unit = {
        fc := fb
        fb := fa
        fa := operand
      }
      def pop(): Bits = fa
      def execute(opcode: Bits, operands: Vec[Bits]): Bits = {
        fa := operands(0)
        fb := operands(1)
        result
      }
      def isBusy: Bool = busy
    })

    addService(new FpuControlSrv {
      def specialValueDetected: Bool = vcu.io.isSpecial
      def specialResult: Bits = vcu.io.specialResult
      def trapEnable: Bool = vcu.io.trapEnable
      def trapType: UInt = 0x4
    })

    println(s"[${this.getDisplayName()}] build end")
  }

  override def getLinks(): Seq[Link] = Seq()
}

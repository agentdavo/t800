package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.plugins._
import transputer.plugins.fpu._

// Minimal timer service used by DummyTimerPlugin
trait TimerService {
  def hi: UInt
  def lo: UInt
  def set(value: UInt): Unit
  def enableHi(): Unit
  def enableLo(): Unit
  def disableHi(): Unit
  def disableLo(): Unit
}

// Minimal stand-in for the MMU trap service
case class TrapHandlerService() extends Bundle {
  val trapAddr = Bits(Global.ADDR_BITS bits)
  val trapType = Bits(4 bits)
  val trapEnable = Bool()
  val trapHandlerAddr = Bits(Global.ADDR_BITS bits)
}

/** Minimal timer plugin exposing [[TimerService]] without any logic. */
class DummyTimerPlugin extends FiberPlugin {
  private var hiReg, loReg: UInt = null
  during setup new Area {
    hiReg = Reg(UInt(Global.WordBits bits)) init 0
    loReg = Reg(UInt(Global.WordBits bits)) init 0
    addService(new TimerService {
      override def hi: UInt = hiReg
      override def lo: UInt = loReg
      override def set(value: UInt): Unit = {
        hiReg := value
        loReg := value
      }
      override def enableHi(): Unit = {}
      override def enableLo(): Unit = {}
      override def disableHi(): Unit = hiReg := hiReg
      override def disableLo(): Unit = loReg := loReg
    })
  }

  // Provide an empty build phase so awaitBuild() doesn't block
  during build new Area {}
}

/** Minimal FPU plugin exposing [[FpuService]] without arithmetic. */
class DummyFpuPlugin extends FiberPlugin {
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null
  during setup new Area {
    pipeReg = Flow(FpCmd())
    pipeReg.setIdle()
    rspReg = Flow(UInt(Global.WordBits bits))
    rspReg.setIdle()
    addService(new FpuService {
      override def pipe: Flow[FpCmd] = pipeReg
      override def rsp: Flow[UInt] = rspReg
      override def send(op: FpOp.C, a: UInt, b: UInt): Unit = {
        pipeReg.valid := True
        pipeReg.payload.op := op
        pipeReg.payload.a := a.asBits
        pipeReg.payload.b := b.asBits
      }
      override def resultValid: Bool = rspReg.valid
      override def result: UInt = rspReg.payload
    })
  }

  // Empty build stage required for the fiber engine
  during build new Area {}
}

/** Minimal trap handler plugin exposing [[TrapHandlerService]]. */
class DummyTrapPlugin extends FiberPlugin {
  private var trap: TrapHandlerService = null
  during setup new Area {
    val addr = Reg(Bits(Global.ADDR_BITS bits)) init 0
    val typ = Reg(Bits(4 bits)) init 0
    val enable = Reg(Bool()) init False
    val handler = Reg(Bits(Global.ADDR_BITS bits)) init 0
    trap = TrapHandlerService()
    trap.trapAddr := addr
    trap.trapType := typ
    trap.trapEnable := enable
    trap.trapHandlerAddr := handler
    addService(trap)
  }
  during build new Area {}
}

/** Minimal plugin providing [[FpuControlService]] without any FPU logic. */
class DummyFpuControlPlugin extends FiberPlugin {
  private var rounding: Bits = null
  private var flags: Bits = null
  during setup new Area {
    rounding = Reg(Bits(2 bits)) init 0
    flags = Reg(Bits(5 bits)) init 0
    addService(new FpuControlService {
      override def specialValueDetected: Bool = False
      override def specialResult: Bits = B(0, 64 bits)
      override def trapEnable: Bool = False
      override def trapType: UInt = U(0, 4 bits)
      override def roundingMode: Bits = rounding
      override def setRoundingMode(mode: Bits): Unit = rounding := mode
      override def getErrorFlags: Bits = flags
      override def clearErrorFlags: Unit = flags := 0
      override def isFpuBusy(opcode: Bits): Bool = False
    })
  }
  during build new Area {}
}

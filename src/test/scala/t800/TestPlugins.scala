package t800

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import t800.plugins._
import t800.plugins.fpu.FpuSrv
import t800.plugins.timers.TimerSrv

/** Minimal timer plugin exposing [[TimerSrv]] without any logic. */
class DummyTimerPlugin extends FiberPlugin {
  private var hiReg, loReg: UInt = null
  during setup new Area {
    hiReg = Reg(UInt(Global.WordBits bits)) init 0
    loReg = Reg(UInt(Global.WordBits bits)) init 0
    addService(new TimerSrv {
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

/** Minimal FPU plugin exposing [[FpuSrv]] without arithmetic. */
class DummyFpuPlugin extends FiberPlugin {
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null
  during setup new Area {
    pipeReg = Flow(FpCmd())
    pipeReg.setIdle()
    rspReg = Flow(UInt(Global.WordBits bits))
    rspReg.setIdle()
    addService(new FpuSrv {
      override def pipe: Flow[FpCmd] = pipeReg
      override def rsp: Flow[UInt] = rspReg
      override def send(op: FpOp.E, a: UInt, b: UInt): Unit = {
        pipeReg.valid := True
        pipeReg.payload.op := op
        pipeReg.payload.opa := a
        pipeReg.payload.opb := b
      }
      override def resultValid: Bool = rspReg.valid
      override def result: UInt = rspReg.payload
    })
  }

  // Empty build stage required for the fiber engine
  during build new Area {}
}

/** Minimal trap handler plugin exposing [[TrapHandlerSrv]]. */
class DummyTrapPlugin extends FiberPlugin {
  private var trap: TrapHandlerSrv = null
  during setup new Area {
    val addr = Reg(Bits(Global.ADDR_BITS bits)) init 0
    val typ = Reg(Bits(4 bits)) init 0
    val enable = Reg(Bool()) init False
    val handler = Reg(Bits(Global.ADDR_BITS bits)) init 0
    trap = TrapHandlerSrv()
    trap.trapAddr := addr
    trap.trapType := typ
    trap.trapEnable := enable
    trap.trapHandlerAddr := handler
    addService(trap)
  }
  during build new Area {}
}

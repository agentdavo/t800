package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import t800.Global

/** Minimal round-robin scheduler with high/low priority queues. */
class SchedulerPlugin extends FiberPlugin {
  private var cmdReg: Flow[SchedCmd] = null
  private var nextReg: UInt = null

  // simple stream FIFOs for high/low priority queues
  private var hiFifo: StreamFifo[UInt] = null
  private var loFifo: StreamFifo[UInt] = null

  private val retain = Retainer()

  during setup new Area {
    println(s"[${SchedulerPlugin.this.getDisplayName()}] setup start")
    cmdReg = Flow(SchedCmd())

    hiFifo = StreamFifo(UInt(Global.ADDR_BITS bits), Global.SCHED_QUEUE_DEPTH)
    loFifo = StreamFifo(UInt(Global.ADDR_BITS bits), Global.SCHED_QUEUE_DEPTH)
    hiFifo.io.push.setIdle()
    loFifo.io.push.setIdle()

    nextReg = Reg(UInt(Global.ADDR_BITS bits)) init 0

    addService(new SchedSrv {
      override def newProc: Flow[SchedCmd] = cmdReg
      override def nextProc: UInt = nextReg
    })
    retain()
    println(s"[${SchedulerPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${SchedulerPlugin.this.getDisplayName()}] build start")
    retain.await()
    val CMD_PTR = Payload(UInt(Global.ADDR_BITS bits))
    val CMD_HI = Payload(Bool())
    val in = CtrlLink()
    val toHi = CtrlLink()
    val toLo = CtrlLink()
    ForkLink(in.down, Seq(toHi.up, toLo.up))
    in.up.driveFrom(cmdReg) { (n, p) =>
      n(CMD_PTR) := p.ptr
      n(CMD_HI) := p.high
    }

    hiFifo.io.push.valid := toHi.isValid && toHi(CMD_HI)
    hiFifo.io.push.payload := toHi(CMD_PTR)
    toHi.haltWhen(hiFifo.io.push.valid && !hiFifo.io.push.ready)

    loFifo.io.push.valid := toLo.isValid && !toLo(CMD_HI)
    loFifo.io.push.payload := toLo(CMD_PTR)
    toLo.haltWhen(loFifo.io.push.valid && !loFifo.io.push.ready)

    val hiPop = hiFifo.io.pop
    val loPop = loFifo.io.pop
    hiPop.ready := False
    loPop.ready := False
    when(hiPop.valid) {
      nextReg := hiPop.payload
      hiPop.ready := True
    } elsewhen (loPop.valid) {
      nextReg := loPop.payload
      loPop.ready := True
    }
    println(s"[${SchedulerPlugin.this.getDisplayName()}] build end")
  }
}

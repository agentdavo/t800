package t800.plugins

import spinal.core._
import spinal.lib._
import t800.plugins._
import t800.TConsts

class SchedulerPlugin extends FiberPlugin {
  val schedCtrl = Flow(Bits(32 bits))
  val timerCtrl = Flow(Bits(32 bits))

  override def setup(): Unit = {
    schedCtrl.valid := False
    schedCtrl.payload := B(0, 32 bits)
    timerCtrl.valid := False
    timerCtrl.payload := B(0, 32 bits)
    addService(new SchedSrv { override def ctrl = schedCtrl })
    addService(new TimerSrv { override def ctrl = timerCtrl })
  }
}

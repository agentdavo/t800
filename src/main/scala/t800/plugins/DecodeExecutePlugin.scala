package t800.plugins

import spinal.core._
import spinal.lib._
import t800.plugins._

class DecodeExecutePlugin extends FiberPlugin {
  val ctrl = Flow(UInt(8 bits))

  override def setup(): Unit = {
    ctrl.valid := False
    ctrl.payload := 0
  }

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val stack = Plugin[StackSrv]
    val fpu = Plugin[FpuSrv]
    val sched = Plugin[SchedSrv]
    val timer = Plugin[TimerSrv]

    when(ctrl.valid) {
      switch(ctrl.payload) {
        is(U(0x30, 8 bits)) { // REV
          val tmp = stack.A
          stack.A := stack.B
          stack.B := tmp
        }
        is(U(0x94, 8 bits)) {
          val sum = stack.A + stack.B
          stack.A := sum
          stack.B := stack.C
        }
      }
    }
  }
}

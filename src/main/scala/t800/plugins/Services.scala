package t800.plugins

import spinal.core._
import spinal.lib._

trait LinkPins extends Area
trait DebugPins extends Area
trait ExtMemPins extends Area

trait StackSrv {
  val A: UInt
  val B: UInt
  val C: UInt
}

trait FpuSrv {
  def pipe: Flow[FpCmd]
  def rsp: Flow[UInt]
}

trait SchedSrv {
  def ctrl: Flow[Bits]
}

trait TimerSrv {
  def ctrl: Flow[Bits]
}

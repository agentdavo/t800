package transputer.plugins.schedule

import spinal.core._
import spinal.lib._

case class SchedCmd() extends Bundle {
  val ptr = UInt(transputer.Global.ADDR_BITS bits)
  val high = Bool()
}

trait SchedService {
  def newProc: Flow[SchedCmd]
  def nextProc: UInt
  def enqueue(ptr: UInt, high: Bool): Unit
  def terminateCurrent(): Unit
  def hiFront: UInt
  def hiBack: UInt
  def loFront: UInt
  def loBack: UInt
}

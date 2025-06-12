package t800.plugins

import spinal.core._
import t800.plugins._
import t800.TConsts

class StackPlugin extends FiberPlugin {
  val A = Reg(UInt(TConsts.WordBits bits)) init (0)
  val B = Reg(UInt(TConsts.WordBits bits)) init (0)
  val C = Reg(UInt(TConsts.WordBits bits)) init (0)

  override def setup(): Unit = {
    addService(new StackSrv {
      override val A: UInt = StackPlugin.this.A
      override val B: UInt = StackPlugin.this.B
      override val C: UInt = StackPlugin.this.C
    })
  }
}

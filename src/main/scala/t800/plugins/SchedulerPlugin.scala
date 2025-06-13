package t800.plugins

import spinal.core._
import spinal.lib._
import t800.Global

/** Minimal round-robin scheduler with high/low priority queues. */
class SchedulerPlugin extends FiberPlugin {
  private var cmdReg: Flow[SchedCmd] = null
  private var nextReg: UInt = null

  // simple ring buffers
  private var hiQ: Vec[UInt] = null
  private var hiHead, hiTail: UInt = null
  private var loQ: Vec[UInt] = null
  private var loHead, loTail: UInt = null

  override def setup(): Unit = {
    cmdReg = Flow(SchedCmd())

    hiQ = Vec.fill(Global.LINK_COUNT)(Reg(UInt(Global.ADDR_BITS bits)) init 0)
    hiHead = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0
    hiTail = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0
    loQ = Vec.fill(Global.LINK_COUNT)(Reg(UInt(Global.ADDR_BITS bits)) init 0)
    loHead = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0
    loTail = Reg(UInt(log2Up(Global.LINK_COUNT) bits)) init 0

    nextReg = Reg(UInt(Global.ADDR_BITS bits)) init 0

    addService(new SchedSrv {
      override def newProc: Flow[SchedCmd] = cmdReg
      override def nextProc: UInt = nextReg
    })
  }

  override def build(): Unit = {
    when(cmdReg.valid) {
      when(cmdReg.payload.high) {
        hiQ(hiTail) := cmdReg.payload.ptr
        hiTail := hiTail + 1
      } otherwise {
        loQ(loTail) := cmdReg.payload.ptr
        loTail := loTail + 1
      }
    }

    when(hiHead =/= hiTail) {
      nextReg := hiQ(hiHead)
      hiHead := hiHead + 1
    } elsewhen (loHead =/= loTail) {
      nextReg := loQ(loHead)
      loHead := loHead + 1
    }
  }
}

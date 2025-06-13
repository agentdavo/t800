package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim.SimManagerContext
import t800.TConsts

/** Simple high/low priority timers. High increments every cycle; low every 64 cycles. */
class TimerPlugin extends FiberPlugin {
  private var hiTimer: UInt = null
  private var loTimer: UInt = null
  private var loCnt: UInt = null
  private var loadReq: Bool = null
  private var loadVal: UInt = null
  private var hiEn: Bool = null
  private var loEn: Bool = null

  override def setup(): Unit = {
    hiTimer = Reg(UInt(TConsts.WordBits bits)) init (0)
    loTimer = Reg(UInt(TConsts.WordBits bits)) init (0)
    loCnt = Reg(UInt(6 bits)) init (0)
    loadReq = Reg(Bool()) init (False)
    loadVal = Reg(UInt(TConsts.WordBits bits)) init (0)
    hiEn = Reg(Bool()) init (True)
    loEn = Reg(Bool()) init (True)
    hiTimer.simPublic()
    loTimer.simPublic()
    hiEn.simPublic()
    loEn.simPublic()
    addService(new TimerSrv {
      override def hi: UInt = hiTimer
      override def lo: UInt = loTimer
      override def set(value: UInt): Unit = {
        loadReq := True
        loadVal := value
      }

      override def enableHi(): Unit =
        if (SimManagerContext.current != null) hiEn #= true else hiEn := True
      override def enableLo(): Unit =
        if (SimManagerContext.current != null) loEn #= true else loEn := True
      override def disableHi(): Unit =
        if (SimManagerContext.current != null) hiEn #= false else hiEn := False
      override def disableLo(): Unit =
        if (SimManagerContext.current != null) loEn #= false else loEn := False
    })
  }

  override def build(): Unit = {
    when(loadReq) {
      hiTimer := loadVal
      loTimer := loadVal
      loCnt := 0
    } otherwise {
      when(hiEn) {
        hiTimer := hiTimer + 1
      }
      when(loEn) {
        loCnt := loCnt + 1
        when(loCnt === 0) {
          loTimer := loTimer + 1
        }
      }
    }
    when(loadReq) { loadReq := False }
  }
}

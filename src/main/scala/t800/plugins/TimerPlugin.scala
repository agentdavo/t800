package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core.fiber.Retainer
import t800.Global

/** Simple high/low priority timers. High increments every cycle; low every 64 cycles. */
class TimerPlugin extends FiberPlugin {
  private var hiTimer: UInt = null
  private var loTimer: UInt = null
  private var loCnt: UInt = null
  private var loadReq: Bool = null
  private var loadVal: UInt = null
  private var hiEn: Bool = null
  private var loEn: Bool = null
  private val retain = Retainer()

  during setup new Area {
    hiTimer = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loTimer = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loCnt = Reg(UInt(6 bits)) init (0)
    loadReq = Reg(Bool()) init (False)
    loadVal = Reg(UInt(Global.WORD_BITS bits)) init (0)
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

      override def enableHi(): Unit = hiEn #= true
      override def enableLo(): Unit = loEn #= true
      override def disableHi(): Unit = hiEn #= false
      override def disableLo(): Unit = loEn #= false
    })
    retain()
  }

  during build new Area {
    retain.await()
    when(loadReq) {
      hiTimer := loadVal
      loTimer := loadVal
      loCnt := 0
    }

    when(!loadReq && hiEn) {
      hiTimer := hiTimer + 1
    }

    when(!loadReq && loEn) {
      loCnt := loCnt + 1
      when(loCnt === 0) {
        loTimer := loTimer + 1
      }
    }

    when(loadReq) { loadReq := False }
  }
}

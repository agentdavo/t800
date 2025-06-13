package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._

class GlobalDatabaseSpec extends AnyFunSuite {
  test("plugins read configuration from database") {
    val db = new Database
    db(Global.WORD_BITS) = 16
    db(Global.ADDR_BITS) = TConsts.AddrBits
    db(Global.ROM_WORDS) = TConsts.RomWords
    db(Global.RAM_WORDS) = TConsts.RamWords
    db(Global.LINK_COUNT) = TConsts.LinkCount
    db(Global.RESET_PC) = TConsts.ResetPC

    val plugins = Seq(new TimerPlugin, new TimerProbePlugin)
    SimConfig.compile(new T800(plugins, db)).doSim { dut =>
      val probe = dut.host.service[TimerProbeSrv]
      assert(probe.hi.getWidth == 16)
      assert(probe.lo.getWidth == 16)
    }
  }
}

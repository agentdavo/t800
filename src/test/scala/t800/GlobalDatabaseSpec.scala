package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import spinal.lib.misc.plugin.PluginHost

class GlobalDatabaseSpec extends AnyFunSuite {
  test("plugins read configuration from database") {
    val db = new Database
    db(Global.WORD_BITS) = 16
    db(Global.ADDR_BITS) = TConsts.AddrBits
    db(Global.PC_BITS) = TConsts.AddrBits
    db(Global.INSTR_BITS) = 8
    db(Global.IPTR_BITS) = TConsts.AddrBits
    db(Global.OPCODE_BITS) = 8
    db(Global.ROM_WORDS) = TConsts.RomWords
    db(Global.RAM_WORDS) = TConsts.RamWords
    db(Global.LINK_COUNT) = TConsts.LinkCount
    db(Global.FPU_PRECISION) = TConsts.FpuPrecision
    db(Global.SCHED_QUEUE_DEPTH) = TConsts.SchedQueueDepth
    db(Global.RESET_IPTR) = TConsts.ResetIPtr

    SimConfig
      .compile {
        PluginHost.on {
          val host = new PluginHost
          val plugins = Seq(new TimerPlugin, new TimerProbePlugin)
          new T800(host, plugins, db)
        }
      }
      .doSim { dut =>
        val probe = dut.host[TimerProbeSrv]
        assert(probe.hi.getWidth == 16)
        assert(probe.lo.getWidth == 16)
      }
  }
}

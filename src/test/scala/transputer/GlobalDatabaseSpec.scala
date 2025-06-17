package transputer

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import transputer.plugins._
import transputer.plugins.timers.TimerPlugin
import spinal.lib.misc.plugin.PluginHost

class GlobalDatabaseSpec extends AnyFunSuite {
  test("plugins read configuration from database") {
    val db = new Database
    db(Global.WORD_BITS) = 16
    db(Global.ADDR_BITS) = Global.AddrBits
    db(Global.PC_BITS) = Global.AddrBits
    db(Global.INSTR_BITS) = 8
    db(Global.IPTR_BITS) = Global.AddrBits
    db(Global.OPCODE_BITS) = 8
    db(Global.ROM_WORDS) = Global.RomWords
    db(Global.RAM_WORDS) = Global.RamWords
    db(Global.LINK_COUNT) = Global.LinkCount
    db(Global.FPU_PRECISION) = Global.FpuPrecision
    db(Global.SCHED_QUEUE_DEPTH) = Global.SchedQueueDepth
    db(Global.RESET_IPTR) = Global.ResetIPtr

    SimConfig
      .compile {
        val host = new PluginHost
        val plugins = Seq(new TimerPlugin, new TimerProbePlugin)
        PluginHost(host).on {
          Database(db).on(Transputer(plugins))
        }
      }
      .doSim { dut =>
        val probe = dut.host[TimerProbeSrv]
        assert(probe.hi.getWidth == 16)
        assert(probe.lo.getWidth == 16)
      }
  }
}

package t800

import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.database.Database
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._
import t800.plugins.fpu._
import spinal.lib.misc.plugin.PluginHost

class FpuDut extends Component {
  val io = new Bundle {
    val cmdValid = in Bool ()
    val op = in(FpOp())
    val a = in UInt (32 bits)
    val b = in UInt (32 bits)
    val rspValid = out Bool ()
    val rsp = out UInt (32 bits)
  }
  val host = new PluginHost
  val plugin = new FpuPlugin
  val db = T800.defaultDatabase()
  Database(db).on {
    host.asHostOf(Seq(plugin))
    plugin.awaitBuild()
  }
  val srv = host[FpuSrv]
  srv.pipe.valid := False
  when(io.cmdValid) { srv.send(io.op, io.a, io.b) }
  io.rspValid := srv.resultValid
  io.rsp := srv.result
}

class FpuPluginSpec extends AnyFunSuite {
  private def run(op: FpOp.E, a: Int, b: Int): Int = {
    var result = 0
    SimConfig.compile(PluginHost.on { new FpuDut }).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.op #= op
      dut.io.a #= a
      dut.io.b #= b
      dut.clockDomain.waitSampling()
      dut.io.cmdValid #= false
      while (!dut.io.rspValid.toBoolean) dut.clockDomain.waitSampling()
      result = dut.io.rsp.toBigInt.intValue
      dut.clockDomain.waitSampling()
    }
    result
  }

  test("FPADD") { assert(run(FpOp.ADD, 3, 4) == 7) }
  test("FPSUB") { assert(run(FpOp.SUB, 9, 5) == 4) }
  test("FPMUL") { assert(run(FpOp.MUL, 3, 5) == 15) }
  test("FPDIV") { assert(run(FpOp.DIV, 8, 2) == 4) }
}

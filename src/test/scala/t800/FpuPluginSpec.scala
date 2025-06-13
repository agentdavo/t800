package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins._

class FpuDut extends Component {
  val io = new Bundle {
    val cmdValid = in Bool ()
    val op = in Bits (3 bits)
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
  }
  val srv = host.service[FpuSrv]
  srv.pipe.valid := io.cmdValid
  srv.pipe.payload.op := io.op
  srv.pipe.payload.opa := io.a
  srv.pipe.payload.opb := io.b
  io.rspValid := srv.rsp.valid
  io.rsp := srv.rsp.payload
}

class FpuPluginSpec extends AnyFunSuite {
  private def run(op: Int, a: Int, b: Int): Int = {
    var result = 0
    SimConfig.compile(new FpuDut).doSim { dut =>
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

  test("FPADD") { assert(run(0, 3, 4) == 7) }
  test("FPSUB") { assert(run(1, 9, 5) == 4) }
  test("FPMUL") { assert(run(2, 3, 5) == 15) }
  test("FPDIV") { assert(run(3, 8, 2) == 4) }
}

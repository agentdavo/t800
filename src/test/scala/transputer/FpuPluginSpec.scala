package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost
import spinal.lib.misc.database.Database
import transputer.plugins.fpu._

/** Wrapper around [[FpuPlugin]] exposing a simple command interface. */
class FpuDut extends Component {
  val io = new Bundle {
    val cmdValid = in Bool ()
    val op = in(FpOp())
    val a = in Bits (64 bits)
    val b = in Bits (64 bits)
    val rounding = in Bits (2 bits)
    val rspValid = out Bool ()
    val rsp = out Bits (64 bits)
  }

  // Build minimal plugin stack with mock trap handler
  val plugins = Transputer.unitPlugins() ++ Seq(new DummyTrapPlugin, new FpuPlugin)
  val core = PluginHost.on {
    Database(Transputer.defaultDatabase()).on(Transputer(plugins))
  }
  val host = core.host
  val fpuService = host[FpuService]
  val ctrl = host[FpuControlService]

  when(io.cmdValid) {
    fpuService.send(io.op, io.a.asUInt, io.b.asUInt)
    ctrl.setRoundingMode(io.rounding)
    ctrl.clearErrorFlags
  }
  io.rsp := fpuService.rsp.payload.asBits
  io.rspValid := fpuService.rsp.valid && io.cmdValid
}

class FpuPluginSpec extends AnyFunSuite {
  private def run(op: FpOp.E, a: Double, b: Double, rm: Int = 0): Double = {
    var res = 0.0
    SimConfig.compile(new FpuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.op #= op
      dut.io.a #= BigInt(java.lang.Double.doubleToRawLongBits(a)) & BigInt("ffffffffffffffff", 16)
      dut.io.b #= BigInt(java.lang.Double.doubleToRawLongBits(b)) & BigInt("ffffffffffffffff", 16)
      dut.io.rounding #= rm
      dut.clockDomain.waitSampling()
      res = java.lang.Double.longBitsToDouble(dut.io.rsp.toLong)
    }
    res
  }

  test("FPADD") { assert(math.abs(run(FpOp.Arithmetic.FPADD, 1.5, 2.0) - 3.5) < 1e-9) }
  test("FPSUB") { assert(math.abs(run(FpOp.Arithmetic.FPSUB, 5.0, 2.5) - 2.5) < 1e-9) }
  test("FPMUL") { assert(math.abs(run(FpOp.Arithmetic.FPMUL, 2.0, 3.0) - 6.0) < 1e-9) }
  test("FPDIV") { assert(math.abs(run(FpOp.Arithmetic.FPDIV, 8.0, 2.0) - 4.0) < 1e-9) }

  test("rounding mode register") {
    SimConfig.compile(new FpuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val ctrl = dut.host[FpuControlService]
      dut.io.cmdValid #= true
      dut.io.op #= FpOp.Rounding.FPRP
      dut.io.a #= 0
      dut.io.b #= 0
      dut.io.rounding #= 2
      dut.clockDomain.waitSampling()
      assert(ctrl.roundingMode.toBigInt == 2)
    }
  }

  test("error flag handling") {
    SimConfig.compile(new FpuDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val ctrl = dut.host[FpuControlService]
      // Set error flags via FPSETERR then clear them
      dut.io.cmdValid #= true
      dut.io.op #= FpOp.Error.FPSETERR
      dut.io.a #= 0
      dut.io.b #= 0
      dut.io.rounding #= 0
      dut.clockDomain.waitSampling()
      assert(ctrl.getErrorFlags.toBigInt == 0x1f)
      // Clear
      dut.io.op #= FpOp.Error.FPCLRERR
      dut.clockDomain.waitSampling()
      assert(ctrl.getErrorFlags.toBigInt == 0)
      // Rounding mode unaffected
      assert(ctrl.roundingMode.toBigInt == 0)
    }
  }
}

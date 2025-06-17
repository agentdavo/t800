package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost
import spinal.lib.misc.database.Database
import transputer.plugins.fpu._

/** Execute FPU opcodes using their raw byte encoding. */
class FpuOpcodeDut extends Component {
  val io = new Bundle {
    val cmdValid = in Bool ()
    val opcode = in Bits (8 bits)
    val a = in Bits (64 bits)
    val b = in Bits (64 bits)
    val rounding = in Bits (2 bits)
    val rspValid = out Bool ()
    val rsp = out Bits (64 bits)
  }

  val host = new PluginHost
  val plugins = Transputer.unitPlugins() ++ Seq(new DummyTrapPlugin, new FpuPlugin)
  PluginHost(host).on(Database(Transputer.defaultDatabase()).on(Transputer(plugins)))

  val fpuService = host[FpuService]
  val ctrl = host[FpuControlService]

  when(io.cmdValid) {
    val op = FpOp.fromOpcode(io.opcode)
    fpuService.send(op, io.a.asUInt, io.b.asUInt)
    ctrl.setRoundingMode(io.rounding)
    ctrl.clearErrorFlags
  }
  io.rsp := fpuService.rsp.payload.asBits
  io.rspValid := fpuService.rsp.valid && io.cmdValid
}

class FpuOpcodeSpec extends AnyFunSuite {
  private def run(opc: Int, a: Double, b: Double = 0.0, rm: Int = 0): Double = {
    var res = 0.0
    SimConfig.compile(new FpuOpcodeDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.opcode #= opc
      dut.io.a #= BigInt(java.lang.Double.doubleToRawLongBits(a)) & BigInt("ffffffffffffffff", 16)
      dut.io.b #= BigInt(java.lang.Double.doubleToRawLongBits(b)) & BigInt("ffffffffffffffff", 16)
      dut.io.rounding #= rm
      dut.clockDomain.waitSampling()
      res = java.lang.Double.longBitsToDouble(dut.io.rsp.toLong)
    }
    res
  }

  test("FPLDNLSN loads 1.0") {
    assert(math.abs(run(0x8e, 1.0) - 1.0) < 1e-6)
  }

  test("FPDIV divides 4.0 / 2.0") {
    assert(math.abs(run(0x8c, 4.0, 2.0) - 2.0) < 1e-6)
  }

  test("FPRN sets rounding mode") {
    SimConfig.compile(new FpuOpcodeDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.opcode #= 0xd0
      dut.io.a #= 0
      dut.io.b #= 0
      dut.io.rounding #= 0
      dut.clockDomain.waitSampling()
      assert(dut.ctrl.roundingMode.toInt == 0)
    }
  }
}

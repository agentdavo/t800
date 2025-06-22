package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin, Plugin}
import spinal.lib.misc.database.Database
import transputer.plugins.fpu._
import transputer.plugins.pipeline.{PipelinePlugin, PipelineBuilderPlugin}
import transputer.plugins.transputer.TransputerPlugin
import transputer.plugins.registers.{RegFilePlugin, RegfileService, RegName, TransputerRegFileSpec}

/** Simple register file storing only Areg for comparison tests. */
class DummyRegFilePlugin extends FiberPlugin with RegfileService {
  val areg = Reg(Bits(64 bits)) init 0
  override def rfSpec = TransputerRegFileSpec()
  override def getPhysicalDepth = 1
  override def writeLatency = 0
  override def readLatency = 0
  override def read(reg: RegName.C, pid: UInt, shadow: Boolean): Bits =
    if (reg == RegName.Areg) areg else B(0, 64 bits)
  override def write(reg: RegName.C, data: Bits, pid: UInt, shadow: Boolean): Unit =
    if (reg == RegName.Areg) areg := data.resized
  override def readShadow(reg: RegName.C, pid: UInt): Bits = read(reg, pid, false)
  override def writeShadow(reg: RegName.C, data: Bits, pid: UInt): Unit =
    write(reg, data, pid, false)
  override def readStatusBit(f: StatusRegBits => Bool, pid: UInt, shadow: Boolean) = False
  override def writeStatusBit(f: StatusRegBits => Bool, v: Bool, pid: UInt, shadow: Boolean) {}
  override def copyToShadow(pid: UInt) {}
  override def restoreFromShadow(pid: UInt) {}
}

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
  val plugins = Seq(
    new TransputerPlugin,
    new DummyRegFilePlugin,
    new PipelinePlugin,
    new PipelineBuilderPlugin,
    new DummyTrapPlugin,
    new FpuPlugin
  )
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

  private def runSingle(opc: Int, a: Float): Double = {
    var res = 0.0
    SimConfig.compile(new FpuOpcodeDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.opcode #= opc
      dut.io.a #= BigInt(java.lang.Float.floatToRawIntBits(a)) & BigInt("ffffffff", 16)
      dut.io.b #= 0
      dut.io.rounding #= 0
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

  test("FPADD adds numbers") {
    assert(math.abs(run(0x87, 1.5, 2.0) - 3.5) < 1e-6)
  }

  test("FPMUL multiplies numbers") {
    assert(math.abs(run(0x8b, 2.0, 3.0) - 6.0) < 1e-6)
  }

  test("FPGT sets Areg on greater-than") {
    SimConfig.compile(new FpuOpcodeDut).doSim { dut =>
      val rf = dut.host[DummyRegFilePlugin]
      dut.clockDomain.forkStimulus(10)
      dut.io.cmdValid #= true
      dut.io.opcode #= 0x94
      dut.io.a #= BigInt(java.lang.Double.doubleToRawLongBits(2.0))
      dut.io.b #= BigInt(java.lang.Double.doubleToRawLongBits(1.0))
      dut.io.rounding #= 0
      dut.clockDomain.waitSampling()
      assert(rf.areg.toBigInt == 1)
    }
  }

  test("FPR32TOR64 converts single to double") {
    assert(math.abs(runSingle(0xd7, 1.5f) - 1.5) < 1e-6)
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

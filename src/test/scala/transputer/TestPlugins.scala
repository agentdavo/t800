package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.plugins._
import transputer.plugins.fpu._

// Minimal timer service used by DummyTimerPlugin
trait TimerService {
  def hi: UInt
  def lo: UInt
  def set(value: UInt): Unit
  def enableHi(): Unit
  def enableLo(): Unit
  def disableHi(): Unit
  def disableLo(): Unit
}

// Minimal stand-in for the MMU trap service
case class TrapHandlerService() extends Bundle {
  val trapAddr = Bits(Global.ADDR_BITS bits)
  val trapType = Bits(4 bits)
  val trapEnable = Bool()
  val trapHandlerAddr = Bits(Global.ADDR_BITS bits)
}

/** Minimal timer plugin exposing [[TimerService]] without any logic. */
class DummyTimerPlugin extends FiberPlugin {
  private var hiReg, loReg: UInt = null
  during setup new Area {
    hiReg = Reg(UInt(Global.WordBits bits)) init 0
    loReg = Reg(UInt(Global.WordBits bits)) init 0
    addService(new TimerService {
      override def hi: UInt = hiReg
      override def lo: UInt = loReg
      override def set(value: UInt): Unit = {
        hiReg := value
        loReg := value
      }
      override def enableHi(): Unit = {}
      override def enableLo(): Unit = {}
      override def disableHi(): Unit = hiReg := hiReg
      override def disableLo(): Unit = loReg := loReg
    })
  }

  // Provide an empty build phase so awaitBuild() doesn't block
  during build new Area {}
}

/** Minimal FPU plugin exposing [[FpuService]] without arithmetic. */
class DummyFpuPlugin extends FiberPlugin {
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null
  during setup new Area {
    pipeReg = Flow(FpCmd())
    pipeReg.setIdle()
    rspReg = Flow(UInt(Global.WordBits bits))
    rspReg.setIdle()
    addService(new FpuService {
      override def pipe: Flow[FpCmd] = pipeReg
      override def rsp: Flow[UInt] = rspReg
      override def send(op: FpOp.C, a: UInt, b: UInt): Unit = {
        pipeReg.valid := True
        pipeReg.payload.op := op
        pipeReg.payload.a := a.asBits
        pipeReg.payload.b := b.asBits
      }
      override def resultValid: Bool = rspReg.valid
      override def result: UInt = rspReg.payload
    })
  }

  // Empty build stage required for the fiber engine
  during build new Area {}
}

/** Minimal trap handler plugin exposing [[TrapHandlerService]]. */
class DummyTrapPlugin extends FiberPlugin {
  private var trap: TrapHandlerService = null
  during setup new Area {
    val addr = Reg(Bits(Global.ADDR_BITS bits)) init 0
    val typ = Reg(Bits(4 bits)) init 0
    val enable = Reg(Bool()) init False
    val handler = Reg(Bits(Global.ADDR_BITS bits)) init 0
    trap = TrapHandlerService()
    trap.trapAddr := addr
    trap.trapType := typ
    trap.trapEnable := enable
    trap.trapHandlerAddr := handler
    addService(trap)
  }
  during build new Area {}
}

/** Minimal plugin providing [[FpuControlService]] without any FPU logic. */
class DummyFpuControlPlugin extends FiberPlugin {
  private var rounding: Bits = null
  private var flags: Bits = null
  during setup new Area {
    rounding = Reg(Bits(2 bits)) init 0
    flags = Reg(Bits(5 bits)) init 0
    addService(new FpuControlService {
      override def specialValueDetected: Bool = False
      override def specialResult: Bits = B(0, 64 bits)
      override def trapEnable: Bool = False
      override def trapType: UInt = U(0, 4 bits)
      override def roundingMode: Bits = rounding
      override def setRoundingMode(mode: Bits): Unit = rounding := mode
      override def getErrorFlags: Bits = flags
      override def clearErrorFlags: Unit = flags := 0
      override def isFpuBusy(opcode: Bits): Bool = False
    })
  }
  during build new Area {}
}

/** Simple memory service exposing ROM/RAM access for unit tests. */
trait MemAccessService {
  def rom: Mem[UInt]
  def ram: Mem[UInt]
}

/** On-chip memory plugin providing small ROM/RAM blocks. Optionally connects the ROM to
  * [[InstrFetchService]] if present.
  */
class MemoryPlugin(romInit: Seq[BigInt] = Seq()) extends FiberPlugin {
  private var _rom: Mem[UInt] = null
  private var _ram: Mem[UInt] = null

  during setup new Area {
    _rom = Mem(UInt(Global.WordBits bits), Global.RomWords)
    for ((v, i) <- romInit.zipWithIndex if i < _rom.wordCount) {
      _rom(i) init v
    }
    _ram = Mem(UInt(Global.WordBits bits), Global.RamWords)
    addService(new MemAccessService {
      override def rom: Mem[UInt] = _rom
      override def ram: Mem[UInt] = _ram
    })
  }

  during build new Area {
    // If an instruction fetch service exists, serve it from the ROM
    val fetch = host.services
      .find(_.isInstanceOf[plugins.fetch.InstrFetchService])
      .map(_.asInstanceOf[plugins.fetch.InstrFetchService])

    fetch.foreach { srv =>
      val addrWidth = log2Up(_rom.wordCount)
      srv.cmd.ready := True
      val readAddr = srv.cmd.address.resized(addrWidth)
      val readNext = (srv.cmd.address + 1).resized(addrWidth)
      val dataLow = _rom.readSync(readAddr)
      val dataHigh = _rom.readSync(readNext)
      val rspData = RegNextWhen(Cat(dataHigh, dataLow), srv.cmd.valid) init 0
      srv.rsp.valid := RegNext(srv.cmd.valid) init False
      srv.rsp.payload := rspData
    }
  }
}

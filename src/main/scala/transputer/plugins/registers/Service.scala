package transputer.plugins.registers

import spinal.core._
import spinal.lib._
import transputer.Global

trait RegfileSpec {
  def width: Int
  def sizeArch: Int
  def initialValue: Bits
  def getName(): String
}

case class TransputerRegFileSpec() extends RegfileSpec {
  override def width = 64 // Max width for FP registers
  override def sizeArch = RegName.elements.size // ~35 registers
  override def initialValue = B(0, 64 bits)
  override def getName() = "TransputerRegFile"
}

object RegName extends SpinalEnum {
  val Areg, Breg, Creg, WdescReg, IptrReg, StatusReg, ThReg, FPstatusReg, FPAreg, FPBreg, FPCreg,
    BMreg0, BMreg1, BMreg2, WIReg, WuReg, Ereg, Xreg, EptrReg, RegionReg0, RegionReg1, RegionReg2,
    RegionReg3, PstateReg, WdescStubReg, FptrReg0, FptrReg1, BptrReg0, BptrReg1, ClockReg0,
    ClockReg1, TptrReg0, TptrReg1, TnextReg0, TnextReg1 = newElement()
}

case class StatusRegBits() extends Bundle {
  val reserved = Bits(32 bits)
  // Status/control bits (Table 5.5)
  val WtchPntPend = reserved(30) // Watchpoint trap pending flag
  val WtchPntEnbl = reserved(29) // Watchpoint trap enable bit
  val IsPprocessBit = reserved(27) // Protection bit (P-process mode)
  val StepBit = reserved(26) // Single-stepping trap enable bit
  val TimesliceDisabledBit = reserved(25) // Timeslice disable bit
  val UnalignTeBit = reserved(19) // Unaligned address trap enable bit
  val FPInexTeBit = reserved(17) // FP inexact result trap enable bit
  val FPInexFlag = reserved(16) // FP inexact result flag
  val FPUndTeBit = reserved(15) // FP underflow trap enable bit
  val FPUndFlag = reserved(14) // FP underflow flag
  val FPOvTeBit = reserved(13) // FP overflow trap enable bit
  val FPOvFlag = reserved(12) // FP overflow flag
  val FPDivByZeroTeBit = reserved(11) // FP divide by zero trap enable bit
  val FPDivByZeroFlag = reserved(10) // FP divide by zero flag
  val FPInOpTeBit = reserved(9) // FP invalid operation trap enable bit
  val FPInOpFlag = reserved(8) // FP invalid operation flag
  val IntOvTeBit = reserved(7) // Integer overflow trap enable bit
  val IntOvFlag = reserved(6) // Integer overflow flag
  val FPErrorTeBit = reserved(3) // FP error trap enable bit
  val FPErrorFlag = reserved(2) // FP error flag
}

case class RegFilePortParam(addressWidth: Int, dataWidth: Int, processIdWidth: Int)

case class RegFileRead(rfpp: RegFilePortParam, withReady: Boolean)
    extends Bundle
    with IMasterSlave {
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(rfpp.addressWidth bits)
  val processId = UInt(rfpp.processIdWidth bits)
  val shadow = Bool()
  val data = Bits(rfpp.dataWidth bits)
  override def asMaster() = {
    out(valid, address, processId, shadow)
    in(ready, data)
  }
}

case class RegFileWrite(rfpp: RegFilePortParam, withReady: Boolean)
    extends Bundle
    with IMasterSlave {
  val valid = Bool()
  val ready = withReady generate Bool()
  val address = UInt(rfpp.addressWidth bits)
  val processId = UInt(rfpp.processIdWidth bits)
  val shadow = Bool()
  val data = Bits(rfpp.dataWidth bits)
  override def asMaster() = {
    out(valid, address, processId, shadow, data)
    in(ready)
  }
}

trait RegfileService {
  def rfSpec: TransputerRegFileSpec
  def getPhysicalDepth: Int
  def writeLatency: Int
  def readLatency: Int
  def read(reg: RegName.C, processId: UInt, shadow: Boolean = false): Bits
  def write(reg: RegName.C, data: Bits, processId: UInt, shadow: Boolean = false): Unit
  def readShadow(reg: RegName.C, processId: UInt): Bits
  def writeShadow(reg: RegName.C, data: Bits, processId: UInt): Unit
  def readStatusBit(field: StatusRegBits => Bool, processId: UInt, shadow: Boolean = false): Bool
  def writeStatusBit(
    field: StatusRegBits => Bool,
    value: Bool,
    processId: UInt,
    shadow: Boolean = false
  ): Unit
  def copyToShadow(processId: UInt): Unit
  def restoreFromShadow(processId: UInt): Unit
}

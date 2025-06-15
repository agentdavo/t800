package t800.plugins.regfile

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import t800.{Global, PipelineSrv}

class RegFilePlugin extends FiberPlugin with RegfileService {
  val version = "RegFilePlugin v0.4"
  val spec = T9000RegFileSpec()
  val physicalDepth = RegName.elements.size // ~35 registers
  val processCount = 4 // Support 4 processes (high/low priority, P-process)
  val preferedWritePortForInit = "INIT_PORT"
  val syncRead = true
  val dualPortRam = true
  val latchBased = false
  val maskReadDuringWrite = true
  val addressWidth = log2Up(physicalDepth)
  val dataWidth = spec.width
  val processIdWidth = log2Up(processCount)
  val rfpp = RegFilePortParam(addressWidth, dataWidth, processIdWidth)

  override def writeLatency = 1
  override def readLatency = syncRead.toInt
  override def getPhysicalDepth = physicalDepth * processCount * 2 // Primary + shadow
  override def rfSpec = spec

  // Register address mapping
  private val regMap = RegName.elements.zipWithIndex.toMap

  case class WriteSpec(port: RegFileWrite, withReady: Boolean, sharingKey: Any, priority: Int)
  private val reads = ArrayBuffer[RegFileRead]()
  private val writes = ArrayBuffer[WriteSpec]()

  private def newRead(withReady: Boolean) = reads.addRet(RegFileRead(rfpp, withReady))
  private def newWrite(withReady: Boolean, sharingKey: Any = new{}, priority: Int = 0) = writes.addRet(
    WriteSpec(RegFileWrite(rfpp, withReady), withReady, sharingKey, priority)
  ).port

  override def getWrites() = {
    logic.await()
    writes.map(_.port)
  }

  override def read(reg: RegName.C, processId: UInt, shadow: Boolean): Bits = {
    val port = newRead(withReady = false)
    port.valid := True
    port.address := regMap(reg)
    port.processId := processId
    port.shadow := shadow
    port.data
  }

  override def write(reg: RegName.C, data: Bits, processId: UInt, shadow: Boolean): Unit = {
    val port = newWrite(withReady = true)
    port.valid := True
    port.address := regMap(reg)
    port.processId := processId
    port.shadow := shadow
    port.data := data
  }

  override def readShadow(reg: RegName.C, processId: UInt): Bits = read(reg, processId, shadow = true)
  override def writeShadow(reg: RegName.C, data: Bits, processId: UInt): Unit = write(reg, data, processId, shadow = true)

  override def readStatusBit(field: StatusRegBits => Bool, processId: UInt, shadow: Boolean): Bool = {
    val data = read(RegName.StatusReg, processId, shadow)
    val bits = data.as(StatusRegBits())
    field(bits)
  }

  override def writeStatusBit(field: StatusRegBits => Bool, value: Bool, processId: UInt, shadow: Boolean): Unit = {
    val readPort = newRead(withReady = false)
    readPort.valid := True
    readPort.address := regMap(RegName.StatusReg)
    readPort.processId := processId
    readPort.shadow := shadow
    val bits = readPort.data.as(StatusRegBits())
    val newBits = StatusRegBits()
    newBits := bits
    when(field(bits)) { newBits.reserved(field(bits).getBitOffset) := value }
    write(RegName.StatusReg, newBits.asBits, processId, shadow)
  }

  override def copyToShadow(processId: UInt): Unit = {
    logic.shadowCopy.valid := True
    logic.shadowCopy.processId := processId
  }

  override def restoreFromShadow(processId: UInt): Unit = {
    logic.shadowRestore.valid := True
    logic.shadowRestore.processId := processId
  }

  val logic = during build new Area {
    elaborationLock.await()

    // Register file: [processId][shadow][address]
    val ram = Mem.fill(processCount * 2 * physicalDepth)(Bits(dataWidth bits))
    Verilator.public(ram)

    // Write arbitration
    val writeGroups = writes.groupByLinked(_.sharingKey)
    val writeMerges = for ((key, elements) <- writeGroups) yield new Area {
      val bus = RegFileWrite(rfpp, false)
      bus.valid := elements.map(_.port.valid).orR
      when(bus.valid) {
        bus.address := OHMux.or(elements.map(_.port.valid), elements.map(_.port.address))
        bus.data := OHMux.or(elements.map(_.port.valid), elements.map(_.port.data))
        bus.processId := OHMux.or(elements.map(_.port.valid), elements.map(_.port.processId))
        bus.shadow := OHMux.or(elements.map(_.port.valid), elements.map(_.port.shadow))
        for (element <- elements if element.withReady) {
          element.port.ready := element.port.valid && !elements.filter(_ != element).map(_.port.valid).orR
        }
      }
    }

    // Write ports
    for (w <- writeMerges) yield new Area {
      val port = ram.writePort()
      port.valid := w.bus.valid
      port.address := w.bus.processId @@ w.bus.shadow.asUInt @@ w.bus.address
      port.data := w.bus.data
      // WdescReg: Ensure Reserved bit (bit 1) is 0
      when(w.bus.address === regMap(RegName.WdescReg)) {
        port.data(1) := False
      }
      // 32-bit registers padded to 64
      when(!w.bus.address.isOneOf(regMap(RegName.FPAreg), regMap(RegName.FPBreg), regMap(RegName.FPCreg))) {
        port.data := w.bus.data(31 downto 0) @@ B(0, 32 bits)
      }
    }

    // Read ports
    for (r <- reads) yield new Area {
      val port = ram.readSyncPort
      port.cmd.valid := r.valid
      port.cmd.payload := r.processId @@ r.shadow.asUInt @@ r.address
      r.data := port.rsp
      when(!r.address.isOneOf(regMap(RegName.FPAreg), regMap(RegName.FPBreg), regMap(RegName.FPCreg))) {
        r.data := port.rsp(31 downto 0) @@ B(0, 32 bits)
      }
    }

    // Shadow copy/restore
    val shadowCopy = Flow(new Bundle {
      val valid = Bool()
      val processId = UInt(processIdWidth bits)
    })
    val shadowRestore = Flow(new Bundle {
      val valid = Bool()
      val processId = UInt(processIdWidth bits)
    })
    val shadowCounter = Reg(UInt(addressWidth bits)) init 0
    val shadowBusy = Reg(Bool()) init False

    when(shadowCopy.valid && !shadowBusy) {
      shadowBusy := True
      shadowCounter := 0
    } elsewhen (shadowRestore.valid && !shadowBusy) {
      shadowBusy := True
      shadowCounter := 0
    }

    when(shadowBusy) {
      val srcAddr = (shadowCopy.valid ? shadowCounter | shadowCounter) @@ U"0" @@ shadowCounter
      val dstAddr = (shadowCopy.valid ? shadowCounter | shadowCounter) @@ U"1" @@ shadowCounter
      val port = ram.writePort()
      port.valid := True
      port.address := (shadowCopy.valid ? dstAddr | srcAddr)
      port.data := ram.readSync(srcAddr)
      shadowCounter := shadowCounter + 1
      when(shadowCounter === physicalDepth - 1) {
        shadowBusy := False
      }
    }

    // Initialization
    val initializer = new Area {
      val port = writeMerges(0).bus
      val counter = Reg(UInt(addressWidth + processIdWidth + 1 bits)) init 0
      val done = counter.msb
      when(!done) {
        port.valid := True
        port.processId := counter(processIdWidth + addressWidth - 1 downto addressWidth)
        port.shadow := counter(addressWidth)
        port.address := counter(addressWidth - 1 downto 0)
        port.data := (counter(addressWidth - 1 downto 0) === regMap(RegName.IptrReg)).mux(
          U(Global.RESET_IPTR, 32 bits) @@ B(0, 32 bits), spec.initialValue
        )
        counter := counter + 1
      }
    }
  }
}

package transputer

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.pipeline._
import spinal.lib._

/** Global configuration elements and configuration register framework for Transputer, accessed via
  * [[Database]]. Default values are defined below for convenience, aligned with IMS T9000
  * specifications.
  */
object Global extends AreaObject {
  // Database handles for configuration
  val WORD_BITS = Database.blocking[Int]
  val ADDR_BITS = Database.blocking[Int]
  val PC_BITS = Database.blocking[Int]
  val INSTR_BITS = Database.blocking[Int]
  val IPTR_BITS = Database.blocking[Int]
  val OPCODE_BITS = Database.blocking[Int]
  val ROM_WORDS = Database.blocking[Int]
  val RAM_WORDS = Database.blocking[Int]
  val LINK_COUNT = Database.blocking[Int]
  val FPU_PRECISION = Database.blocking[Int]
  val SCHED_QUEUE_DEPTH = Database.blocking[Int]
  val RESET_IPTR = Database.blocking[Long]

  // Default constants formerly hosted in `TConsts`, aligned with T9000
  val WordBits = 32
  val AddrBitsValue = 32 // Renamed to avoid conflict with getter
  val RomWords = 16
  val RamWords = 4096
  val MicroWords = 1024
  val LinkCount = 4
  val FpuPrecision = WordBits
  val SchedQueueDepth = LinkCount
  val ResetIptr = 0x00000000L

  // Dynamic getter for systemBusParam and plugins
  // In the current minimal build the database may not contain ADDR_BITS,
  // so fall back to the constant directly.
  def AddrBits: Int = AddrBitsValue

  // Memory layout and interrupt vectors
  val InternalMemStart = 0x80000000L
  val InternalMemEnd = 0x80000fffL
  val ExternalMemStart = 0x80001000L
  val MemStart = 0x80000070L

  val MaxINT = 0x7fffffffL
  val ResetCode = 0x7ffffffeL
  val EregIntSaveLoc = 0x80000044L
  val StatusIntSaveLoc = 0x80000040L
  val CregIntSaveLoc = 0x8000003cL
  val BregIntSaveLoc = 0x80000038L
  val AregIntSaveLoc = 0x80000034L
  val IptrIntSaveLoc = 0x80000030L
  val WdescIntSaveLoc = 0x8000002cL
  val TPtrLoc1 = 0x80000028L
  val TPtrLoc0 = 0x80000024L
  val EventLoc = 0x80000020L
  val Link3Input = 0x8000001cL
  val Link2Input = 0x80000018L
  val Link1Input = 0x80000014L
  val Link0Input = 0x80000010L
  val Link3Output = 0x8000000cL
  val Link2Output = 0x80000008L
  val Link1Output = 0x80000004L
  val Link0Output = 0x80000000L

  // Pipeline payloads (using defaults to avoid database dependency)
  def IPTR: Payload[UInt] = Payload(UInt(AddrBitsValue bits))
  def OPCODE: Payload[Bits] = Payload(Bits(8 bits))
  def MEM_ADDR: Payload[UInt] = Payload(UInt(AddrBitsValue bits))
  def MEM_DATA: Payload[Bits] = Payload(Bits(WordBits bits))
  
  // T9000 pipeline payloads
  def OPERAND: Payload[Bits] = Payload(Bits(WordBits bits))
  def GROUPED_INSTR: Payload[Bits] = Payload(Bits(64 bits))  // Up to 2 instructions
  def AREG_VALUE: Payload[UInt] = Payload(UInt(WordBits bits))
  def BREG_VALUE: Payload[UInt] = Payload(UInt(WordBits bits))
  def CREG_VALUE: Payload[UInt] = Payload(UInt(WordBits bits))
  def MEM_WRITE: Payload[Bool] = Payload(Bool())
  def ALU_RESULT: Payload[UInt] = Payload(UInt(WordBits bits))
  def FPU_RESULT: Payload[Bits] = Payload(Bits(64 bits))  // Double precision
  def STACK_OP: Payload[Bits] = Payload(Bits(3 bits))  // Stack operation type
  def BRANCH_TARGET: Payload[UInt] = Payload(UInt(AddrBitsValue bits))
  def BRANCH_TAKEN: Payload[Bool] = Payload(Bool())
  def TRAP_CAUSE: Payload[Bits] = Payload(Bits(8 bits))
  def TRAP_ENABLE: Payload[Bool] = Payload(Bool())

  // Memory command definitions, aligned with T9000
  case class MemRead[T <: Data](
    payloadType: HardType[T] = HardType(Bits(WordBits bits)),
    depth: Int = 1 << AddrBitsValue
  ) extends Bundle
      with IMasterSlave {
    val cmd = Flow(UInt(log2Up(depth) bits))
    val rsp = payloadType()

    override def asMaster() = {
      master(cmd)
      in(rsp)
    }
  }

  // Configuration register addresses (16-bit, MSB for subsystem, LSB for register) [cite: t9000hrm.pdf, 225-226]
  object ConfigAddr {
    val CPU = 0x01
    val PMI_BANK = 0x02
    val PMI_STROBE = 0x04
    val VCP = 0x08
    val SYSTEM_SERVICES = 0x10
    val CACHE = 0x20
    val SCHEDULER = 0x40
    val LINK0 = 0x80
    val LINK1 = 0x81
    val LINK2 = 0x82
    val LINK3 = 0x83
    val CLINK0 = 0xfd
    val CLINK1 = 0xfe

    // Subsystem set addresses
    val PMI_BANK_STROBE = 0x06
    val CPU_VCP = 0x09
    val CPU_SCHEDULER = 0x41
    val CPU_VCP_SCHEDULER = 0x49

    // CPU configuration registers [cite: t9000hrm.pdf, 227]
    val CHAN_WRITE_LOCK = 0x4900
    val HDR_AREA_BASE = 0x0901
    val MEM_START = 0x410e
    val MIN_INVALID_CHANNEL = 0x010f
    val EXTERNAL_RC_BASE = 0x4110
    val INITIAL_IPTR = 0x0111
    val INITIAL_WPTR = 0x0112
    val REASON = 0x0113
    val EMI_BAD_ADDRESS = 0x0115

    // PMI bank address configuration registers [cite: t9000hrm.pdf, 228]
    val PMI_WRITE_LOCK = 0x0600
    val ADDRESS0 = 0x0202
    val MASK0 = 0x0203
    val FORMAT_CONTROL0 = 0x0204
    val ADDRESS1 = 0x0205
    val MASK1 = 0x0206
    val FORMAT_CONTROL1 = 0x0207
    val ADDRESS2 = 0x0208
    val MASK2 = 0x0209
    val FORMAT_CONTROL2 = 0x020a
    val ADDRESS3 = 0x020b
    val MASK3 = 0x020c
    val FORMAT_CONTROL3 = 0x020d
    val RAS_BITS0 = 0x020e
    val RAS_BITS1 = 0x020f
    val RAS_BITS2 = 0x0210
    val RAS_BITS3 = 0x0211
    val DO_PMI_CONFIGURED = 0x0212
    val ERROR_ADDRESS = 0x0213

    // PMI strobe timing configuration registers [cite: t9000hrm.pdf, 228-229]
    val RAS_STROBE0 = 0x0401
    val RAS_STROBE1 = 0x0402
    val RAS_STROBE2 = 0x0403
    val RAS_STROBE3 = 0x0404
    val CAS_STROBE0 = 0x0405
    val CAS_STROBE1 = 0x0406
    val CAS_STROBE2 = 0x0407
    val CAS_STROBE3 = 0x0408
    val PROG_STROBE0 = 0x0409
    val PROG_STROBE1 = 0x040a
    val PROG_STROBE2 = 0x040b
    val PROG_STROBE3 = 0x040c
    val WRITE_STROBE0 = 0x040d
    val WRITE_STROBE1 = 0x040e
    val WRITE_STROBE2 = 0x040f
    val WRITE_STROBE3 = 0x0410
    val TIMING_CONTROL0 = 0x0411
    val TIMING_CONTROL1 = 0x0412
    val TIMING_CONTROL2 = 0x0413
    val TIMING_CONTROL3 = 0x0414
    val REFRESH_CONTROL = 0x0415
    val REMAP_BOOT_BANK = 0x0416

    // VCP configuration registers [cite: t9000hrm.pdf, 229]
    val VCP_STATUS = 0x0802
    val VCP_COMMAND = 0x0804
    val VCP_LINK0_MODE = 0x0805
    val VCP_LINK0_MAX_HEADER = 0x0806
    val VCP_LINK0_MIN_HEADER = 0x0807
    val VCP_LINK0_HDR_OFFSET = 0x0808
    val VCP_LINK1_MODE = 0x0809
    val VCP_LINK1_MAX_HEADER = 0x080a
    val VCP_LINK1_MIN_HEADER = 0x080b
    val VCP_LINK1_HDR_OFFSET = 0x080c
    val VCP_LINK2_MODE = 0x080d
    val VCP_LINK2_MAX_HEADER = 0x080e
    val VCP_LINK2_MIN_HEADER = 0x080f
    val VCP_LINK2_HDR_OFFSET = 0x0810
    val VCP_LINK3_MODE = 0x0811
    val VCP_LINK3_MAX_HEADER = 0x0812
    val VCP_LINK3_MIN_HEADER = 0x0813
    val VCP_LINK3_HDR_OFFSET = 0x0814

    // System services configuration registers [cite: t9000hrm.pdf, 230]
    val DEVICE_ID = 0x1001
    val DEVICE_REVISION = 0x1002
    val MODE_STATUS = 0x1003
    val ERROR_CODE = 0x1004
    val DS_LINK_PLL = 0x1005
    val SYS_SERV_WRITE_LOCK = 0x1006

    // Cache configuration registers [cite: t9000hrm.pdf, 230]
    val RAM_SIZE = 0x2001
    val DO_RAM_SIZE = 0x2002
    val RAM_LINE_NUMBER = 0x2003
    val RAM_ADDRESS = 0x2004
    val DO_ALLOCATE = 0x2005

    // Scheduler configuration registers [cite: t9000hrm.pdf, 230]
    val SCHEDULER_CHAN_WRITE_LOCK = 0x4900 // Shared with CPU, VCP

    // Link configuration registers [cite: t9000hrm.pdf, 230-231]
    val LINK0_MODE = 0x8001
    val LINK0_COMMAND = 0x8002
    val LINK0_STATUS = 0x8003
    val LINK0_WRITE_LOCK = 0x8004
    val LINK1_MODE = 0x8101
    val LINK1_COMMAND = 0x8102
    val LINK1_STATUS = 0x8103
    val LINK1_WRITE_LOCK = 0x8104
    val LINK2_MODE = 0x8201
    val LINK2_COMMAND = 0x8202
    val LINK2_STATUS = 0x8203
    val LINK2_WRITE_LOCK = 0x8204
    val LINK3_MODE = 0x8301
    val LINK3_COMMAND = 0x8302
    val LINK3_STATUS = 0x8303
    val LINK3_WRITE_LOCK = 0x8304

    // Control link configuration registers [cite: t9000hrm.pdf, 231]
    val CLINK0_MODE = 0xfd01
    val CLINK0_COMMAND = 0xfd02
    val CLINK0_STATUS = 0xfd03
    val CLINK0_WRITE_LOCK = 0xfd04
    val CLINK1_MODE = 0xfe01
    val CLINK1_COMMAND = 0xfe02
    val CLINK1_STATUS = 0xfe03
    val CLINK1_WRITE_LOCK = 0xfe04
  }

  // Write-lock registers with dynamic state management [cite: t9000hrm.pdf, 227]
  object WriteLock {
    val PMI_WRITE_LOCK = 0x0600 // Shared between PMI bank and strobe
    val CHAN_WRITE_LOCK = 0x4900 // Shared between CPU, VCP, Scheduler
    // Track write locks for PMI and channel registers
    val writeLocks = Reg(Vec(Bool(), 2))
    def isLocked(addr: UInt): Bool = {
      val isPmiLock = addr === PMI_WRITE_LOCK && writeLocks(0)
      val isChanLock = addr === CHAN_WRITE_LOCK && writeLocks(1)
      isPmiLock || isChanLock
    }
    def setLock(lockType: Int, state: Bool): Unit = writeLocks(lockType) := state
  }

  // Configuration register access service for Ideonf/steonf [cite: t9000hrm.pdf, 225]
  case class ConfigAccessService() extends Bundle {
    val addr = UInt(16 bits) // 16-bit configuration address
    val data = Bits(32 bits) // 32-bit register data
    val writeEnable = Bool() // Write operation flag
    val isValid = Bool() // Valid access flag
    val significantBits = UInt(5 bits) // Number of significant bits (0-32)
    // Simplified access helpers used in tests. Real register semantics will
    // be implemented by the individual plugins.
    def read(addr: UInt, sigBits: Int): Bits = {
      this.addr := addr
      this.writeEnable := False
      this.significantBits := sigBits
      isValid := !WriteLock.isLocked(addr)
      data
    }
    def write(addr: UInt, data: Bits, sigBits: Int): Unit = {
      this.addr := addr
      this.data := data
      this.writeEnable := True
      this.significantBits := sigBits
      isValid := !WriteLock.isLocked(addr)
    }
  }
}

// Minimal memory command bundles used across services
case class MemWriteCmd(depth: Int = 1 << Global.AddrBitsValue) extends Bundle {
  val address = UInt(log2Up(depth) bits)
  val data = Bits(Global.WordBits bits)
}

case class MemReadCmd(depth: Int = 1 << Global.AddrBitsValue) extends Bundle {
  val address = UInt(log2Up(depth) bits)
}

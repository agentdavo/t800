package t800

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.pipeline._

/** Global configuration elements accessed via [[Database]]. Default values are defined below for
  * convenience.
  */
object Global extends AreaObject {
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

  // Default constants formerly hosted in `TConsts`
  val WordBits = 32
  val AddrBits = 32
  val RomWords = 16
  val RamWords = 4096
  val MicroWords = 1024
  val LinkCount = 4
  val FpuPrecision = WordBits
  val SchedQueueDepth = LinkCount
  val ResetIPtr = 0x00000000L

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

  def IPTR: Payload[UInt] = Payload(UInt(IPTR_BITS bits))
  def OPCODE: Payload[Bits] = Payload(Bits(OPCODE_BITS bits))
  def MEM_ADDR: Payload[UInt] = Payload(UInt(ADDR_BITS bits))
  def MEM_DATA: Payload[Bits] = Payload(Bits(WORD_BITS bits))
}

/** Generic memory port command definitions formerly located in MemPort.scala. */
case class MemReadCmd() extends Bundle {
  val addr = UInt(Global.ADDR_BITS bits)
}

case class MemWriteCmd() extends Bundle {
  val addr = UInt(Global.ADDR_BITS bits)
  val data = Bits(Global.WORD_BITS bits)
}

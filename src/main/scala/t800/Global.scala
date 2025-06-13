package t800

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.pipeline._

/** Global configuration elements accessed via [[Database]]. Plugins should use these handles rather
  * than constants in [[TConsts]].
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

  def IPTR: Payload[UInt] = Payload(UInt(IPTR_BITS bits))
  def OPCODE: Payload[Bits] = Payload(Bits(OPCODE_BITS bits))
  def MEM_ADDR: Payload[UInt] = Payload(UInt(ADDR_BITS bits))
  def MEM_DATA: Payload[Bits] = Payload(Bits(WORD_BITS bits))
}

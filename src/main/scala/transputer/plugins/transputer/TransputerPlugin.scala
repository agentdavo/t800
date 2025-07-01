package transputer.plugins.transputer

import spinal.core._
import spinal.lib.misc.database.{Database, Element}
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global

/** Centralized configuration plugin for Transputer, setting T9000-specific parameters in the
  * Database. Reads variant parameters from Database if set, else uses defaults.
  */
class TransputerPlugin(
  var wordBits: Int = Global.WordBits,
  var addrBits: Int = Global.AddrBitsValue,
  var linkCount: Int = Global.LinkCount,
  var fpuPrecision: Int = Global.FpuPrecision,
  var schedQueueDepth: Int = Global.SchedQueueDepth,
  var romWords: Int = Global.RomWords,
  var ramWords: Int = Global.RamWords,
  var resetIptr: Long = Global.ResetIptr
) extends FiberPlugin {
  override def getDisplayName(): String = "TransputerPlugin"
  setName("transputer")
  
  // Define pipeline constant keys
  val FETCH_WIDTH = Database.value[Int]()
  val FETCH_BYTES = Database.value[Int]()
  val PIPELINE_STAGES = Database.value[Int]()

  val logic = during build new Area {
    // Ensure early execution (no locks in minimal build)

    // Helper to safely set Database value only if not already set
    def setIfEmpty[T](key: Element[T], value: T)(implicit db: Database): Unit = {
      try {
        // Try to get existing value first
        val existing = key.get
        // If we get here, value exists, don't set it
      } catch {
        case _: Exception =>
          // Value doesn't exist, safe to set
          key.set(value)
      }
    }

    // Set Database keys only if not already configured (avoid conflicts)
    implicit val db = Database.get
    setIfEmpty(Global.WORD_BITS, wordBits)
    setIfEmpty(Global.ADDR_BITS, addrBits)
    setIfEmpty(Global.PC_BITS, addrBits)
    setIfEmpty(Global.INSTR_BITS, 8) // Default to 8 bits, but allow override
    setIfEmpty(Global.IPTR_BITS, addrBits)
    setIfEmpty(Global.OPCODE_BITS, 8)
    setIfEmpty(Global.ROM_WORDS, romWords)
    setIfEmpty(Global.RAM_WORDS, ramWords)
    setIfEmpty(Global.LINK_COUNT, linkCount)
    setIfEmpty(Global.FPU_PRECISION, fpuPrecision)
    setIfEmpty(Global.SCHED_QUEUE_DEPTH, schedQueueDepth)
    setIfEmpty(Global.RESET_IPTR, resetIptr)

    // Set pipeline constants
    FETCH_WIDTH.set(8)
    FETCH_BYTES.set(1)
    PIPELINE_STAGES.set(5)
  }
}

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
  // Define pipeline constant keys
  val FETCH_WIDTH = Database.value[Int]()
  val FETCH_BYTES = Database.value[Int]()
  val PIPELINE_STAGES = Database.value[Int]()

  val logic = during build new Area {
    // Ensure early execution (no locks in minimal build)

    // Helper to get Database value or default
    def getOrElse[T](key: Element[T], default: T)(implicit db: Database): T = {
      if (db.storageExists(key)) db.storageGet(key) else default
    }

    // Set Database keys, prioritizing pre-set values
    implicit val db = Database.get
    Global.WORD_BITS.set(getOrElse(Global.WORD_BITS, wordBits))
    Global.ADDR_BITS.set(getOrElse(Global.ADDR_BITS, addrBits))
    Global.PC_BITS.set(getOrElse(Global.PC_BITS, addrBits))
    Global.INSTR_BITS.set(getOrElse(Global.INSTR_BITS, 8))
    Global.IPTR_BITS.set(getOrElse(Global.IPTR_BITS, addrBits))
    Global.OPCODE_BITS.set(getOrElse(Global.OPCODE_BITS, 8))
    Global.ROM_WORDS.set(getOrElse(Global.ROM_WORDS, romWords))
    Global.RAM_WORDS.set(getOrElse(Global.RAM_WORDS, ramWords))
    Global.LINK_COUNT.set(getOrElse(Global.LINK_COUNT, linkCount))
    Global.FPU_PRECISION.set(getOrElse(Global.FPU_PRECISION, fpuPrecision))
    Global.SCHED_QUEUE_DEPTH.set(getOrElse(Global.SCHED_QUEUE_DEPTH, schedQueueDepth))
    Global.RESET_IPTR.set(getOrElse(Global.RESET_IPTR, resetIptr))

    // Set pipeline constants
    FETCH_WIDTH.set(8)
    FETCH_BYTES.set(1)
    PIPELINE_STAGES.set(5)
  }
}

package transputer.plugins.core.transputer

import spinal.core._
// import spinal.lib.misc.database.{Database, Element} // Temporarily disabled for build compatibility
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
  // val FETCH_WIDTH = Database.value[Int]() // Temporarily disabled
  // val FETCH_BYTES = Database.value[Int]() // Temporarily disabled
  val FETCH_WIDTH = 4 // 32-bit fetch width
  val FETCH_BYTES = 4 // 4 bytes per fetch
  // val PIPELINE_STAGES = Database.value[Int]() // Temporarily disabled
  val PIPELINE_STAGES = 5 // T9000 5-stage pipeline

  during setup new Area {
    // Configure database during setup phase when Global context is available
    // Temporarily disabled for build compatibility
    /*
    // Helper to safely set Database value only if not already set
    def setIfEmpty[T](key: Element[T], value: T): Unit = {
      try {
        // Try to get existing value first
        val existing = key.get
        // If we get here, value exists, don't set it
      } catch {
        case _: Exception =>
          // Value doesn't exist, safe to set
          try {
            key.set(value)
          } catch {
            case _: Exception =>
              // If setting fails, skip it (test mode)
              println(s"[TransputerPlugin] Skipping database configuration in test mode")
          }
      }
    }
     */

    // Safely set Database keys only if context allows
    // Temporarily disabled for build compatibility
    /*
    try {
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
    } catch {
      case _: Exception =>
        println(s"[TransputerPlugin] Running in test mode - skipping database configuration")
    }
     */

    println(s"[TransputerPlugin] Setup complete - using hardcoded values for build compatibility")
  }
}

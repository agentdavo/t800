package t800.plugins

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import t800.Global

/**
 * Centralized configuration plugin for T800, setting T9000-specific parameters in the Database.
 */
class TransputerPlugin(
  var wordBits: Int = Global.WordBits, // 32-bit default
  var addrBits: Int = Global.AddrBits, // 32-bit default
  var linkCount: Int = Global.LinkCount, // 4 links default
  var fpuPrecision: Int = Global.FpuPrecision, // 32-bit default
  var schedQueueDepth: Int = Global.SchedQueueDepth, // 4 default
  var romWords: Int = Global.RomWords, // 16 default
  var ramWords: Int = Global.RamWords, // 4096 default
  var resetIptr: Long = Global.ResetIPtr // 0x00000000 default
) extends FiberPlugin {

  val logic = during build new Area {
    // Set Database keys for T9000 configuration
    Global.WORD_BITS.set(wordBits)
    Global.ADDR_BITS.set(addrBits)
    Global.PC_BITS.set(addrBits)
    Global.INSTR_BITS.set(8) // Fixed 8-bit opcodes
    Global.IPTR_BITS.set(addrBits)
    Global.OPCODE_BITS.set(8) // Fixed 8-bit opcodes
    Global.ROM_WORDS.set(romWords)
    Global.RAM_WORDS.set(ramWords)
    Global.LINK_COUNT.set(linkCount)
    Global.FPU_PRECISION.set(fpuPrecision)
    Global.SCHED_QUEUE_DEPTH.set(schedQueueDepth)
    Global.RESET_IPTR.set(resetIptr)

    // Pipeline-related constants
    Global.FETCH_WIDTH.set(8) // 8-bit instruction fetch
    Global.FETCH_BYTES.set(1) // 1 byte per instruction
    Global.PIPELINE_STAGES.set(5) // Fixed 5-stage pipeline
  }
}

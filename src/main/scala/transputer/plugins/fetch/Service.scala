package transputer.plugins.fetch

import spinal.core._
import spinal.lib._
import transputer.{Global, MemReadCmd}

/** Service interface for instruction fetch, providing instruction stream to GrouperPlugin. This
  * interface follows the T9000-inspired pipeline: systemBus -> FetchPlugin -> GrouperPlugin
  */
trait InstrFetchService {
  def cmd: Stream[MemReadCmd] // Legacy BMB-based read command (for compatibility)
  def rsp: Stream[Bits] // Response with opcodes (8-bit opcodes for T9000 compatibility)

  /** Direct instruction stream for modern plugin-to-plugin communication */
  def instrStream: Stream[Bits] = rsp
}

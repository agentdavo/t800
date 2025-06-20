package transputer.plugins.fetch

import spinal.core._
import spinal.lib._
import transputer.Global

/** Service interface for instruction fetch, integrated with MainCachePlugin. */
trait InstrFetchService {
  def cmd: Flow[Global.MemReadCmd] // BMB-based read command
  def rsp: Flow[Bits] // Response with opcodes
}

package t800.plugins.fetch

import spinal.core._
import spinal.lib._
import t800.Global

/** Service interface for instruction fetch, integrated with MainCachePlugin. */
trait InstrFetchSrv {
  def cmd: Flow[Global.MemReadCmd] // BMB-based read command
  def rsp: Flow[Bits] // Response with opcodes
}

package t800.plugins.fetch

import spinal.core._
import spinal.lib._

/** Service for instruction fetch, integrated with MainCachePlugin. */
trait InstrFetchSrv {
  def cmd: Flow[t800.MemReadCmd]  // BMB-based read command
  def rsp: Flow[Bits]             // Response from MainCachePlugin
}

package transputer.plugins.cache

import spinal.core._
import spinal.lib._

/** Cache command structure for T9000 cache operations */
case class CacheCmd() extends Bundle {
  val address = UInt(32 bits)
  val writeData = Bits(32 bits)
  val write = Bool()
  val byteEnable = Bits(4 bits)
}

/** Service interface for T9000 Main Cache */
trait MainCacheService {
  def cpuLoadA: Flow[CacheCmd] // CPU Load A port
  def cpuLoadB: Flow[CacheCmd] // CPU Load B port
  def cpuStore: Flow[CacheCmd] // CPU Store port
  def instrFetch: Flow[CacheCmd] // Instruction fetch port
  def hit: Bool // Cache hit signal
  def miss: Bool // Cache miss signal
}

/** Service interface for T9000 Workspace Cache */
trait WorkspaceCacheService {
  def readA(addr: UInt): Bits // Port A read access
  def readB(addr: UInt): Bits // Port B read access
  def write(addr: UInt, data: Bits): Unit // Port C write access
  def invalidate(): Unit // Invalidate entire cache
  def writePending: Bool // Write operation pending
}

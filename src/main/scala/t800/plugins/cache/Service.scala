package t800.plugins.cache

import spinal.core._

case class WorkspaceCacheAccessSrv() extends Bundle {
  val addrA = Bits(32 bits)
  val addrB = Bits(32 bits)
  val dataOutA = Bits(32 bits)
  val dataOutB = Bits(32 bits)
  val writeAddr = Bits(32 bits)
  val writeData = Bits(32 bits)
  val writeEnable = Bool()
  val isHitA = Bool()
  val isHitB = Bool()
}

case class MainCacheAccessSrv() extends Bundle {
  val addr = Bits(32 bits)
  val dataOut = Bits(128 bits)
  val isHit = Bool()
  val writeEnable = Bool()
  val writeData = Bits(128 bits)
}
trait MainCacheSrv { def read(addr: Bits): Bits; def write(addr: Bits, data: Bits): Unit }
trait WorkspaceCacheSrv { def write(addr: Bits, data: Bits): Unit; def fetch(addr: Bits): Bits }

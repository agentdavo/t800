package transputer.plugins.cache

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.Bmb
import transputer.Global

case class CacheReq() extends Bundle {
  val address = UInt(Global.ADDR_BITS bits)
  val data = Bits(Global.WORD_BITS bits)
  val isWrite = Bool()
}

case class CacheRsp() extends Bundle {
  val data = Bits(Global.WORD_BITS bits)
  val valid = Bool()
}

case class WorkspaceCacheAccessService() extends Bundle {
  val addrA = Bits(Global.ADDR_BITS bits)
  val addrB = Bits(Global.ADDR_BITS bits)
  val writeAddr = Bits(Global.ADDR_BITS bits)
  val writeData = Bits(Global.WORD_BITS bits)
  val writeEnable = Bool()
  val isHitA = Bool()
  val isHitB = Bool()
  val dataOutA = Bits(Global.WORD_BITS bits)
  val dataOutB = Bits(Global.WORD_BITS bits)
}

case class MainCacheAccessService() extends Bundle {
  val addr = Bits(Global.ADDR_BITS bits)
  val dataOut = Bits(128 bits) // 128-bit cache line
  val isHit = Bool()
  val writeEnable = Bool()
  val writeData = Bits(128 bits)
}

trait CacheAccessService {
  def req: Flow[CacheReq]
  def rsp: Flow[CacheRsp]
}

trait MainCacheService {
  def read(addr: UInt): Bits
  def write(addr: UInt, data: Bits): Unit
}

trait WorkspaceCacheService {
  def read(addr: UInt): Bits
  def write(addr: UInt, data: Bits): Unit
  def bus: Bmb // BMB interface for workspace cache
}

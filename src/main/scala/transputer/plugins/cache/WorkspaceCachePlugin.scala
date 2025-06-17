package transputer.plugins.cache

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.database._
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbOnChipRamMultiPort}
import transputer.plugins.{AddressTranslationSrv, MainCacheSrv}
import transputer.plugins.cache.CacheAccessSrv
import spinal.lib.bus.misc.SingleMapping

// The WorkspaceCachePlugin implements a 32-word triple-ported cache (two reads, one write per cycle)
// Providing zero-cycle read access in the Decode stage.
// It is write-through to MainCachePlugin, organized as a circular buffer addressed by Wptr[4:0]
// With invalidation on context switches/interrupts and roll-over, supporting two simultaneous CPU reads

class WorkspaceCachePlugin extends FiberPlugin {
  val version = "WorkspaceCachePlugin v1.0"
  report(L"Initializing $version")

  object DBKeys {
    val WS_CACHE_ADDR_A = Database.blocking[Bits]()
    val WS_CACHE_ADDR_B = Database.blocking[Bits]()
    val WS_CACHE_DATA_A = Database.blocking[Bits]()
    val WS_CACHE_DATA_B = Database.blocking[Bits]()
  }

  lazy val WS_CACHE_ADDR_A = Payload(Bits(32 bits))
  lazy val WS_CACHE_ADDR_B = Payload(Bits(32 bits))
  lazy val WS_CACHE_DATA_A = Payload(Bits(32 bits))
  lazy val WS_CACHE_DATA_B = Payload(Bits(32 bits))

  lazy val srv = during setup new Area {
    val service = WorkspaceCacheAccessSrv()
    addService(service)
    val cacheIf = new CacheAccessSrv {
      override val req = Flow(CacheReq())
      override val rsp = Flow(CacheRsp())
    }
    cacheIf.req.setIdle()
    cacheIf.rsp.setIdle()
    addService(cacheIf)
  }

  buildBefore(retains(host[MainCachePlugin].lock).lock)

  lazy val logic = during build new Area {
    val decode = host.find[StageCtrlPipeline].ctrl(2)

    // BMB bus parameters (32-bit data, 32-bit address)
    val bmbParameter = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = 32,
        dataWidth = 32,
        lengthWidth = 2, // Single-beat transactions
        sourceWidth = 0,
        contextWidth = 0
      ),
      sourceCount = 1
    )

    // Triple-ported RAM for 32 words
    val ram = BmbOnChipRamMultiPort(
      portsParameter = Seq.fill(3)(bmbParameter), // Two read ports, one write port
      size = 128 // 32 words * 4 bytes
    )

    // Workspace pointer and invalidation logic
    val wptr = Reg(Bits(32 bits)) init (0)
    val validBits = Reg(Bits(32 bits)) init (0)
    val contextSwitch = False // Triggered by SchedulerPlugin
    val interrupt = False // Triggered by SystemControlPlugin
    when(contextSwitch || interrupt) {
      validBits := 0
    }

    // Circular buffer logic
    val wptrDelta = SInt(6 bits)
    when(decode.isValid && decode(WPTR_UPDATE)) {
      wptr := (wptr.asUInt + wptrDelta).asBits
      when(wptrDelta < 0) { // Procedure call (move down)
        for (i <- 0 until 32) {
          when((wptr(4 downto 0).asUInt + i) >= (wptr(4 downto 0).asUInt + wptrDelta.asUInt)) {
            validBits(i) := False
          }
        }
      } elsewhen (wptrDelta > 0) { // Procedure return (move up)
        for (i <- 0 until 32) {
          when((wptr(4 downto 0).asUInt + i) < (wptr(4 downto 0).asUInt + wptrDelta.asUInt)) {
            validBits(i) := False
          }
        }
      }
    }

    // Read port A
    ram.io.buses(0).cmd.valid := decode.isValid
    ram.io.buses(0).cmd.opcode := 0 // Read
    ram.io.buses(0).cmd.address := Plugin[AddressTranslationSrv].translate(srv.addrA)
    ram.io.buses(0).cmd.length := 0
    ram.io.buses(0).cmd.data := 0
    ram.io.buses(0).rsp.ready := True
    srv.isHitA := validBits(srv.addrA(4 downto 0).asUInt)
    srv.dataOutA := ram.io.buses(0).rsp.data
    when(!srv.isHitA) {
      decode.haltWhen(True)
      // Fetch from Main Cache
      val mainCacheData =
        Plugin[MainCacheSrv].read(Plugin[AddressTranslationSrv].translate(srv.addrA))
      ram.io.buses(0).cmd.opcode := 1 // Write
      ram.io.buses(0).cmd.data := mainCacheData(31 downto 0)
      validBits(srv.addrA(4 downto 0).asUInt) := True
    }

    // Read port B
    ram.io.buses(1).cmd.valid := decode.isValid
    ram.io.buses(1).cmd.opcode := 0 // Read
    ram.io.buses(1).cmd.address := Plugin[AddressTranslationSrv].translate(srv.addrB)
    ram.io.buses(1).cmd.length := 0
    ram.io.buses(1).cmd.data := 0
    ram.io.buses(1).rsp.ready := True
    srv.isHitB := validBits(srv.addrB(4 downto 0).asUInt)
    srv.dataOutB := ram.io.buses(1).rsp.data
    when(!srv.isHitB) {
      decode.haltWhen(True)
      // Fetch from Main Cache
      val mainCacheData =
        Plugin[MainCacheSrv].read(Plugin[AddressTranslationSrv].translate(srv.addrB))
      ram.io.buses(1).cmd.opcode := 1 // Write
      ram.io.buses(1).cmd.data := mainCacheData(31 downto 0)
      validBits(srv.addrB(4 downto 0).asUInt) := True
    }

    // Write port
    ram.io.buses(2).cmd.valid := srv.writeEnable
    ram.io.buses(2).cmd.opcode := 1 // Write
    ram.io.buses(2).cmd.address := Plugin[AddressTranslationSrv].translate(srv.writeAddr)
    ram.io.buses(2).cmd.length := 0
    ram.io.buses(2).cmd.data := srv.writeData
    ram.io.buses(2).rsp.ready := True
    when(srv.writeEnable) {
      validBits(srv.writeAddr(4 downto 0).asUInt) := True
      // Write-through to Main Cache
      Plugin[MainCacheSrv].write(
        Plugin[AddressTranslationSrv].translate(srv.writeAddr),
        srv.writeData ## srv.writeData ## srv.writeData ## srv.writeData
      )
    }

    val wsCacheLogic = new Area {
      when(decode.isValid) {
        decode.insert(WS_CACHE_ADDR_A) := srv.addrA
        decode.insert(WS_CACHE_ADDR_B) := srv.addrB
        decode.insert(WS_CACHE_DATA_A) := srv.dataOutA
        decode.insert(WS_CACHE_DATA_B) := srv.dataOutB
      }
    }

    when((srv.isHitA || srv.isHitB) && debugEn) {
      report(L"WS_CACHE addrA=$WS_CACHE_ADDR_A addrB=$WS_CACHE_ADDR_B hitA=$isHitA hitB=$isHitB")
    }
  }
}

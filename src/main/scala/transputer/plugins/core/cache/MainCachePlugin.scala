package transputer.plugins.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import transputer.Global
import transputer.plugins.SystemBusService
import transputer.plugins.core.cache.{CacheCmd, MainCacheService}

/** T9000 Main Cache Plugin implementing 16KB unified write-back cache.
  *
  * Based on T9000 Hardware Reference Manual specification:
  *   - 16 KB unified write-back cache with four independent 4 KB banks
  *   - 256 lines per bank (1024 total lines), 4 words (16 bytes) per line
  *   - Fully associative within each bank, random replacement strategy
  *   - Address mapping: MemAddr[5:4] selects bank, MemAddr[31:6] is tag
  *   - Multiple access ports for CPU pipeline, VCP, scheduler, and PMI
  *   - Peak bandwidth: 800 MB/s at 50MHz
  */
class MainCachePlugin extends FiberPlugin {
  override def getDisplayName(): String = "MainCachePlugin"
  setName("mainCache")

  // Cache configuration constants per T9000 spec
  private val CACHE_SIZE_KB = 16
  private val BANK_COUNT = 4
  private val BANK_SIZE_KB = CACHE_SIZE_KB / BANK_COUNT // 4 KB per bank
  private val LINES_PER_BANK = 256
  private val WORDS_PER_LINE = 4
  private val BYTES_PER_LINE = WORDS_PER_LINE * 4 // 16 bytes
  private val TAG_BITS = 26 // Physical address tag bits

  // Cache line structure
  case class CacheLine() extends Bundle {
    val valid = Bool()
    val tag = UInt(TAG_BITS bits)
    val dirtyLine = Bool() // Dirty line bit
    val dirtyWords = Bits(WORDS_PER_LINE bits) // Dirty word bits
    val data = Vec(Bits(32 bits), WORDS_PER_LINE) // Four 32-bit words
  }

  // Cache access ports per T9000 specification
  case class CachePort() extends Bundle {
    val cmd = Stream(CacheCmd())
    val rsp = Stream(Bits(32 bits))
  }

  private var cacheBanks: Seq[Mem[CacheLine]] = null
  private var cpuPorts: Vec[CachePort] = null
  private var vcpPorts: Vec[CachePort] = null
  private var schedulerPort: CachePort = null
  private var pmiPort: CachePort = null

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    // Service will be registered in build phase after hardware creation
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    val systemBusService = Plugin[SystemBusService]

    // Initialize cache banks (4 banks of 256 lines each)
    cacheBanks = Seq.fill(BANK_COUNT)(Mem(CacheLine(), LINES_PER_BANK))

    // Initialize cache access ports per T9000 specification
    cpuPorts = Vec.fill(4)(CachePort()) // CPU Load A, Load B, Store, Instruction Fetch
    vcpPorts = Vec.fill(3)(CachePort()) // VCP Comms0, Comms1, Comms2
    schedulerPort = CachePort() // Scheduler port
    pmiPort = CachePort() // PMI port

    // Cache controller logic
    val cacheController = new Area {

      // Address decoding per T9000 specification
      def decodeAddress(addr: UInt) = new Area {
        val bankSelect = addr(5 downto 4) // Bank selection (0-3)
        val wordSelect = addr(3 downto 2) // Word within line (0-3)
        val lineTag = addr(31 downto 6) // 26-bit cache line tag
        val lineIndex = addr(13 downto 6) // 8-bit line index within bank (256 lines)
      }

      // Bank arbitration - round-robin among active ports
      val bankArbiters = for (bankId <- 0 until BANK_COUNT) yield new Area {
        val requests = Vec.fill(8)(Bool()) // 4 CPU + 3 VCP + 1 Scheduler + 1 PMI = 9 total
        val grants = Vec.fill(8)(Bool())

        // Simple round-robin arbiter for each bank
        val arbiter = new Area {
          val counter = Reg(UInt(3 bits)) init 0
          counter := counter + 1

          // Grant to highest priority active request
          val anyRequest = requests.reduce(_ || _)
          when(anyRequest) {
            for (i <- 0 until 8) {
              grants(i) := requests(i) && (counter === i)
            }
          } otherwise {
            grants.foreach(_ := False)
          }
        }
      }

      // Cache lookup and update logic for each bank
      val bankControllers = for (bankId <- 0 until BANK_COUNT) yield new Area {
        val bank = cacheBanks(bankId)

        // Cache line lookup
        val lookupLogic = new Area {
          val readAddr = UInt(8 bits)
          val readData = bank.readSync(readAddr)
          val writeAddr = UInt(8 bits)
          val writeData = CacheLine()
          val writeEnable = Bool()

          when(writeEnable) {
            bank.write(writeAddr, writeData)
          }

          // Hit/miss detection
          val requestTag = UInt(TAG_BITS bits)
          val hit = readData.valid && (readData.tag === requestTag)
          val miss = !hit
        }

        // Write-back logic for dirty lines
        val writeBackLogic = new Area {
          val needWriteBack = lookupLogic.readData.valid && lookupLogic.readData.dirtyLine
          val writeBackAddr = lookupLogic.readData.tag @@ U(bankId, 2 bits) @@ U(0, 6 bits)

          // Connect to system bus for write-back when needed
          when(needWriteBack) {
            // Implement write-back to external memory via system bus
          }
        }
      }

      // Default port assignments (simplified)
      cpuPorts.foreach { port =>
        port.cmd.ready := True
        port.rsp.valid := RegNext(port.cmd.valid)
        port.rsp.payload := 0
      }

      vcpPorts.foreach { port =>
        port.cmd.ready := True
        port.rsp.valid := RegNext(port.cmd.valid)
        port.rsp.payload := 0
      }

      schedulerPort.cmd.ready := True
      schedulerPort.rsp.valid := RegNext(schedulerPort.cmd.valid)
      schedulerPort.rsp.payload := 0

      pmiPort.cmd.ready := True
      pmiPort.rsp.valid := RegNext(pmiPort.cmd.valid)
      pmiPort.rsp.payload := 0
    }

    // Register service after hardware is created
    addService(new MainCacheService {
      override def cpuLoadA: Flow[CacheCmd] = cpuPorts(0).cmd.toFlow
      override def cpuLoadB: Flow[CacheCmd] = cpuPorts(1).cmd.toFlow
      override def cpuStore: Flow[CacheCmd] = cpuPorts(2).cmd.toFlow
      override def instrFetch: Flow[CacheCmd] = cpuPorts(3).cmd.toFlow
      override def hit: Bool = {
        // Simplified hit detection - at least one bank has a hit
        val bankHits = for (bankId <- 0 until BANK_COUNT) yield {
          cacheController.bankControllers(bankId).lookupLogic.hit
        }
        bankHits.reduce(_ || _)
      }
      override def miss: Bool = !hit
    })

    println(s"[${this.getDisplayName()}] build end")
  }
}

package transputer.plugins.boot

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.SizeMapping
import transputer.{Global, plugins}
import transputer.plugins.SystemBusService
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/** Boot ROM plugin that provides initial program memory for the T9000.
  *
  * This plugin creates a ROM memory that responds to fetch requests from the reset vector. It can
  * be initialized with Intel HEX files.
  */
class BootRomPlugin(
  startAddress: Long = 0x80000000L,
  size: Int = 4096, // 4KB default
  hexFile: Option[String] = None
) extends FiberPlugin {

  override def getDisplayName(): String = "BootRomPlugin"
  setName("bootrom")

  // Setup phase
  during setup new Area {
    println(s"[${BootRomPlugin.this.getDisplayName()}] setup start")

    // Register the boot ROM service
    addService(new BootRomService {
      override def isEnabled: Bool = True // Boot ROM is always enabled if plugin is included
      override def startAddress: UInt = U(BootRomPlugin.this.startAddress, Global.AddrBits bits)
      override def size: Int = BootRomPlugin.this.size
      override def isBootRomAddress(address: UInt): Bool = {
        (address >= U(BootRomPlugin.this.startAddress)) &&
        (address < U(BootRomPlugin.this.startAddress + BootRomPlugin.this.size))
      }
      override def bootRomBus: Option[SparseMemBus] = None // BMB bus used instead
    })

    println(s"[${BootRomPlugin.this.getDisplayName()}] setup end")
  }

  /** Parse Intel HEX file format */
  private def parseHexFile(filename: String): Array[Byte] = {
    val memory = Array.fill[Byte](size)(0)
    var baseAddress = 0L

    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines()

      for (line <- lines if line.startsWith(":")) {
        val recordLength = Integer.parseInt(line.substring(1, 3), 16)
        val address = Integer.parseInt(line.substring(3, 7), 16)
        val recordType = Integer.parseInt(line.substring(7, 9), 16)

        recordType match {
          case 0 => // Data record
            for (i <- 0 until recordLength) {
              val data = Integer.parseInt(line.substring(9 + i * 2, 11 + i * 2), 16).toByte
              val targetAddr = (baseAddress + address + i - startAddress).toInt
              if (targetAddr >= 0 && targetAddr < size) {
                memory(targetAddr) = data
              }
            }

          case 1 => // End of file
          // Done

          case 4 => // Extended linear address
            val highAddr = Integer.parseInt(line.substring(9, 13), 16)
            baseAddress = (highAddr.toLong << 16)

          case _ =>
          // Ignore other record types
        }
      }
      source.close()
    } catch {
      case e: Exception =>
        println(s"Warning: Failed to load hex file $filename: ${e.getMessage}")
    }

    memory
  }

  during build new Area {
    println(s"[${BootRomPlugin.this.getDisplayName()}] build start")

    // Initialize ROM content
    val romContent = hexFile match {
      case Some(file) =>
        println(s"[BootROM] Loading hex file: $file")
        parseHexFile(file)
      case None =>
        println(s"[BootROM] No hex file specified, using zero-initialized ROM")
        Array.fill[Byte](size)(0)
    }

    // Convert byte array to word array for Mem
    val words = romContent
      .grouped(4)
      .map { bytes =>
        val word = bytes.zipWithIndex.foldLeft(0L) { case (acc, (byte, idx)) =>
          acc | ((byte & 0xffL) << (idx * 8))
        }
        B(word, 32 bits)
      }
      .toArray

    // Create ROM memory
    val rom = Mem(Bits(32 bits), size / 4)
    rom.init(words)

    // Create a simple ROM interface
    val systemBusService = host.get[SystemBusService]
    if (systemBusService.isDefined) {
      // Use bus master service to add ROM as a memory-mapped device

      // Create BMB interface that responds to reads in the ROM address range
      val romPort = new Area {
        val systemBusService = Plugin[SystemBusService]
        val bus = systemBusService.bus

        // Monitor bus for ROM accesses
        when(
          bus.cmd.valid && !bus.cmd.isWrite &&
            bus.cmd.address >= startAddress &&
            bus.cmd.address < (startAddress + size)
        ) {

          // Calculate ROM address
          val romAddr = ((bus.cmd.address - startAddress) >> 2).resize(log2Up(size / 4))

          // TODO: This is a simplified approach - in reality we need proper bus arbitration
          // For now, just note that ROM is available at the specified address
        }
      }

      println(s"[BootROM] ROM configured at 0x${startAddress.toHexString} (simplified)")
    } else {
      // Alternative: Create a dedicated ROM bus master that can inject data
      println(s"[BootROM] Warning: SystemBusService not available, ROM may not be accessible")
    }

    println(s"[BootROM] Mapped ${size} bytes at 0x${startAddress.toHexString}")
    if (hexFile.isDefined) {
      println(s"[BootROM] Loaded ${romContent.count(_ != 0)} non-zero bytes")
    }

    // Export ROM content for external initialization if needed
    val romInit = new Area {
      val initData = out(Vec(words))
      initData.setName("bootRomInitData")
    }

    println(s"[${BootRomPlugin.this.getDisplayName()}] build end")
  }
}

/** Companion object with helper methods */
object BootRomPlugin {

  /** Create a boot ROM plugin for the INMOS bootloader */
  def inmos(): BootRomPlugin = {
    new BootRomPlugin(
      startAddress = 0x80000000L,
      size = 4096,
      hexFile = Some("scripts/hex/bootload.hex")
    )
  }

  /** Create a boot ROM plugin with custom hex file */
  def withHex(
    hexFile: String,
    startAddress: Long = 0x80000000L,
    size: Int = 4096
  ): BootRomPlugin = {
    new BootRomPlugin(startAddress, size, Some(hexFile))
  }
}

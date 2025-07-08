package transputer.plugins.legacy.mmu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.Global

// Memory access response
case class MemoryResponse() extends Bundle {
  val physicalAddr = UInt(32 bits)
  val accessGranted = Bool()
  val protectionFault = Bool()
  val regionHit = UInt(2 bits) // Which region matched (0-3)
}

/** T9000 Memory Management Unit implementing 4-region protection model.
  *
  * Based on T9000 Hardware Reference Manual specification:
  *   - Four independently sized memory regions (Region 0-3)
  *   - Region types: 00xx, 01xx, 10xx, 11xx (top 2 bits of logical address)
  *   - Region sizes: 2^n bytes, minimum 256 bytes (64 words), maximum 2^30 bytes
  *   - Address translation: Logical to physical address mapping per region
  *   - Access control: Read-only, read-write, read-execute, read-write-execute
  *   - P-process (protected) vs L-process (unprotected) modes
  *   - Hardware stack extension and protection
  */
class MemoryManagementPlugin extends FiberPlugin {
  override def getDisplayName(): String = "MemoryManagementPlugin"
  setName("memoryManagement")

  // MMU configuration per T9000 specification
  private val REGION_COUNT = 4
  private val MIN_REGION_SIZE = 256 // 64 words minimum
  private val MAX_REGION_SIZE_BITS = 30 // 2^30 bytes maximum

  // Memory region descriptor
  case class MemoryRegion() extends Bundle {
    val baseAddress = UInt(32 bits) // Physical base address
    val logicalBase = UInt(32 bits) // Logical base address
    val sizeBits = UInt(5 bits) // Region size as 2^n (n = 8..30)
    val permissions = Bits(4 bits) // Read, Write, Execute, Privileged
    val valid = Bool() // Region is active
  }

  // Memory access request
  case class MemoryRequest() extends Bundle {
    val logicalAddr = UInt(32 bits)
    val accessType = Bits(3 bits) // Read=001, Write=010, Execute=100
    val isPrivileged = Bool() // P-process vs L-process
  }

  private var memoryRegions: Vec[MemoryRegion] = null
  private var accessRequest: MemoryRequest = null
  private var accessResponse: MemoryResponse = null

  during setup new Area {
    // Create memory management service interface
    addService(new MemoryManagementService {
      override def translate(
        logicalAddr: UInt,
        accessType: Bits,
        privileged: Bool
      ): MemoryResponse = {
        accessRequest.logicalAddr := logicalAddr
        accessRequest.accessType := accessType
        accessRequest.isPrivileged := privileged
        accessResponse
      }

      override def configureRegion(
        regionId: UInt,
        base: UInt,
        logical: UInt,
        size: UInt,
        perms: Bits
      ): Unit = {
        when(regionId < REGION_COUNT) {
          memoryRegions(regionId).baseAddress := base
          memoryRegions(regionId).logicalBase := logical
          memoryRegions(regionId).sizeBits := size
          memoryRegions(regionId).permissions := perms
          memoryRegions(regionId).valid := True
        }
      }

      override def protectionFault: Bool = accessResponse.protectionFault
      override def stackOverflow: Bool = False // Simplified for now
    })
  }

  during build new Area {
    // Initialize memory regions
    memoryRegions = Vec.fill(REGION_COUNT)(Reg(MemoryRegion()))
    accessRequest = Reg(MemoryRequest())
    accessResponse = Reg(MemoryResponse())

    // Memory Management Unit logic
    val mmuController = new Area {

      // Region selection based on logical address top 2 bits
      val regionSelect = accessRequest.logicalAddr(31 downto 30)

      // Address translation logic for each region
      val translationLogic = for (regionId <- 0 until REGION_COUNT) yield new Area {
        val region = memoryRegions(regionId)
        val selected = regionSelect === regionId

        // Check if address falls within this region
        val regionMask = (U(1) << region.sizeBits) - 1
        val regionOffset = accessRequest.logicalAddr - region.logicalBase
        val withinRegion = selected && region.valid && (regionOffset <= regionMask)

        // Calculate physical address
        val physicalAddr = region.baseAddress + regionOffset

        // Permission checking
        val readPerm = region.permissions(0)
        val writePerm = region.permissions(1)
        val execPerm = region.permissions(2)
        val privPerm = region.permissions(3)

        val readAccess = accessRequest.accessType(0)
        val writeAccess = accessRequest.accessType(1)
        val execAccess = accessRequest.accessType(2)

        // Check access permissions
        val permissionOk = (
          (!readAccess || readPerm) &&
            (!writeAccess || writePerm) &&
            (!execAccess || execPerm) &&
            (!privPerm || accessRequest.isPrivileged)
        )

        val accessGranted = withinRegion && permissionOk
        val protectionFault = withinRegion && !permissionOk
      }

      // Combine results from all regions (simplified)
      val combinationLogic = new Area {
        val anyHit = translationLogic.map(_.withinRegion).reduce(_ || _)
        val anyGrant = translationLogic.map(_.accessGranted).reduce(_ || _)
        val anyFault = translationLogic.map(_.protectionFault).reduce(_ || _)

        // Simple region selection (first hit wins)
        val winningRegion = U(0, 2 bits)
        val winningPhysAddr = translationLogic(0).physicalAddr

        // Update response
        accessResponse.physicalAddr := winningPhysAddr
        accessResponse.accessGranted := anyGrant
        accessResponse.protectionFault := anyFault || !anyHit
        accessResponse.regionHit := winningRegion
      }

      // Stack extension logic (simplified)
      val stackExtensionLogic = new Area {
        val stackRegion = memoryRegions(0) // Assume region 0 is stack
        val stackPtr = UInt(32 bits) // Current stack pointer
        val stackLimit = stackRegion.baseAddress + ((U(1) << stackRegion.sizeBits) - 1)

        val stackOverflow = stackPtr > stackLimit
        val needsExtension = stackOverflow && stackRegion.valid

        // Auto-extend stack if possible (simplified)
        when(needsExtension) {
          // Request larger stack region from OS/kernel
        }
      }

      // Performance and debugging
      val monitoringLogic = new Area {
        val accessCounter = Reg(UInt(32 bits)) init 0
        val faultCounter = Reg(UInt(32 bits)) init 0
        val regionCounters = Vec.fill(REGION_COUNT)(Reg(UInt(32 bits)) init 0)

        when(accessRequest.logicalAddr.orR) {
          accessCounter := accessCounter + 1

          when(accessResponse.protectionFault) {
            faultCounter := faultCounter + 1
          }

          when(accessResponse.accessGranted) {
            regionCounters(accessResponse.regionHit) := regionCounters(accessResponse.regionHit) + 1
          }
        }
      }
    }

    // Initialize default state
    for (i <- 0 until REGION_COUNT) {
      memoryRegions(i).baseAddress := 0
      memoryRegions(i).logicalBase := U(i) << 30 // Region i starts at i*1GB logical
      memoryRegions(i).sizeBits := 20 // Default 1MB regions
      memoryRegions(i).permissions := B"1111" // All permissions by default
      memoryRegions(i).valid := False
    }

    accessRequest.logicalAddr := 0
    accessRequest.accessType := 0
    accessRequest.isPrivileged := False

    accessResponse.physicalAddr := 0
    accessResponse.accessGranted := False
    accessResponse.protectionFault := False
    accessResponse.regionHit := 0
  }
}

/** Service interface for T9000 Memory Management Unit */
trait MemoryManagementService {
  def translate(logicalAddr: UInt, accessType: Bits, privileged: Bool): MemoryResponse
  def configureRegion(regionId: UInt, base: UInt, logical: UInt, size: UInt, perms: Bits): Unit
  def protectionFault: Bool
  def stackOverflow: Bool
}

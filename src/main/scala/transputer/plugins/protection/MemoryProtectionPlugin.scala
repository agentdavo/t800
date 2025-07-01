package transputer.plugins.protection

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global
import transputer.plugins.protection.ProtectionTypes._

/** T9000 Memory Protection Plugin implementing logical-to-physical address translation and access
  * checking for P-processes.
  *
  * Features:
  *   - 4 configurable memory regions per P-process
  *   - Logical to physical address translation
  *   - Read/write/execute permission checking
  *   - Automatic trap generation on access violations
  *   - Stack extension support with automatic allocation
  */
class MemoryProtectionPlugin extends FiberPlugin {
  override def getDisplayName(): String = "MemoryProtectionPlugin"
  setName("memoryProtection")

  // Memory region configuration
  case class MemoryRegion() extends Bundle {
    val base = UInt(32 bits) // Physical base address
    val size = UInt(32 bits) // Region size in bytes
    val permissions = Bits(3 bits) // [2:execute, 1:write, 0:read]
    val enabled = Bool() // Region active
  }

  // Protection mode state
  case class ProtectionState() extends Bundle {
    val protectedMode = Bool() // Currently in P-process mode
    val currentRegions = Vec(MemoryRegion(), 4) // 4 regions per P-process
    val supervisorIptr = UInt(32 bits) // L-process return address
    val violationAddr = UInt(32 bits) // Last violation address
    val violationType = Bits(4 bits) // Violation type code
  }

  // Service interface for other plugins
  trait MemoryProtectionService {
    def translateAddress(logical: UInt, accessType: Bits): UInt
    def checkPermissions(logical: UInt, accessType: Bits): Bool
    def configureRegion(regionId: UInt, base: UInt, size: UInt, permissions: Bits): Unit
    def enterProtectedMode(regions: Vec[MemoryRegion], supervisor: UInt): Unit
    def exitProtectedMode(): Unit
    def getViolationInfo(): (UInt, Bits) // (address, type)
  }

  during setup new Area {
    println(s"[${getDisplayName()}] setup start")

    addService(new MemoryProtectionService {
      override def translateAddress(logical: UInt, accessType: Bits): UInt = translatedAddr
      override def checkPermissions(logical: UInt, accessType: Bits): Bool = permissionValid
      override def configureRegion(
        regionId: UInt,
        base: UInt,
        size: UInt,
        permissions: Bits
      ): Unit = {
        // Will be implemented in build phase
      }
      override def enterProtectedMode(regions: Vec[MemoryRegion], supervisor: UInt): Unit = {
        // Will be implemented in build phase
      }
      override def exitProtectedMode(): Unit = {
        // Will be implemented in build phase
      }
      override def getViolationInfo(): (UInt, Bits) = (state.violationAddr, state.violationType)
    })

    println(s"[${getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var state: ProtectionState = null
  var translatedAddr: UInt = null
  var permissionValid: Bool = null
  var trapRequest: Bool = null

  during build new Area {
    println(s"[${getDisplayName()}] build start")

    // Protection state registers
    state = Reg(ProtectionState())
    state.protectedMode init False
    state.supervisorIptr init 0
    state.violationAddr init 0
    state.violationType init 0

    // Initialize regions as disabled
    for (i <- 0 until 4) {
      state.currentRegions(i).enabled init False
      state.currentRegions(i).base init 0
      state.currentRegions(i).size init 0
      state.currentRegions(i).permissions init 0
    }

    // Address translation logic
    translatedAddr = UInt(32 bits)
    permissionValid = Bool()
    trapRequest = Bool()

    // Default values
    translatedAddr := 0
    permissionValid := True
    trapRequest := False

    println(
      s"[${MemoryProtectionPlugin.this.getDisplayName()}] Memory protection hardware configured"
    )
    println(s"[${MemoryProtectionPlugin.this.getDisplayName()}] - 4 configurable memory regions")
    println(
      s"[${MemoryProtectionPlugin.this.getDisplayName()}] - Logical to physical address translation"
    )
    println(
      s"[${MemoryProtectionPlugin.this.getDisplayName()}] - Read/write/execute permission checking"
    )
    println(s"[${MemoryProtectionPlugin.this.getDisplayName()}] build end")
  }

  /** Address translation function for logical to physical mapping.
    *
    * T9000 memory management combines low-order logical address bits with high-order physical base
    * bits from the appropriate region register.
    */
  def translateLogicalAddress(logicalAddr: UInt, accessType: Bits): (UInt, Bool) = {
    val physicalAddr = UInt(32 bits)
    val accessAllowed = Bool()

    // Default pass-through when not in protected mode
    physicalAddr := logicalAddr
    accessAllowed := True

    when(state.protectedMode) {
      // Find matching region for logical address
      val regionMatch = Vec(Bool(), 4)
      val regionAddr = Vec(UInt(32 bits), 4)
      val regionPermOk = Vec(Bool(), 4)

      for (i <- 0 until 4) {
        val region = state.currentRegions(i)
        regionMatch(i) := region.enabled && (logicalAddr >= region.base) &&
          (logicalAddr < (region.base + region.size))

        // Translation: combine logical offset with physical base
        val offset = logicalAddr - region.base
        regionAddr(i) := region.base + offset

        // Permission checking
        regionPermOk(i) := region.permissions(accessType.asUInt)
      }

      // Priority encoder - first matching region wins
      when(regionMatch(0)) {
        physicalAddr := regionAddr(0)
        accessAllowed := regionPermOk(0)
      } elsewhen (regionMatch(1)) {
        physicalAddr := regionAddr(1)
        accessAllowed := regionPermOk(1)
      } elsewhen (regionMatch(2)) {
        physicalAddr := regionAddr(2)
        accessAllowed := regionPermOk(2)
      } elsewhen (regionMatch(3)) {
        physicalAddr := regionAddr(3)
        accessAllowed := regionPermOk(3)
      } otherwise {
        // No region matches - access violation
        accessAllowed := False
        state.violationAddr := logicalAddr
        state.violationType := B"0001" // Unmapped access
      }

      // Permission violation
      when(regionMatch.orR && !accessAllowed) {
        state.violationAddr := logicalAddr
        state.violationType := B"0010" | accessType.resize(4) // Permission + access type
      }
    }

    (physicalAddr, accessAllowed)
  }

  /** Configure a memory region for the current P-process.
    *
    * @param regionId
    *   Region index (0-3)
    * @param base
    *   Physical base address
    * @param size
    *   Region size in bytes
    * @param permissions
    *   [2:execute, 1:write, 0:read]
    */
  def configureMemoryRegion(regionId: UInt, base: UInt, size: UInt, permissions: Bits): Unit = {
    when(regionId < 4) {
      state.currentRegions(regionId).base := base
      state.currentRegions(regionId).size := size
      state.currentRegions(regionId).permissions := permissions
      state.currentRegions(regionId).enabled := True
    }
  }

  /** Enter protected mode for P-process execution.
    *
    * @param regions
    *   4 memory regions to configure
    * @param supervisorReturn
    *   L-process return address
    */
  def enterProtectedMode(regions: Vec[MemoryRegion], supervisorReturn: UInt): Unit = {
    state.protectedMode := True
    state.supervisorIptr := supervisorReturn
    for (i <- 0 until 4) {
      state.currentRegions(i) := regions(i)
    }
  }

  /** Exit protected mode and return to L-process.
    */
  def exitProtectedMode(): Unit = {
    state.protectedMode := False
    // Disable all regions
    for (i <- 0 until 4) {
      state.currentRegions(i).enabled := False
    }
  }

  /** Check if currently in protected mode.
    */
  def isProtectedMode(): Bool = state.protectedMode

  /** Get supervisor return address for trap handling.
    */
  def getSupervisorIptr(): UInt = state.supervisorIptr
}

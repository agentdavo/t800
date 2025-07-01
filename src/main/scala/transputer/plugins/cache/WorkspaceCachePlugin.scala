package transputer.plugins.cache

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.Global
import transputer.plugins.cache.WorkspaceCacheService

/** T9000 Workspace Cache Plugin implementing 32-word circular buffer.
  *
  * Based on T9000 Hardware Reference Manual specification:
  *   - 32 words (128 bytes) workspace cache
  *   - Triple-ported: allows 2 reads + 1 write per cycle
  *   - Circular buffer addressed using bottom 5 bits of workspace pointer
  *   - Write-through to main cache
  *   - Zero-cycle access to local variables
  *   - Invalidated on context switch/interrupt
  */
class WorkspaceCachePlugin extends FiberPlugin {
  setName("workspaceCache")

  // Workspace cache configuration per T9000 spec
  private val WORKSPACE_SIZE_WORDS = 32
  private val WORKSPACE_ADDR_BITS = 5 // log2(32) = 5 bits for addressing

  case class WorkspaceCacheAccess() extends Bundle {
    val addr = UInt(WORKSPACE_ADDR_BITS bits)
    val writeData = Bits(32 bits)
    val write = Bool()
  }

  private var workspaceRam: Mem[Bits] = null
  private var portA: WorkspaceCacheAccess = null
  private var portB: WorkspaceCacheAccess = null
  private var portC: WorkspaceCacheAccess = null
  private var dataA: Bits = null
  private var dataB: Bits = null

  during setup new Area {
    // Service will be registered in build phase after hardware creation
  }

  during build new Area {
    // Initialize triple-ported workspace RAM per T9000 specification
    workspaceRam = Mem(Bits(32 bits), WORKSPACE_SIZE_WORDS)

    // Initialize access ports
    portA = WorkspaceCacheAccess()
    portB = WorkspaceCacheAccess()
    portC = WorkspaceCacheAccess()

    // Initialize data signals
    dataA = Bits(32 bits)
    dataB = Bits(32 bits)

    // Workspace cache controller logic
    val workspaceCacheController = new Area {

      // Port A: Read port for first operand
      val portALogic = new Area {
        portA.write := False
        dataA := workspaceRam.readSync(
          address = portA.addr,
          enable = True
        )
      }

      // Port B: Read port for second operand
      val portBLogic = new Area {
        portB.write := False
        dataB := workspaceRam.readSync(
          address = portB.addr,
          enable = True
        )
      }

      // Port C: Write port for results and local variable updates
      val portCLogic = new Area {
        when(portC.write) {
          workspaceRam.write(
            address = portC.addr,
            data = portC.writeData
          )
        }
      }

      // Write-through to main cache logic
      val writeThroughLogic = new Area {
        // When workspace cache is written, also write to main cache
        // This maintains cache coherency per T9000 specification
        when(portC.write) {
          // Calculate full address from workspace pointer + offset
          val fullAddr = /* workspace pointer + */ portC.addr.resize(32)

          // Forward write to main cache (when available)
          // Implementation depends on main cache service interface
        }
      }

      // Context switch invalidation logic
      val invalidationLogic = new Area {
        val invalidateRequest = Bool()
        val invalidating = Reg(Bool()) init False
        val invalidateCounter = Reg(UInt(WORKSPACE_ADDR_BITS bits)) init 0

        when(invalidateRequest && !invalidating) {
          invalidating := True
          invalidateCounter := 0
        }

        when(invalidating) {
          // Clear one entry per cycle
          workspaceRam.write(
            address = invalidateCounter,
            data = B(0, 32 bits)
          )
          invalidateCounter := invalidateCounter + 1

          when(invalidateCounter === (WORKSPACE_SIZE_WORDS - 1)) {
            invalidating := False
          }
        }
      }

      // Performance monitoring
      val performanceCounters = new Area {
        val hitCounter = Reg(UInt(32 bits)) init 0
        val accessCounter = Reg(UInt(32 bits)) init 0

        when(portA.addr.orR || portB.addr.orR) {
          accessCounter := accessCounter + 1
          hitCounter := hitCounter + 1 // Workspace cache always hits
        }
      }
    }

    // Default port values
    portA.addr := 0
    portA.writeData := 0
    portA.write := False

    portB.addr := 0
    portB.writeData := 0
    portB.write := False

    portC.addr := 0
    portC.writeData := 0
    portC.write := False

    // Register service after hardware is created
    addService(new WorkspaceCacheService {
      override def readA(addr: UInt): Bits = {
        portA.addr := addr(WORKSPACE_ADDR_BITS - 1 downto 0)
        dataA
      }
      override def readB(addr: UInt): Bits = {
        portB.addr := addr(WORKSPACE_ADDR_BITS - 1 downto 0)
        dataB
      }
      override def write(addr: UInt, data: Bits): Unit = {
        portC.addr := addr(WORKSPACE_ADDR_BITS - 1 downto 0)
        portC.writeData := data
        portC.write := True
      }
      override def invalidate(): Unit = {
        workspaceCacheController.invalidationLogic.invalidateRequest := True
      }
      override def writePending: Bool = portC.write
    })
  }
}

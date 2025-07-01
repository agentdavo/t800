package transputer.plugins.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.plugin._
import transputer.plugins.SystemBusService
import scala.collection.mutable.ArrayBuffer

/** System bus management plugin that handles arbitration between multiple bus masters.
  *
  * This plugin provides:
  *   - Registration of multiple bus masters (FetchPlugin, PrimaryInstrPlugin, etc.)
  *   - Arbitration between masters using SpinalHDL's BmbArbiter
  *   - The system bus output for external memory access
  */
class SystemBusPlugin extends FiberPlugin {
  setName("systemBus")

  case class BusMaster(name: String, port: Bmb)
  private val masters = ArrayBuffer[BusMaster]()

  during setup new Area {
    // Register service for plugins to add their bus masters
    addService(new BusMasterService {
      override def addMaster(name: String, master: Bmb): Unit = {
        masters += BusMaster(name, master)
      }
    })
  }

  during build new Area {
    // Get the system bus from the host
    val systemBusService = host[SystemBusService]
    val systemBus = systemBusService.bus

    if (masters.nonEmpty) {
      if (masters.length == 1) {
        // Single master - direct connection
        masters.head.port >> systemBus
      } else {
        // Multiple masters - need arbitration
        val inputParams = masters.map(_.port.p)
        val arbiter = BmbArbiter(
          inputsParameter = inputParams,
          outputParameter = systemBus.p,
          lowerFirstPriority = false, // Round-robin arbitration
          pendingInvMax = 4
        )

        // Connect masters to arbiter inputs
        masters.zipWithIndex.foreach { case (master, idx) =>
          master.port >> arbiter.io.inputs(idx)
        }

        // Connect arbiter output to system bus
        arbiter.io.output >> systemBus
      }

      // Set debug names for better waveform analysis
      masters.foreach { master =>
        master.port.setName(s"${master.name}_bus")
      }
    } else {
      // No masters - idle the bus
      systemBus.cmd.valid := False
      systemBus.cmd.opcode := 0
      systemBus.cmd.address := 0
      systemBus.cmd.length := 0
      systemBus.cmd.data := 0
      systemBus.cmd.mask := 0
      systemBus.cmd.last := True
      systemBus.rsp.ready := True
    }
  }
}

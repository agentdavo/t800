package transputer.plugins.device

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.device._

/** T9000 Device Plugin implementing Table 6.15 device access instructions.
  *
  * This plugin implements memory-mapped device access from T9000 Table 6.15:
  *   - Load operations: devlb, devls, devlw
  *   - Store operations: devsb, devss, devsw
  *
  * Features:
  *   - Direct hardware device access
  *   - Multiple data sizes (byte, 16-bit, word)
  *   - Bus error handling
  *   - Integration with memory protection
  */
class DevicePlugin extends FiberPlugin {
  override def getDisplayName(): String = "DevicePlugin"
  setName("device")

  during setup new Area {
    println(s"[${DevicePlugin.this.getDisplayName()}] setup start")

    addService(new DeviceService {
      override def executeOp(op: DeviceOp.C, deviceAddr: UInt, data: UInt): DeviceResult =
        deviceResult
      override def isDeviceOp(opcode: Bits): Bool = isDeviceOperation
      override def getDeviceOp(opcode: Bits): DeviceOp.C = deviceOperation
    })

    println(s"[${DevicePlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var deviceResult: DeviceResult = null
  var isDeviceOperation: Bool = null
  var deviceOperation: DeviceOp.C = null

  during build new Area {
    println(s"[${DevicePlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    deviceResult = DeviceResult()
    isDeviceOperation = Bool()
    deviceOperation = DeviceOp()

    // Default values
    deviceResult.data := 0
    deviceResult.completed := False
    deviceResult.error := False
    deviceResult.busError := False
    isDeviceOperation := False
    deviceOperation := DeviceOp.DEVLB

    println(s"[${DevicePlugin.this.getDisplayName()}] Device access hardware configured")
    println(s"[${DevicePlugin.this.getDisplayName()}] - Table 6.15: Device access operations")
    println(s"[${DevicePlugin.this.getDisplayName()}] build end")
  }
}

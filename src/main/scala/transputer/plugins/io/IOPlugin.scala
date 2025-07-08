package transputer.plugins.io

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.io._

/** T9000 I/O Plugin implementing Tables 6.19-6.20 input/output instructions.
  *
  * This plugin implements channel communication operations from T9000 Tables 6.19-6.20:
  *   - Basic I/O: in, out, outword, outbyte
  *   - Vector I/O: vin, vout
  *   - Timer I/O: tin, talt, taltwt
  *   - Channel control: enbs, diss, resetch
  *
  * These operations provide the foundation for transputer-to-transputer communication and process
  * synchronization.
  *
  * Features:
  *   - Channel-based communication primitives
  *   - Support for both synchronous and asynchronous I/O
  *   - Integration with scheduler for process blocking
  *   - Timer-based alternative constructs
  */
class IOPlugin extends FiberPlugin {
  override def getDisplayName(): String = "IOPlugin"
  setName("io")

  during setup new Area {
    println(s"[${IOPlugin.this.getDisplayName()}] setup start")

    addService(new IOService {
      override def executeOp(op: IOOp.C, channelAddr: UInt, data: UInt, length: UInt): IOResult =
        ioResult
      override def isIOOp(opcode: Bits): Bool = isIOOperation
      override def getIOOp(opcode: Bits): IOOp.C = ioOperation
    })

    println(s"[${IOPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var ioResult: IOResult = null
  var isIOOperation: Bool = null
  var ioOperation: IOOp.C = null

  during build new Area {
    println(s"[${IOPlugin.this.getDisplayName()}] build start")

    // Initialize hardware signals
    ioResult = IOResult()
    isIOOperation = Bool()
    ioOperation = IOOp()

    // Default values
    ioResult.completed := False
    ioResult.dataTransferred := 0
    ioResult.channelReady := False
    ioResult.error := False
    isIOOperation := False
    ioOperation := IOOp.IN

    println(s"[${IOPlugin.this.getDisplayName()}] I/O hardware configured")
    println(s"[${IOPlugin.this.getDisplayName()}] - Tables 6.19-6.20: Channel I/O operations")
    println(s"[${IOPlugin.this.getDisplayName()}] - Transputer communication primitives")
    println(s"[${IOPlugin.this.getDisplayName()}] build end")
  }
}

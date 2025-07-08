package transputer

import spinal.core._
import spinal.lib._

/** T9000 Transputer with Debug Interface for Hardware Testing */
class T9000TransputerWithDebug(param: T9000Param = T9000Param()) extends Component {
  
  // Standard T9000 core
  val core = new T9000Transputer(param)
  
  // Debug interface for FPU testing
  val io = new Bundle {
    // Standard T9000 I/O
    val system = core.io.system
    val links = core.io.links
    val events = core.io.events
    
    // Debug outputs for FPU testing
    val debug = new Bundle {
      val fpuBusy = out Bool()
      val fpuExceptions = out Bits(5 bits)
      val fpuEdgeCase = out Bool()
      val fpuCycleCount = out UInt(6 bits)
      val fpuCurrentOp = out Bits(8 bits)
    }
  }
  
  // Connect standard I/O
  io.system <> core.io.system
  io.links <> core.io.links
  io.events.interrupt := core.io.events.interrupt
  core.io.events.error := io.events.error
  
  // Connect debug signals if FPU is enabled
  if (param.enableFpu) {
    val fpuPlugin = core.getFpuPlugin
    if (fpuPlugin != null) {
      io.debug.fpuBusy := fpuPlugin.fpuBusy
      io.debug.fpuExceptions := fpuPlugin.fpuExceptions
      io.debug.fpuEdgeCase := fpuPlugin.edgeCaseDetected
      io.debug.fpuCycleCount := fpuPlugin.cycleCounter
      // Handle null currentOp during initialization
      io.debug.fpuCurrentOp := (if (fpuPlugin.currentOp != null) fpuPlugin.currentOp.asBits.resize(8) else B(0, 8 bits))
    } else {
      io.debug.fpuBusy := False
      io.debug.fpuExceptions := 0
      io.debug.fpuEdgeCase := False
      io.debug.fpuCycleCount := 0
      io.debug.fpuCurrentOp := 0
    }
  } else {
    io.debug.fpuBusy := False
    io.debug.fpuExceptions := 0
    io.debug.fpuEdgeCase := False
    io.debug.fpuCycleCount := 0
    io.debug.fpuCurrentOp := 0
  }
}
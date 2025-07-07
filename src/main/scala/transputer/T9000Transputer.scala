package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.bus.bmb._

/** T9000 Transputer specific utilities. */
object T9000Transputer {

  /** Configure global constants based on T9000 parameters. This is a simplified approach that
    * doesn't use Database.
    */
  def configureGlobals(param: T9000Param): Unit = {
    // The globals are compile-time constants in this implementation
    // In a full implementation, these would be set via Database
    println(s"[T9000] Configuring with:")
    println(s"  Word Width: ${param.wordWidth} bits")
    println(s"  Address Width: ${param.addrWidth} bits")
    println(s"  Link Count: ${param.linkCount}")
    println(s"  FPU Precision: ${param.fpuPrecision} bits")
    println(s"  Cache: ${param.mainCacheKb}KB main + ${param.wsCacheWords}-word workspace")
  }

  /** Create T9000 Transputer with plugins. */
  def apply(plugins: scala.collection.Seq[Hostable]): Transputer = {
    Transputer(plugins)
  }
}

/** T9000 Transputer Implementation
  * 
  * Full T9000 transputer with all enhanced features as configured
  * by T9000Param. This includes FPU, VCP, PMI, and other T9000-specific
  * components.
  */
class T9000Transputer(param: T9000Param = T9000Param()) extends Component {
  
  // Create the core transputer with plugins
  val core = Transputer(param.plugins())
  
  // T9000 IO interface
  val io = new Bundle {
    val system = new Bundle {
      val bmb = master(Bmb(Transputer.systemBusParam))
    }
    
    val links = Vec(slave(Stream(Bits(32 bits))), param.linkCount)
    
    val events = new Bundle {
      val interrupt = in Bool()
      val error = out Bool()
    }
  }
  
  // Connect system bus
  io.system.bmb <> core.systemBus
  
  // Connect other interfaces (simplified for now)
  io.events.error := False
  io.links.foreach(_.ready := True)
  
  // Provide access to FPU plugin for debug interface
  def getFpuPlugin: transputer.plugins.fpu.FpuMinimal = {
    if (param.enableFpu) {
      core.host[transputer.plugins.fpu.FpuMinimal]
    } else {
      null
    }
  }
}
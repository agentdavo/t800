package t800

import spinal.core._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins.transputer.TransputerPlugin

/** Variant-aware entry point used by the build scripts. */

object TopVerilog {
  def main(args: Array[String]): Unit = {
    
    // Configure Database for variant support
    val db = T800.defaultDatabase()
    
    if (args.contains("--double-precision")) {
      db(Global.FPU_PRECISION) = 64
    }
    args.find(_.startsWith("--link-count=")).foreach { arg =>
      db(Global.LINK_COUNT) = arg.split("=")(1).toInt
    }
    args.find(_.startsWith("--ram-words=")).foreach { arg =>
      db(Global.RAM_WORDS) = arg.split("=")(1).toInt
    }

    // Generate Verilog with default plugins
    val report = SpinalVerilog {
      val host = new PluginHost
      new T800(host, T800.defaultPlugins(), db)
    }
    
    println(s"Verilog generated: ${report.toplevelName}")
    
  }
}

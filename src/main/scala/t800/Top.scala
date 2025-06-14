package t800

import spinal.core._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */
object TopVerilog {
  def main(args: Array[String]): Unit = {
    // Configure database with variant support (e.g., FPU precision)
    val db = T800.defaultDatabase()
    if (args.contains("--double-precision")) {
      db(Global.FPU_PRECISION) = 64 // Override for double-precision FPU
    }
    require(db != null, "Failed to initialize T800 database")
    val report = SpinalVerilog {
      val host = new PluginHost
      val plugins = T800.defaultPlugins()
      new T800(host, plugins, db)
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

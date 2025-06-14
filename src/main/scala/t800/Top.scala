package t800

import spinal.core._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */
object TopVerilog {
  def main(args: Array[String]): Unit = {
    val db = T800.defaultDatabase()
    require(db != null, "Failed to initialize T800 database")
    val report = SpinalVerilog {
      val host = new PluginHost
      val plugins = T800.defaultPlugins()
      new T800(host, plugins, db)
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

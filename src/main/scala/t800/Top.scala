package t800

import spinal.core._
import spinal.lib.misc.plugin.PluginHost
import t800.plugins._

/** Variant-aware entry point used by the build scripts. */

object TopVerilog {
  def main(args: Array[String]): Unit = {
    
    // Build plugin list with configured TransputerPlugin
    val plugins = Seq(
      new TransputerPlugin(
        fpuPrecision = if (args.contains("--double-precision")) 64 else Global.FpuPrecision,
        linkCount = args.find(_.startsWith("--link-count=")).map(_.split("=")(1).toInt).getOrElse(Global.LinkCount),
        ramWords = args.find(_.startsWith("--ram-words=")).map(_.split("=")(1).toInt).getOrElse(Global.RamWords)
      )
    ) ++ T800.defaultPlugins().tail

    // Generate Verilog
    val db = T800.defaultDatabase()
    val report = SpinalVerilog {
      val host = new PluginHost
      new T800(host, plugins, db)
    }
    println(s"Verilog generated: ${report.toplevelName}")
  }
}

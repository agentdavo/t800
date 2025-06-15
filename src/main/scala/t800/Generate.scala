package t800

import spinal.core._
import spinal.lib.misc.database.Database
import scopt.OParser

object Generate {
  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Param]
    val parser = {
      import builder._
      OParser.sequence(
        programName("Generate"),
        opt[Int]("word-width").action((x, c) => c.copy(wordWidth = x)),
        opt[Int]("link-count").action((x, c) => c.copy(linkCount = x)),
        opt[Boolean]("fpu").action((x, c) => c.copy(enableFpu = x)),
        help("help")
      )
    }
    OParser.parse(parser, args, Param()) match {
      case Some(p) =>
        val db = T800.defaultDatabase()
        val core = Database(db).on(T800(p.plugins()))
        val report = SpinalVerilog(core)
        println(s"Verilog generated: ${report.toplevelName}")
      case _ =>
        println("Invalid arguments")
    }
  }
}

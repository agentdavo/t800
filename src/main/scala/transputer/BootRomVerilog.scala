package transputer

import spinal.core._
import spinal.lib.misc.database.Database

/** Minimal entry point generating Verilog for the unit plugin set. */
object BootRomVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new BootRomDesign())
  }
}

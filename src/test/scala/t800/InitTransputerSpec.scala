package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import t800.plugins.PipelineSrv

/** Ensure minimal T800 configuration builds with basic pipeline. */
class InitTransputerSpec extends AnyFunSuite {
  test("TransputerPlugin sets default configuration") {
    val db = T800.defaultDatabase()
    val report = SpinalConfig().generateVerilog(new T800Unit(db))
    assert(db(Global.WORD_BITS) == Global.WordBits)
    assert(db(Global.RAM_WORDS) == Global.RamWords)
  }
}

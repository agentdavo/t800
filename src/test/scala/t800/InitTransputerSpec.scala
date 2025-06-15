package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.plugin.PluginHost

/** Ensure minimal T800 configuration builds with only TransputerPlugin. */
class InitTransputerSpec extends AnyFunSuite {
  test("TransputerPlugin sets default configuration") {
    val db = T800.defaultDatabase()
    SimConfig
      .compile(
        PluginHost.on {
          new T800Unit(db)
        }
      )
      .doSim { _ =>
        assert(db(Global.WORD_BITS) == Global.WordBits)
        assert(db(Global.RAM_WORDS) == Global.RamWords)
      }
  }
}

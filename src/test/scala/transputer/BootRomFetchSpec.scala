package transputer

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import transputer.plugins.fetch.{FetchPlugin, InstrFetchService}

class BootRomFetchSpec extends AnyFunSuite {
  test("BootRomDesign exposes FetchPlugin signals") {
    SimConfig.compile(new BootRomDesign()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      val fetch = dut.core.host[InstrFetchService]
      assert(dut.core.host[FetchPlugin] != null)
      assert(!fetch.cmd.valid.toBoolean)
      assert(!fetch.rsp.valid.toBoolean)
      dut.clockDomain.waitSampling(5)
    }
  }
}

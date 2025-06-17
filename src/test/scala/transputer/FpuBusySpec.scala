package transputer

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class BusyDut extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val cycles = in UInt (10 bits)
    val busy = out Bool ()
  }
  val counter = Reg(UInt(10 bits)) init (0)

  when(io.start) {
    counter := io.cycles
  } elsewhen (counter =/= 0) {
    counter := counter - 1
  }

  io.busy := counter =/= 0
}

class FpuBusySpec extends AnyFunSuite {
  test("busy clears after operations") {
    SimConfig.compile(new BusyDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      def issue(c: Int): Unit = {
        dut.io.start #= true
        dut.io.cycles #= c
        dut.clockDomain.waitSampling()
        dut.io.start #= false
      }

      issue(2)
      dut.clockDomain.waitSampling()
      for (_ <- 0 until 2) {
        assert(dut.io.busy.toBoolean)
        dut.clockDomain.waitSampling()
      }

      issue(1)
      dut.clockDomain.waitSampling()
      for (_ <- 0 until 1) {
        assert(dut.io.busy.toBoolean)
        dut.clockDomain.waitSampling()
      }

      assert(!dut.io.busy.toBoolean)
    }
  }
}

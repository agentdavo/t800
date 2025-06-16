package t800

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class BusyDut extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val cycles = in UInt (10 bits)
    val busy = out Bool ()
  }
  val cycleCnt = Reg(UInt(10 bits)) init (0)
  val maxCycles = Reg(UInt(10 bits)) init (0)

  when(io.start) {
    maxCycles := io.cycles
    cycleCnt := 0
  }

  when(cycleCnt < maxCycles) {
    cycleCnt := cycleCnt + 1
  } otherwise {
    cycleCnt := 0
  }

  io.busy := cycleCnt < maxCycles
}

class FpuBusySpec extends AnyFunSuite {
  test("busy clears after operations") {
    SimConfig.compile(new BusyDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      def run(cycles: Int): Unit = {
        dut.io.start #= true
        dut.io.cycles #= cycles
        dut.clockDomain.waitSampling()
        dut.io.start #= false

        // busy asserted for 'cycles' clocks
        for (i <- 0 until cycles) {
          assert(dut.io.busy.toBoolean)
          dut.clockDomain.waitSampling()
        }
        // one extra cycle to observe clear
        assert(!dut.io.busy.toBoolean)
        dut.clockDomain.waitSampling()
      }

      run(2)
      run(1)
    }
  }
}

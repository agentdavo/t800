import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class VcpResetDut extends Component {
  val io = new Bundle {
    val cmd = in Bits(32 bits)
    val state = out UInt(3 bits)
  }
  val linkState = RegInit(U"001")
  val resetCounter = Reg(UInt(8 bits)) init(0)

  when(io.cmd(0)) {
    linkState := U"001"
    resetCounter := U(255)
  }
  when(resetCounter =/= 0) {
    linkState := U"001"
    resetCounter := resetCounter - 1
  }
  when(io.cmd(1) && linkState === U"001" && resetCounter === 0) {
    linkState := U"010"
  }
  io.state := linkState
}

class VcpParityDut extends Component {
  val io = new Bundle {
    val start = in Bool()
    val end = in Bool()
    val isEnd = in Bool()
    val parity = out Bool()
  }
  val tokenType = Reg(Bits(2 bits)) init(B"00")
  val parityBit = Reg(Bool()) init(False)

  when(io.start) {
    tokenType := B"00"
    parityBit := (B"0" ## B"00000000").xorR
  }
  when(io.end) {
    val nextType = Mux(io.isEnd, B"10", B"01")
    tokenType := nextType
    parityBit := (B"1" ## nextType).xorR
  }
  io.parity := parityBit
}

class VcpPluginSpec extends AnyFunSuite {
  test("reset held for 256 cycles") {
    SimConfig.compile(new VcpResetDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.cmd #= B"1"
      dut.clockDomain.waitSampling()
      dut.io.cmd #= 0
      for(_ <- 0 until 255) {
        assert(dut.io.state.toBigInt == 1)
        dut.clockDomain.waitSampling()
      }
      dut.io.cmd #= B"10"
      dut.clockDomain.waitSampling()
      assert(dut.io.state.toBigInt == 2)
    }
  }

  test("parity generation") {
    SimConfig.compile(new VcpParityDut).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.start #= true
      dut.io.end #= false
      dut.io.isEnd #= false
      dut.clockDomain.waitSampling()
      assert(dut.io.parity.toBoolean == (B"0" ## B"00000000").xorR.toBoolean)
      dut.io.start #= false
      dut.io.end #= true
      dut.io.isEnd #= true
      dut.clockDomain.waitSampling()
      assert(dut.io.parity.toBoolean == (B"1" ## B"10").xorR.toBoolean)
    }
  }
}

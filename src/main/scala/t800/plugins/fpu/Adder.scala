package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import Utils._

/** Very small double‑precision adder/subtractor. The implementation only supports normalised
  * IEEE‑754 numbers and ignores corner cases such as NaNs or denormals which are handled separately
  * by the [[FpuVCU]].
  *
  * The design is split in two pipeline stages:
  *   1. Decode and align the operands 2. Perform the addition/subtraction and normalise the result
  */
object Adder {
  case class Cmd() extends Bundle {
    val a = Bits(64 bits)
    val b = Bits(64 bits)
    val sub = Bool()
    val rounding = Bits(2 bits)
  }
}

class FpuAdder extends Component {
  import Adder._

  val io = new Bundle {
    val cmd = slave Stream (Cmd())
    val rsp = master Stream (Bits(64 bits))
  }

  // --------------------------------------------------------------------------
  // Pipeline definition
  // --------------------------------------------------------------------------
  private val pipe = new StageCtrlPipeline
  private val s0 = pipe.ctrl(0)
  private val s1 = pipe.ctrl(1)

  // Payloads carried between the two stages
  val SA = Payload(Bool())
  val SB = Payload(Bool())
  val SUB = Payload(Bool())
  val A = Payload(Bits(64 bits))
  val B = Payload(Bits(64 bits))
  val ROUND = Payload(Bits(2 bits))
  val EA = Payload(UInt(11 bits))
  val EB = Payload(UInt(11 bits))
  val MA_RAW = Payload(UInt(53 bits))
  val MB_RAW = Payload(UInt(53 bits))
  val MA = Payload(UInt(53 bits))
  val MB = Payload(UInt(53 bits))
  val EXP = Payload(UInt(11 bits))
  val RESULT = Payload(Bits(64 bits))

  // --------------------------------------------------------------------------
  // Stage 0 : decode and exponent alignment
  // --------------------------------------------------------------------------
  s0.up.driveFrom(io.cmd) { (self, p) =>
    self(A) := p.a
    self(B) := p.b
    self(SUB) := p.sub
    self(ROUND) := p.rounding
  }

  new s0.Area {
    val opa = parseIeee754(s0(A))
    val opb = parseIeee754(s0(B))

    val mantA =
      ((Mux(opa.exponent === 0, U(0, 1 bits), U(1, 1 bits)) ## opa.mantissa).asUInt).resize(53)
    val mantB =
      ((Mux(opb.exponent === 0, U(0, 1 bits), U(1, 1 bits)) ## opb.mantissa).asUInt).resize(53)

    s0(SA) := opa.sign
    s0(SB) := opb.sign
    s0(EA) := Mux(opa.exponent === 0, U(1), opa.exponent)
    s0(EB) := Mux(opb.exponent === 0, U(1), opb.exponent)
    s0(MA_RAW) := mantA
    s0(MB_RAW) := mantB
  }

  new s0.Area {
    val diff = (s0(EA) - s0(EB)).asSInt
    val shA = (diff < 0).mux((-diff).asUInt.min(63), U(0))
    val shB = (diff > 0).mux(diff.asUInt.min(63), U(0))

    val expL = Mux(diff >= 0, s0(EA), s0(EB))
    s0(EXP) := expL
    s0(MA) := (s0(MA_RAW) >> shA).resize(53)
    s0(MB) := (s0(MB_RAW) >> shB).resize(53)
  }

  // --------------------------------------------------------------------------
  // Stage 1 : add/subtract and normalise
  // --------------------------------------------------------------------------
  new s1.Area {
    val signA = s1(SA)
    val signB = s1(SB) ^ s1(SUB)
    val mantA = s1(MA).resize(56)
    val mantB = s1(MB).resize(56)
    val exp = s1(EXP).resize(12)

    val opA = signA ? (~mantA + 1) | mantA
    val opB = signB ? (~mantB + 1) | mantB
    val sumS = (opA.asSInt + opB.asSInt).resize(57)

    val signRes = sumS.msb
    val absSum = Mux(signRes, (~sumS + 1).asUInt, sumS.asUInt)

    val overflow = absSum(56)
    val mantAdj = UInt(56 bits)
    when(overflow) {
      mantAdj := (absSum >> 1)
    } otherwise {
      mantAdj := absSum(55 downto 0)
    }
    val expAdj = (exp + overflow.asUInt).resize(12)

    val lz = CountLeadingZeroes(mantAdj.asBits)
    val shift = (lz.asSInt - 3)
    val normMant = UInt(53 bits)
    val normExp = UInt(11 bits)
    when(shift >= 0) {
      normMant := (mantAdj << shift.asUInt).resize(53)
      normExp := (expAdj.asSInt - shift).asUInt.resize(11)
    } otherwise {
      val r = (-shift).asUInt
      normMant := (mantAdj >> r).resize(53)
      normExp := (expAdj.asSInt + r.asSInt).asUInt.resize(11)
    }

    val dropped = (s1(MA_RAW) =/= s1(MA)) || (s1(MB_RAW) =/= s1(MB))
    val rawRes = packIeee754(signRes, normExp, normMant(51 downto 0).asBits)
    val rounded = Bits(64 bits)
    rounded := rawRes
    when(signRes && s1(ROUND) === B"11" && dropped) {
      rounded := (rawRes.asUInt + 1).asBits
    }

    s1(RESULT) := rounded
  }

  s1.down.driveTo(io.rsp) { (p, n) =>
    p := n(RESULT)
  }

  pipe.build()
}

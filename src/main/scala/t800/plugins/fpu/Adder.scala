package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import Utils._

/**
  * Two stage IEEE-754 adder/subtractor used by the FPU plugin.
  * The design mirrors the historic T9000 approach with dual
  * alignment subtractors and speculative overflow correction.
  */
object Adder {
  /** Command payload. */
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
    val cmd = slave Stream(Cmd())
    val rsp = master Stream(Bits(64 bits))
  }

  private val pip = new StageCtrlPipeline
  private val s0 = pip.ctrl(0)
  private val s1 = pip.ctrl(1)

  // Payloads
  val A      = Payload(Bits(64 bits))
  val B      = Payload(Bits(64 bits))
  val SUB    = Payload(Bool())
  val ROUND  = Payload(Bits(2 bits))
  val EXP    = Payload(SInt(12 bits))
  val MA     = Payload(UInt(55 bits))
  val MB     = Payload(UInt(55 bits))
  val SIGN   = Payload(Bool())
  val RESULT = Payload(Bits(64 bits))

  // Stage0 : operand decode and alignment
  s0.driveFrom(io.cmd) { (self, p) =>
    self(A)     := p.a
    self(B)     := p.b
    self(SUB)   := p.sub
    self(ROUND) := p.rounding
  }

  new s0.Area {
    val opa = parseIeee754(s0(A))
    val opb = parseIeee754(s0(B))
    val mantA = ((opa.exponent === 0) ? B"0" | B"1") ## opa.mantissa
    val mantB = ((opb.exponent === 0) ? B"0" | B"1") ## opb.mantissa
    val diffAB = (opa.exponent - opb.exponent).resize(12)
    val diffBA = (opb.exponent - opa.exponent).resize(12)
    val shiftA = diffBA.max(0)
    val shiftB = diffAB.max(0)
    val expMax = Mux(diffAB > 0, opa.exponent, opb.exponent).resize(12)
    s0(MA)   := mantA.asUInt >> shiftA.asUInt
    s0(MB)   := mantB.asUInt >> shiftB.asUInt
    s0(EXP)  := expMax
    s0(SIGN) := opa.sign
  }

  // Stage1 : addition, normalization and rounding
  new s1.Area {
    val addendA = s1(MA)
    val addendB = s1(MB)
    val doSub   = s1(SUB)
    val rawSum  = Mux(doSub, addendA - addendB, addendA + addendB)
    val ovf     = rawSum.msb
    val sumAdj  = Mux(ovf, rawSum >> 1, rawSum)
    val expAdj  = s1(EXP) + ovf.asUInt.asSInt
    val lz      = sumAdj.asBits.leadingZeros()
    val normMan = (sumAdj << lz).resize(53)
    val normExp = expAdj - lz.asSInt
    val rounded = roundIeee754(AFix(normMan, 53 bit, 0 exp), s1(ROUND))
    s1(RESULT)  := packIeee754(s1(SIGN), normExp, rounded.raw(51 downto 0))
  }

  s1.driveTo(io.rsp){ (p,self) => p := self(RESULT) }
  io.rsp.ready := True
  io.cmd.ready := s0.up.ready

  pip.build()
}


package t800.plugins

import spinal.core._
import spinal.lib._

import spinal.lib.misc.pipeline._
import t800.plugins._
import t800.Global

class FpCmd(opBits: Int = 3) extends Bundle {
  val op = Bits(opBits bits)
  val opa = UInt(Global.WORD_BITS() bits)
  val opb = UInt(Global.WORD_BITS() bits)
  def this(name: String, a: UInt, b: UInt) = {
    this()
    name match {
      case "FPADD" => op := B"3'b000"
      case "FPSUB" => op := B"3'b001"
      case "FPMUL" => op := B"3'b010"
      case "FPDIV" => op := B"3'b011"
      case _ => op := B"3'b111"
    }
    opa := a
    opb := b
  }
}

class FpuPlugin extends FiberPlugin {
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null

  override def setup(): Unit = {
    pipeReg = Flow(new FpCmd)
    rspReg = Flow(UInt(Global.WORD_BITS() bits))
    addService(new FpuSrv {
      override def pipe = pipeReg
      override def rsp = rspReg
    })
  }

  override def build(): Unit = {
    val pip = new StagePipeline
    val n0 = pip.node(0)
    val n1 = pip.node(1)

    n0.arbitrateFrom(pipeReg)

    val OP = n0.insert(pipeReg.payload.op)
    val A = n0.insert(pipeReg.payload.opa)
    val B = n0.insert(pipeReg.payload.opb)

    val resultCalc = UInt(Global.WORD_BITS() bits)
    switch(n1(OP)) {
      is(B"000") { resultCalc := (n1(A) + n1(B)).resized }
      is(B"001") { resultCalc := (n1(A) - n1(B)).resized }
      is(B"010") { resultCalc := (n1(A) * n1(B)).resized }
      is(B"011") { resultCalc := (n1(A) / n1(B)).resized }
      default { resultCalc := 0 }
    }
    val RESULT = n1.insert(resultCalc)

    val rsp = n1.toFlow(n => n(RESULT))
    rspReg.valid := rsp.valid
    rspReg.payload := rsp.payload

    pip.build()
  }
}

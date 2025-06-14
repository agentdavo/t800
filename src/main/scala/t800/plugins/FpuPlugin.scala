package t800.plugins

import spinal.core._
import spinal.lib._

import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core.fiber.Retainer
import t800.Global

object FpOp extends SpinalEnum {
  val ADD, SUB, MUL, DIV, INVALID = newElement()
}

class FpCmd() extends Bundle {
  val op = FpOp()
  val opa = UInt(Global.WORD_BITS bits)
  val opb = UInt(Global.WORD_BITS bits)
  def this(name: String, a: UInt, b: UInt) = {
    this()
    name match {
      case "FPADD" => op := FpOp.ADD
      case "FPSUB" => op := FpOp.SUB
      case "FPMUL" => op := FpOp.MUL
      case "FPDIV" => op := FpOp.DIV
      case _ => op := FpOp.INVALID
    }
    opa := a
    opb := b
  }
}

class FpuPlugin extends FiberPlugin {
  val version = "FpuPlugin v0.1"
  private var pipeReg: Flow[FpCmd] = null
  private var rspReg: Flow[UInt] = null
  private val retain = Retainer()

  during setup new Area {
    report(L"Initializing $version")
    println(s"[${FpuPlugin.this.getDisplayName()}] setup start")
    pipeReg = Flow(new FpCmd)
    pipeReg.setIdle()
    rspReg = Flow(UInt(Global.WORD_BITS bits))
    rspReg.setIdle()
    addService(new FpuSrv {
      override def pipe = pipeReg
      override def rsp = rspReg
    })
    retain()
    println(s"[${FpuPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${FpuPlugin.this.getDisplayName()}] build start")
    retain.await()
    val pip = new StagePipeline
    val n0 = pip.node(0)
    val n1 = pip.node(1)

    n0.arbitrateFrom(pipeReg)

    val OP = n0.insert(pipeReg.payload.op)
    val A = n0.insert(pipeReg.payload.opa)
    val B = n0.insert(pipeReg.payload.opb)

    val resultCalc = UInt(Global.WORD_BITS bits)
    switch(n1(OP)) {
      is(FpOp.ADD) { resultCalc := (n1(A) + n1(B)).resized }
      is(FpOp.SUB) { resultCalc := (n1(A) - n1(B)).resized }
      is(FpOp.MUL) { resultCalc := (n1(A) * n1(B)).resized }
      is(FpOp.DIV) { resultCalc := (n1(A) / n1(B)).resized }
      default { resultCalc := 0 }
    }
    val RESULT = n1.insert(resultCalc)

    val rsp = n1.toFlow(n => n(RESULT))
    rspReg.valid := rsp.valid
    rspReg.payload := rsp.payload

    pip.build()
    println(s"[${FpuPlugin.this.getDisplayName()}] build end")
  }
}

package t800.plugins

import spinal.core._
import spinal.lib._

import t800.plugins._
import t800.TConsts

class FpCmd(opBits: Int = 3) extends Bundle {
  val op = Bits(opBits bits)
  val opa = UInt(TConsts.WordBits bits)
  val opb = UInt(TConsts.WordBits bits)
  def this(name: String, a: UInt, b: UInt) = {
    this()
    name match {
      case "FPADD" => op := B"3'b000"
      case "FPSUB" => op := B"3'b001"
      case "FPMUL" => op := B"3'b010"
      case "FPDIV" => op := B"3'b011"
      case _       => op := B"3'b111"
    }
    opa := a
    opb := b
  }
}

class FpuPlugin extends FiberPlugin {
  val pipe = Flow(new FpCmd)
  val rsp = Flow(UInt(TConsts.WordBits bits))

  override def setup(): Unit = {
    pipe.valid := False
    rsp.valid := False
    rsp.payload := U(0)
    addService(new FpuSrv {
      override def pipe = FpuPlugin.this.pipe
      override def rsp = FpuPlugin.this.rsp
    })
  }
}

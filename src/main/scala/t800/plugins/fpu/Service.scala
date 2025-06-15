package t800.plugins.fpu

import spinal.core._
import spinal.lib._
import t800.Global

trait FpuSrv {
  def pipe: Flow[FpCmd]
  def rsp: Flow[UInt]

  /** Issue a new FPU command. */
  def send(op: FpOp.E, a: UInt, b: UInt): Unit = {
    pipe.valid := True
    pipe.payload.op := op
    pipe.payload.opa := a
    pipe.payload.opb := b
  }

  /** Result of the previously issued command. */
  def result: UInt = rsp.payload

  /** True when a result is available. */
  def resultValid: Bool = rsp.valid
}

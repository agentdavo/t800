package t800.plugins

import spinal.core._
import spinal.lib.misc.pipeline._

/** Defines the global CPU pipeline structure and exposes stage handles. */
trait PipelineSrv {
  def fetch: CtrlLink
  def decode: CtrlLink
  def execute: CtrlLink
  def memory: CtrlLink
  def writeBack: CtrlLink
  def INSTR: Payload[Bits]
}

class PipelinePlugin extends FiberPlugin {
  private var pipeline: StageCtrlPipeline = null
  private var fetchReg: CtrlLink = null
  private var decodeReg: CtrlLink = null
  private var executeReg: CtrlLink = null
  private var memoryReg: CtrlLink = null
  private var writeBackReg: CtrlLink = null
  private var instrPayload: Payload[Bits] = null

  override def setup(): Unit = {
    pipeline = new StageCtrlPipeline()
    fetchReg = pipeline.ctrl(0)
    decodeReg = pipeline.ctrl(1)
    executeReg = pipeline.ctrl(2)
    memoryReg = pipeline.ctrl(3)
    writeBackReg = pipeline.ctrl(4)
    instrPayload = Payload(Bits(8 bits))
  }
  def fetch: CtrlLink = fetchReg
  def decode: CtrlLink = decodeReg
  def execute: CtrlLink = executeReg
  def memory: CtrlLink = memoryReg
  def writeBack: CtrlLink = writeBackReg
  def INSTR: Payload[Bits] = instrPayload

  override def build(): Unit = {
    pipeline.build()
    addService(new PipelineSrv {
      override def fetch = fetchReg
      override def decode = decodeReg
      override def execute = executeReg
      override def memory = memoryReg
      override def writeBack = writeBackReg
      override def INSTR = instrPayload
    })
  }
}

package t800.plugins

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core.fiber.Retainer
import t800.Global

/** Defines the global CPU pipeline structure and exposes stage handles. */
trait PipelineSrv {
  def fetch: CtrlLink
  def decode: CtrlLink
  def execute: CtrlLink
  def memory: CtrlLink
  def writeBack: CtrlLink
  def INSTR: Payload[Bits]
  def PC: Payload[UInt]
  def MEM_ADDR: Payload[UInt]
  def MEM_DATA: Payload[Bits]
}

class PipelinePlugin extends FiberPlugin {
  private var pipeline: StageCtrlPipeline = null
  private var fetchReg: CtrlLink = null
  private var decodeReg: CtrlLink = null
  private var executeReg: CtrlLink = null
  private var memoryReg: CtrlLink = null
  private var writeBackReg: CtrlLink = null
  private val retain = Retainer()

  during setup new Area {
    pipeline = new StageCtrlPipeline()
    fetchReg = pipeline.ctrl(0)
    decodeReg = pipeline.ctrl(1)
    executeReg = pipeline.ctrl(2)
    memoryReg = pipeline.ctrl(3)
    writeBackReg = pipeline.ctrl(4)
    // Touch isFiring early so StageCtrlPipeline can drive it during build
    Seq(fetchReg, decodeReg, executeReg, memoryReg, writeBackReg).foreach(_.down.isFiring)
    // Pre-create the global payloads across stages
    Seq(fetchReg, decodeReg, executeReg, memoryReg, writeBackReg).foreach { stage =>
      stage(Global.INSTR)
      stage(Global.PC)
      stage(Global.MEM_ADDR)
      stage(Global.MEM_DATA)
    }
    retain()
  }
  def fetch: CtrlLink = fetchReg
  def decode: CtrlLink = decodeReg
  def execute: CtrlLink = executeReg
  def memory: CtrlLink = memoryReg
  def writeBack: CtrlLink = writeBackReg
  def INSTR: Payload[Bits] = Global.INSTR
  def PC: Payload[UInt] = Global.PC
  def MEM_ADDR: Payload[UInt] = Global.MEM_ADDR
  def MEM_DATA: Payload[Bits] = Global.MEM_DATA

  during build new Area {
    retain.await()
    pipeline.build()
    addService(new PipelineSrv {
      override def fetch = fetchReg
      override def decode = decodeReg
      override def execute = executeReg
      override def memory = memoryReg
      override def writeBack = writeBackReg
      override def INSTR = Global.INSTR
      override def PC = Global.PC
      override def MEM_ADDR = Global.MEM_ADDR
      override def MEM_DATA = Global.MEM_DATA
    })
  }
}

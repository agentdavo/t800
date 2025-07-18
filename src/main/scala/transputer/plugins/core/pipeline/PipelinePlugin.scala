package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin._
import transputer.Global
import transputer.plugins.core.pipeline.PipelineStageService

/** Defines the global CPU pipeline structure and exposes stage handles. */
class PipelinePlugin extends FiberPlugin with PipelineStageService {
  override def getDisplayName(): String = "PipelinePlugin"
  setName("pipeline")
  val version = "PipelinePlugin v0.2"
  val elaborationLock = Retainer()

  // Pipeline hardware - created in build phase
  var pipeline: StageCtrlPipeline = null
  var fetchReg: CtrlLink = null
  var decodeReg: CtrlLink = null
  var executeReg: CtrlLink = null
  var memoryReg: CtrlLink = null
  var writeBackReg: CtrlLink = null

  during setup new Area {
    // Early pipeline setup
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
      stage(Global.OPCODE)
      stage(Global.IPTR)
      stage(Global.MEM_ADDR)
      stage(Global.MEM_DATA)
    }
  }

  during build new Area {
    elaborationLock.await()
    pipeline.build()
  }

  // ========================================
  // PipelineStageService Implementation
  // ========================================

  override def fetch = fetchReg
  override def decode = decodeReg
  override def execute = executeReg
  override def memory = memoryReg
  override def writeBack = writeBackReg
  override def OPCODE = Global.OPCODE
  override def IPTR = Global.IPTR
  override def MEM_ADDR = Global.MEM_ADDR
  override def MEM_DATA = Global.MEM_DATA
}

package transputer.plugins.pipeline

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin._
import transputer.Global
import transputer.plugins.pipeline.PipelineStageService

/** Defines the global CPU pipeline structure and exposes stage handles. */
class PipelinePlugin extends FiberPlugin {
  val version = "PipelinePlugin v0.2"
  private val retain = Retainer()

  private var pipeline: StageCtrlPipeline = null
  private var fetchReg: CtrlLink = null
  private var decodeReg: CtrlLink = null
  private var executeReg: CtrlLink = null
  private var memoryReg: CtrlLink = null
  private var writeBackReg: CtrlLink = null

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    report(L"Initializing $version")
    retain()
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
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    pipeline.build()
    addService(new PipelineStageService {
      override def fetch = fetchReg
      override def decode = decodeReg
      override def execute = executeReg
      override def memory = memoryReg
      override def writeBack = writeBackReg
      override def OPCODE = Global.OPCODE
      override def IPTR = Global.IPTR
      override def MEM_ADDR = Global.MEM_ADDR
      override def MEM_DATA = Global.MEM_DATA
    })
    println(s"[${this.getDisplayName()}] build end")
  }
}

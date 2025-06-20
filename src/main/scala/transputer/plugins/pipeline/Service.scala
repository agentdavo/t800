package transputer.plugins.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

trait PipelineService {
  def getLinks(): Seq[Link]
}

trait PipelineStageService {
  def fetch: CtrlLink
  def decode: CtrlLink
  def execute: CtrlLink
  def memory: CtrlLink
  def writeBack: CtrlLink
  def OPCODE: Payload[Bits]
  def IPTR: Payload[UInt]
  def MEM_ADDR: Payload[UInt]
  def MEM_DATA: Payload[Bits]
}

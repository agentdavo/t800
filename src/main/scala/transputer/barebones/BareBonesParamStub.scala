package transputer

import spinal.lib.misc.plugin.{FiberPlugin, Hostable}
import spinal.core.Area
import scala.collection.mutable.ArrayBuffer

/** Simplified parameters used when building the BareBones generator. */
case class Param() {
  def plugins(hartId: Int = 0): Seq[FiberPlugin] =
    pluginsArea(hartId).plugins.collect { case p: FiberPlugin => p }.toSeq

  def pluginsArea(hartId: Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()
    plugins += new transputer.plugins.transputer.TransputerPlugin

    plugins += new transputer.plugins.pipeline.PipelinePlugin
    plugins += new transputer.plugins.registers.RegFilePlugin
    plugins += new transputer.plugins.fetch.DummyInstrFetchPlugin
    plugins += new transputer.plugins.fetch.FetchPlugin
    plugins += new transputer.plugins.pipeline.PipelineBuilderPlugin
    plugins += new transputer.plugins.grouper.GrouperPlugin
    plugins += new transputer.plugins.decode.PrimaryInstrPlugin
  }
}

package transputer

import spinal.lib.misc.plugin.{FiberPlugin, Hostable}
import spinal.core.Area
import scala.collection.mutable.ArrayBuffer

/** Configuration parameters for Transputer generation. */
case class Param(
  wordWidth: Int = transputer.Global.WordBits,
  linkCount: Int = transputer.Global.LinkCount,
  enableFpu: Boolean = true,
  mainCacheKb: Int = 16,
  wsCacheWords: Int = 32,
  fpuPrecision: Int = 64,
  cacheSize: Int = 4096,
  reportModel: Boolean = false
) {
  import transputer._

  /** Returns a sequence of FiberPlugin instances based on configuration. */
  def plugins(hartId: Int = 0): Seq[FiberPlugin] =
    pluginsArea(hartId).plugins.collect { case p: FiberPlugin => p }.toSeq

  /** Creates an area to manage plugin instances. Ideal T9000-inspired pipeline: systemBus ->
    * FetchPlugin -> GrouperPlugin -> PrimaryInstrPlugin -> SecondaryInstrPlugin
    */
  def pluginsArea(hartId: Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()

    // Core plugins - essential for basic operation
    plugins += new transputer.plugins.core.transputer.TransputerPlugin(
      wordBits = wordWidth,
      linkCount = linkCount
    )

    // Barebones
    plugins += new transputer.plugins.core.pipeline.PipelinePlugin
    plugins += new transputer.plugins.bus.SystemBusPlugin
    plugins += new transputer.plugins.core.regstack.RegStackPlugin

    
    // Instruction fetch and processing pipeline
    plugins += new transputer.plugins.core.fetch.FetchPlugin
    plugins += new transputer.plugins.core.grouper.InstrGrouperPlugin
    
    // Basic instruction execution
    plugins += new transputer.plugins.arithmetic.ArithmeticPlugin
    plugins += new transputer.plugins.general.GeneralPlugin

    // Timers
    plugins += new transputer.plugins.timers.TimerPlugin
    
    // Pipeline builder to connect everything
    plugins += new transputer.plugins.core.pipeline.PipelineBuilderPlugin
    
  }
}

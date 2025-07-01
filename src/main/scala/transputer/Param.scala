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

    // Minimal working plugin set
    plugins += new transputer.plugins.core.transputer.TransputerPlugin(
      wordBits = wordWidth,
      linkCount = linkCount
    )
    plugins += new transputer.plugins.core.regstack.RegStackPlugin
    plugins += new transputer.plugins.bus.SystemBusPlugin
  }
}

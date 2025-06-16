package t800

import spinal.lib.misc.plugin.{FiberPlugin, Hostable}
import spinal.core.Area
import scala.collection.mutable.ArrayBuffer

case class Param(
  wordWidth: Int = t800.Global.WordBits,
  linkCount: Int = t800.Global.LinkCount,
  enableFpu: Boolean = true,
  mainCacheKb: Int = 16,
  wsCacheWords: Int = 32
) {
  import t800.plugins._

  def plugins(hartId: Int = 0): Seq[FiberPlugin] =
    pluginsArea(hartId).plugins.collect { case p: FiberPlugin => p }.toSeq

  def pluginsArea(hartId: Int = 0) = new Area {
    val plugins = ArrayBuffer[Hostable]()
    plugins += new t800.plugins.transputer.TransputerPlugin(
      wordBits = wordWidth,
      linkCount = linkCount
    )
    if (enableFpu)
      plugins += new t800.plugins.fpu.FpuPlugin
  }
}

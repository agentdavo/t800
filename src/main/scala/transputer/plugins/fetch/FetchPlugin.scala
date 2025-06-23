package transputer.plugins.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.MemReadCmd
import transputer.plugins.fetch.InstrFetchService

/** Simplified instruction fetch plugin used by the BareBones build. */
class FetchPlugin extends FiberPlugin {
  setName("fetch")

  private var cmdReg: Flow[MemReadCmd] = null
  private var rspReg: Flow[Bits] = null

  during setup new Area {
    cmdReg = Flow(MemReadCmd())
    cmdReg.setIdle()
    rspReg = Flow(Bits(64 bits))
    rspReg.setIdle()
    val service = new InstrFetchService {
      override def cmd: Flow[MemReadCmd] = cmdReg
      override def rsp: Flow[Bits] = rspReg
    }
    addService(service)
    host.addService(service)
  }

  during build new Area {}
}

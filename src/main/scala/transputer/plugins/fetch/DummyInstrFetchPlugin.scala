package transputer.plugins.fetch

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, MemReadCmd}
import transputer.plugins.fetch.InstrFetchService

/** Dummy provider for [[InstrFetchService]] used by the BareBones build. */
class DummyInstrFetchPlugin extends FiberPlugin {
  private var cmdReg: Flow[MemReadCmd] = null
  private var rspReg: Flow[Bits] = null

  during setup new Area {
    cmdReg = Flow(MemReadCmd())
    cmdReg.setIdle()
    rspReg = Flow(Bits(64 bits))
    rspReg.setIdle()
    addService(new InstrFetchService {
      override def cmd: Flow[MemReadCmd] = cmdReg
      override def rsp: Flow[Bits] = rspReg
    })
  }

  during build new Area {}
}

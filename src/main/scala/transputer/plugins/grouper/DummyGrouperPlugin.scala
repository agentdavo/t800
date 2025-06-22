package transputer.plugins.grouper

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin

/** Lightweight stub for [[GrouperPlugin]] used in minimal builds. */
class DummyGrouperPlugin extends FiberPlugin {
  private var grp: Flow[GroupedInstructions] = null

  during setup new Area {
    grp = Flow(GroupedInstructions())
    grp.setIdle()
    addService(new GroupedInstrService {
      override def groups: Flow[GroupedInstructions] = grp
    })
  }

  during build new Area {}
}

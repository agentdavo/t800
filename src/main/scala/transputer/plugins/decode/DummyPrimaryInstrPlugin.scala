package transputer.plugins.decode

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin

/** Minimal stub for [[PrimaryInstrPlugin]] so lightweight builds compile. */
class DummyPrimaryInstrPlugin extends FiberPlugin {
  during setup new Area {}
  during build new Area {}
}

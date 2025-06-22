package transputer.plugins.execute

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin

/** Minimal stub for [[SecondaryInstrPlugin]] used in lightweight builds. */
class DummySecondaryInstrPlugin extends FiberPlugin {
  // Provide empty setup and build phases so the plugin compiles without logic.
  during setup new Area {}
  during build new Area {}
}

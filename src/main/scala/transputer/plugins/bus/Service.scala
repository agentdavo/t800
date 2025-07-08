package transputer.plugins.bus

import spinal.lib.bus.bmb.Bmb

/** Service for registering bus masters */
trait BusMasterService {
  def addMaster(name: String, master: Bmb): Unit
}

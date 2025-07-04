package transputer.plugins.trap

import spinal.core._

/**
 * Trap Handler Service
 * Handles T9000 trap and exception processing
 */
trait TrapHandlerService {
  def triggerTrap(trapCode: Bits): Unit
  def getTrapHandler(): UInt
  def setTrapHandler(address: UInt): Unit
  def inTrapHandler(): Bool
  def trapHandlerAddr: UInt
}
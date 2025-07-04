package transputer.plugins.config

import spinal.core._

/**
 * Configuration Access Service
 * Provides access to T9000 configuration registers
 */
trait ConfigAccessService {
  def readConfig(address: UInt): Bits
  def writeConfig(address: UInt, data: Bits): Unit
  def getProcessorId(): Bits
  def getMemoryConfig(): Bits
  def read(address: UInt, width: Int): Bits
  def write(address: UInt, data: Bits, width: Int): Unit
}
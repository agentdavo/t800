package transputer.plugins.pmi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import transputer.Global._

/** T9000 Programmable Memory Interface Service Provides access to external memory with configurable
  * timing and protocols
  */
trait PmiService {
  // PMI Control and Configuration
  def pmiControl: Bits // PMI control register
  def pmiStatus: Bits // PMI status register
  def isEnabled: Bool // PMI is enabled
  def isReady: Bool // PMI is ready for transactions

  // Memory Timing Configuration
  def accessTime: UInt // Memory access time cycles
  def setupTime: UInt // Address setup time
  def holdTime: UInt // Data hold time
  def recoveryTime: UInt // Recovery time between accesses

  // Memory Interface Signals
  def memoryBus: Bmb // BMB interface to external memory
  def addressBus: Bits // External address bus
  def dataBus: Bits // External data bus (bidirectional)
  def controlSignals: Bits // Control signals (CS, OE, WE, etc.)

  // DMA Support
  def dmaRequest: Bool // DMA request signal
  def dmaAcknowledge: Bool // DMA acknowledge signal
  def dmaTransferCount: UInt // Remaining DMA transfer count

  // Burst and Cache Support
  def burstLength: UInt // Current burst length
  def cacheLineSize: UInt // Cache line size
  def supportsBurst: Bool // Burst mode supported
  def supportsCache: Bool // Cache coherency supported

  // Error Handling
  def memoryError: Bool // Memory error detected
  def timeoutError: Bool // Access timeout error
  def parityError: Bool // Parity error (if enabled)
  def clearErrors(): Unit // Clear all error flags

  // Configuration Methods
  def configureTiming(access: UInt, setup: UInt, hold: UInt, recovery: UInt): Unit
  def enablePmi(): Unit
  def disablePmi(): Unit
  def resetPmi(): Unit
  def setBurstMode(enabled: Bool, length: UInt): Unit
  def setCacheLineSize(size: UInt): Unit
  def updateRegisters(): Unit
}

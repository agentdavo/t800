package transputer.plugins.pmi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/** T9000 Programmable Memory Interface (PMI) Service
  *
  * Provides high-performance external memory access with configurable timing, DMA support, and
  * burst mode operation per T9000 specification Chapter 10.
  */
trait PmiService {
  // Memory Interface Control
  def enable(): Unit
  def disable(): Unit
  def reset(): Unit
  def isEnabled: Bool
  def isReady: Bool

  // Memory Timing Configuration
  def setTimingParameters(access: UInt, setup: UInt, hold: UInt, recovery: UInt): Unit
  def getAccessTime: UInt
  def getSetupTime: UInt
  def getHoldTime: UInt
  def getRecoveryTime: UInt

  // Burst Mode Support
  def enableBurstMode(length: UInt): Unit
  def disableBurstMode(): Unit
  def getBurstLength: UInt
  def supportsBurst: Bool

  // DMA Controller Interface
  def startDmaTransfer(sourceAddr: UInt, destAddr: UInt, length: UInt): Bool
  def isDmaActive: Bool
  def dmaComplete: Bool
  def getDmaStatus: Bits

  // Memory Bus Interface
  def memoryBus: Bmb
  def addressBus: Bits
  def dataBus: Bits
  def controlSignals: Bits

  // Error Handling
  def hasMemoryError: Bool
  def hasTimeoutError: Bool
  def hasParityError: Bool
  def clearErrors(): Unit

  // Cache Integration
  def setCacheLineSize(size: UInt): Unit
  def getCacheLineSize: UInt
  def supportsCache: Bool

  // Status and Configuration Registers
  def getStatusRegister: Bits
  def getControlRegister: Bits
  def setControlRegister(value: Bits): Unit
}

/** PMI Memory Timing Configuration
  */
case class PmiTimingConfig() extends Bundle {
  val accessTime = UInt(8 bits) // Memory access time in cycles
  val setupTime = UInt(8 bits) // Address setup time
  val holdTime = UInt(8 bits) // Data hold time
  val recoveryTime = UInt(8 bits) // Recovery time between accesses
}

/** PMI DMA Command
  */
case class PmiDmaCmd() extends Bundle {
  val sourceAddr = UInt(32 bits)
  val destAddr = UInt(32 bits)
  val length = UInt(16 bits)
  val burstMode = Bool()
  val direction = Bool() // True = memory to external, False = external to memory
}

/** PMI Status Register Layout (per T9000 spec)
  */
object PmiStatus {
  def ENABLED = 0
  def READY = 1
  def DMA_REQUEST = 2
  def DMA_ACKNOWLEDGE = 3
  def BURST_ENABLED = 4
  def CACHE_ENABLED = 5
  def MEMORY_ERROR = 6
  def TIMEOUT_ERROR = 7
  def PARITY_ERROR = 8
  def PMI_STATE_LSB = 9 // 3-bit state field
  def PMI_STATE_MSB = 11
}

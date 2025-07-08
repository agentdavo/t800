package transputer.plugins.event

import spinal.core._
import spinal.lib._
import transputer.Global._

/** T9000 Event Handling and Interrupt Service Interface Manages event channels, interrupts, and
  * error conditions
  */
trait EventService {
  // Event System Control
  def eventMask: Bits // Event mask register
  def eventStatus: Bits // Event status register
  def eventEnabled: Bool // Event system enabled
  def interruptEnabled: Bool // Interrupt handling enabled

  // Event Channel Management
  def eventChannelReady(channel: Int): Bool
  def eventChannelData(channel: Int): Bits
  def enableEventChannel(channel: Int): Unit
  def disableEventChannel(channel: Int): Unit

  // Interrupt Handling
  def interruptRequest: Bool // Interrupt request signal
  def interruptAcknowledge: Bool // Interrupt acknowledge
  def interruptVector: Bits // Current interrupt vector
  def interruptPriority: UInt // Current interrupt priority

  // Error Event Management
  def errorCode: Bits // Current error code
  def errorAddress: UInt // Address where error occurred
  def errorFlags: Bits // Error flag register
  def clearError(errorType: Int): Unit
  def reportError(errorCode: Bits, address: UInt): Unit

  // Timer Event Support
  def timerExpired(timer: Int): Bool
  def enableTimerEvent(timer: Int): Unit
  def disableTimerEvent(timer: Int): Unit

  // Process Event Support
  def processReady: Bool // Process ready event
  def processTerminated: Bool // Process terminated event
  def enableProcessEvents(): Unit
  def disableProcessEvents(): Unit

  // Event Methods
  def triggerEvent(eventType: Int, data: Bits): Unit
  def waitForEvent(eventMask: Bits): Bool
  def selectEvent(eventChannels: Vec[Bool]): UInt
  def updateRegisters(): Unit
}

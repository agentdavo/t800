package transputer.plugins.clock

import spinal.core._
import spinal.lib._
import transputer.T9000ClockService

/** Clock Domain Service for T9000 Transputer
  *
  * Provides centralized clock domain management for all plugins. This service manages the multiple
  * clock domains required by the T9000:
  *   - System clock (CPU pipeline and core logic)
  *   - Memory clock (PMI and cache controllers)
  *   - Timer clock (1MHz for timers)
  *   - Debug clock (Debug interface)
  *   - DS-Link clocks (Communication interfaces)
  *
  * The service also provides utilities for safe clock domain crossing.
  */
trait ClockDomainService extends T9000ClockService {

  /** Check if all clocks are stable and ready
    */
  def areAllClocksReady: Bool

  /** Get clock status for monitoring
    */
  def getClockStatus: ClockStatus

  /** Enable/disable specific clock domains for power management
    */
  def enableClock(domain: ClockDomainId.E): Unit
  def disableClock(domain: ClockDomainId.E): Unit

  /** Get clock frequency information
    */
  def getClockFrequency(domain: ClockDomainId.E): HertzNumber
}

/** Clock domain identifiers
  */
object ClockDomainId extends SpinalEnum {
  val SYSTEM, MEMORY, TIMER, DEBUG = newElement()
  val DSLINK0, DSLINK1, DSLINK2, DSLINK3 = newElement()
  val SLOW_SYSTEM, VERY_SLOW_SYSTEM = newElement()
}

/** Clock status bundle
  */
case class ClockStatus() extends Bundle {
  val systemClockOk = Bool()
  val memoryClockOk = Bool()
  val timerClockOk = Bool()
  val debugClockOk = Bool()
  val dsLinkClocksOk = Vec(Bool(), 4)
  val allClocksOk = Bool()
}

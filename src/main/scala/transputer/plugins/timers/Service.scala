package transputer.plugins.timers

import spinal.core._

/** T9000 Timer Service with hybrid signal-method architecture.
  *
  * The T9000 implements two timers:
  *   - ClockReg0: Microsecond timer (1MHz resolution)
  *   - ClockReg1: 64-microsecond timer (15.625kHz resolution)
  *
  * This interface uses a hybrid approach:
  *   - Direct signal access for performance-critical timer operations
  *   - Method interfaces for complex timer control and management
  *   - Proper SpinalHDL signal ownership patterns
  */
trait TimerService {
  // ========================================
  // DIRECT SIGNAL ACCESS (for performance-critical operations)
  // ========================================
  // These are mutable signals owned by the TimerPlugin
  // They can be read directly and assigned from other plugins
  def clockReg0: UInt // Direct access to ClockReg0 (microsecond timer)
  def clockReg1: UInt // Direct access to ClockReg1 (64-microsecond timer)
  def clockReg0Enable: Bool // Direct access to ClockReg0 enable control
  def clockReg1Enable: Bool // Direct access to ClockReg1 enable control

  // ========================================
  // METHOD INTERFACES (for complex operations with state management)
  // ========================================

  /** Legacy interface for compatibility */
  def hi: UInt = clockReg0
  def lo: UInt = clockReg1

  /** Set ClockReg0 (microsecond timer) to a specific value */
  def setClockReg0(value: UInt): Unit

  /** Set ClockReg1 (64-microsecond timer) to a specific value */
  def setClockReg1(value: UInt): Unit

  /** Request to set both timers to the same value (legacy interface) */
  def set(value: UInt): Unit = {
    setClockReg0(value)
    setClockReg1(value)
  }

  /** Enable/disable ClockReg0 (microsecond timer) */
  def enableClockReg0(): Unit
  def disableClockReg0(): Unit

  /** Enable/disable ClockReg1 (64-microsecond timer) */
  def enableClockReg1(): Unit
  def disableClockReg1(): Unit

  /** Legacy interface for compatibility */
  def enableHi(): Unit = enableClockReg0()
  def enableLo(): Unit = enableClockReg1()
  def disableHi(): Unit = disableClockReg0()
  def disableLo(): Unit = disableClockReg1()

  /** Check if timers have reached specific values (for process scheduling) */
  def clockReg0After(time: UInt): Bool
  def clockReg1After(time: UInt): Bool

  // Signal update triggers (called when direct signal assignment occurs)
  def updateRegisters(): Unit // Sync signals back to register file
}

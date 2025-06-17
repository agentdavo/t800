package transputer.plugins.timers

import spinal.core._

trait TimerService {
  def hi: UInt
  def lo: UInt

  /** Request to set the timers to a new value. */
  def set(value: UInt): Unit

  /** Resume the high priority counter. */
  def enableHi(): Unit

  /** Resume the low priority counter. */
  def enableLo(): Unit

  /** Halt the high priority counter. */
  def disableHi(): Unit

  /** Halt the low priority counter. */
  def disableLo(): Unit
}

package t800.plugins.timers

import spinal.core._

trait TimerSrv {
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

package t800.plugins.stack

import spinal.core._
import spinal.lib._

/** Service interface for workspace memory access in the T800 pipeline. */
trait StackSrv {
  def read(offset: SInt): UInt // Read from workspace at WdescReg + offset
  def write(offset: SInt, data: UInt): Unit // Write to workspace at WdescReg + offset
}

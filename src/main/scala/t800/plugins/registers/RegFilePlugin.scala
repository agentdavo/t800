package t800.plugins.registers

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import t800.Global

/** Minimal stub register file for unit tests. */
class RegFilePlugin extends FiberPlugin with RegfileSrv {
  val version = "RegFilePlugin v0.5"

  override def writeLatency = 1
  override def readLatency = 1
  override def getPhysicalDepth = 1
  override def rfSpec = T9000RegFileSpec()

  override def read(reg: RegName.C, processId: UInt, shadow: Boolean): Bits =
    B(0, rfSpec.width bits)
  override def write(reg: RegName.C, data: Bits, processId: UInt, shadow: Boolean): Unit = {}
  override def readShadow(reg: RegName.C, processId: UInt): Bits = B(0, rfSpec.width bits)
  override def writeShadow(reg: RegName.C, data: Bits, processId: UInt): Unit = {}
  override def readStatusBit(field: StatusRegBits => Bool, processId: UInt, shadow: Boolean): Bool =
    False
  override def writeStatusBit(
    field: StatusRegBits => Bool,
    value: Bool,
    processId: UInt,
    shadow: Boolean
  ): Unit = {}
  override def copyToShadow(processId: UInt): Unit = {}
  override def restoreFromShadow(processId: UInt): Unit = {}
}

package transputer.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/** Generator for a BMB on-chip RAM or ROM mapped at a fixed address.
  *
  * Usage example:
  * {{
  *   implicit val interconnect = BmbInterconnectGenerator(ClockDomain.current)
  *   val ramA = BmbOnChipRamGenerator(0x80000000L)
  *   ramA.size := 4096
  *   ramA.hexInit := "prog.hex"
  * }}
  */
case class BmbOnChipRamGenerator(address: Handle[BigInt] = Unset)
                                 (implicit interconnect: BmbInterconnectGenerator)
    extends Area {
  val size      = Handle[BigInt]
  var hexOffset = BigInt(0)
  val hexInit   = Handle[String]
  val source    = Handle[BmbAccessCapabilities]
  val requirements = Handle[BmbAccessParameter]
  val ctrl      = Handle(logic.io.bus)

  interconnect.addSlave(
    accessSource       = source,
    accessCapabilities = Handle(BmbOnChipRam.busCapabilities(size, source.dataWidth)),
    accessRequirements = requirements,
    bus                = ctrl,
    mapping            = Handle(SizeMapping(address, BigInt(1) << log2Up(size)))
  )

  val logic = Handle(
    BmbOnChipRam(
      p         = requirements.toBmbParameter(),
      size      = size,
      hexOffset = address.get + hexOffset,
      hexInit   = hexInit
    )
  )

  sexport[BigInt](size, size.toInt)
}

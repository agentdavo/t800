package transputer.plugins.clock

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{T9000ClockDomains, T9000ClockManager}

/** T9000 Clock Domain Service Plugin
  *
  * Provides clock domain management as a service to other plugins. This plugin creates and manages
  * all the clock domains required by the T9000:
  *   - System clock domain (200MHz)
  *   - Memory clock domain (200MHz)
  *   - Timer clock domain (1MHz)
  *   - Debug clock domain (50MHz)
  *   - DS-Link clock domains (200MHz each)
  *
  * The plugin also provides utilities for clock domain crossing and synchronization.
  */
class ClocksPlugin extends FiberPlugin {

  override def getDisplayName(): String = "ClocksPlugin"
  setName("clocks")

  // Clock domains - will be initialized during build
  var systemDomain: ClockDomain = null
  var memoryDomain: ClockDomain = null
  var timerDomain: ClockDomain = null
  var debugDomain: ClockDomain = null
  var dsLinkDomains: Array[ClockDomain] = null
  var slowSystemDomain: ClockDomain = null
  var verySlowSystemDomain: ClockDomain = null

  // Clock status signals
  var clockStatus: ClockStatus = null

  during setup new Area {
    println(s"[${ClocksPlugin.this.getDisplayName()}] Setup starting...")

    // Early initialization of external clock domains
    systemDomain = ClockDomain.external("system")
    memoryDomain = ClockDomain.external("memory")
    timerDomain = ClockDomain.external("timer")
    debugDomain = ClockDomain.external("debug")
    dsLinkDomains = Array.tabulate(4)(i => ClockDomain.external(s"dslink$i"))

    println(s"[${ClocksPlugin.this.getDisplayName()}] Setup complete")
  }

  during build new Area {
    println(s"[${ClocksPlugin.this.getDisplayName()}] Build starting...")

    // Create proper clock domains with configuration
    systemDomain = T9000ClockDomains.systemClockDomain()
    memoryDomain = T9000ClockDomains.memoryClockDomain()
    timerDomain = T9000ClockDomains.timerClockDomain()
    debugDomain = T9000ClockDomains.debugClockDomain()
    dsLinkDomains = Array.tabulate(4)(i => T9000ClockDomains.dsLinkClockDomain(i))

    // Derived clock domains
    slowSystemDomain = systemDomain.newClockDomainSlowedBy(2)
    verySlowSystemDomain = systemDomain.newClockDomainSlowedBy(8)

    // Clock status monitoring
    val clockStatusArea = systemDomain {
      new Area {
        clockStatus = ClockStatus()

        // Initialize clock status (simplified - in real implementation would monitor actual clocks)
        clockStatus.systemClockOk := RegInit(True)
        clockStatus.memoryClockOk := RegInit(True)
        clockStatus.timerClockOk := RegInit(True)
        clockStatus.debugClockOk := RegInit(True)

        for (i <- 0 until 4) {
          clockStatus.dsLinkClocksOk(i) := RegInit(True)
        }

        clockStatus.allClocksOk := clockStatus.systemClockOk &&
          clockStatus.memoryClockOk &&
          clockStatus.timerClockOk &&
          clockStatus.debugClockOk &&
          clockStatus.dsLinkClocksOk.reduce(_ && _)
      }
    }

    // Register clock domain service
    addService(new ClockDomainService {
      override def getSystemDomain: ClockDomain = systemDomain
      override def getMemoryDomain: ClockDomain = memoryDomain
      override def getTimerDomain: ClockDomain = timerDomain
      override def getDebugDomain: ClockDomain = debugDomain
      override def getDsLinkDomain(linkId: Int): ClockDomain = {
        require(linkId >= 0 && linkId < 4, s"DS-Link ID must be 0-3, got $linkId")
        dsLinkDomains(linkId)
      }
      override def getSlowSystemDomain: ClockDomain = slowSystemDomain
      override def getVerySlowSystemDomain: ClockDomain = verySlowSystemDomain

      override def createDataCrossing[T <: Data](
        dataType: HardType[T],
        from: ClockDomain,
        to: ClockDomain
      ): StreamCCByToggle[T] = {
        T9000ClockDomains.CrossDomain.createCrossing(dataType, from, to)
      }

      override def synchronizePulse(
        pulse: Bool,
        from: ClockDomain,
        to: ClockDomain
      ): Bool = {
        T9000ClockDomains.CrossDomain.pulseSynchronizer(from, to, pulse)
      }

      override def areAllClocksReady: Bool = clockStatus.allClocksOk

      override def getClockStatus: ClockStatus = clockStatus

      override def enableClock(domain: ClockDomainId.E): Unit = {
        // Clock enable logic would go here
        // For now, this is a placeholder
        domain match {
          case ClockDomainId.SYSTEM => // System clock always enabled
          case ClockDomainId.MEMORY => // Memory clock enable control
          case ClockDomainId.TIMER => // Timer clock enable control
          case ClockDomainId.DEBUG => // Debug clock enable control
          case ClockDomainId.DSLINK0 | ClockDomainId.DSLINK1 | ClockDomainId.DSLINK2 |
              ClockDomainId.DSLINK3 => // DS-Link clock control
          case ClockDomainId.SLOW_SYSTEM | ClockDomainId.VERY_SLOW_SYSTEM => // Derived clocks
        }
      }

      override def disableClock(domain: ClockDomainId.E): Unit = {
        // Clock disable logic would go here
        // For now, this is a placeholder
      }

      override def getClockFrequency(domain: ClockDomainId.E): HertzNumber = {
        domain match {
          case ClockDomainId.SYSTEM => T9000ClockDomains.Frequencies.SYSTEM_CLOCK
          case ClockDomainId.MEMORY => T9000ClockDomains.Frequencies.MEMORY_CLOCK
          case ClockDomainId.TIMER => T9000ClockDomains.Frequencies.TIMER_CLOCK
          case ClockDomainId.DEBUG => T9000ClockDomains.Frequencies.DEBUG_CLOCK
          case ClockDomainId.DSLINK0 | ClockDomainId.DSLINK1 | ClockDomainId.DSLINK2 |
              ClockDomainId.DSLINK3 =>
            T9000ClockDomains.Frequencies.DSLINK_CLOCK
          case ClockDomainId.SLOW_SYSTEM => T9000ClockDomains.Frequencies.SYSTEM_CLOCK / 2
          case ClockDomainId.VERY_SLOW_SYSTEM => T9000ClockDomains.Frequencies.SYSTEM_CLOCK / 8
        }
      }
    })

    println(s"[${ClocksPlugin.this.getDisplayName()}] Build complete")
  }

  /** Helper method to create a simple clock domain crossing for control signals
    */
  def createControlCrossing(
    from: ClockDomain,
    to: ClockDomain,
    width: Int = 32
  ): (StreamCCByToggle[Bits], StreamCCByToggle[Bits]) = {
    val forward = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(width bits),
      pushDomain = from,
      popDomain = to,
      depth = 4
    )

    val backward = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(width bits),
      pushDomain = to,
      popDomain = from,
      depth = 4
    )

    (forward, backward)
  }

  /** Helper method to synchronize reset signals across domains
    */
  def synchronizeReset(from: ClockDomain, to: ClockDomain, name: String): Bool = {
    T9000ClockDomains.CrossDomain.synchronizeReset(from, to, name)
  }
}

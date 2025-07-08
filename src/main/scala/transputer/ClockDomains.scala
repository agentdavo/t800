package transputer

import spinal.core._
import spinal.lib._

/** T9000 Transputer Clock Domain Architecture
  *
  * Implements the complete T9000 clocking strategy with:
  *   - Primary system clock domain
  *   - Memory interface clock domains
  *   - DS-Link communication clock domains
  *   - Timer clock domains
  *   - Debug clock domain
  */
object T9000ClockDomains {

  // T9000 Standard Clock Frequencies
  object Frequencies {
    val SYSTEM_CLOCK = 200 MHz // Main CPU clock
    val MEMORY_CLOCK = 200 MHz // Memory interface clock
    val DSLINK_CLOCK = 200 MHz // DS-Link communication
    val TIMER_CLOCK = 1 MHz // Timer subsystem
    val DEBUG_CLOCK = 50 MHz // Debug interface
  }

  /** T9000 System Clock Domain Primary clock for CPU pipeline, ALU, FPU, and core logic
    */
  def systemClockDomain(
    clockName: String = "sys_clk",
    resetName: String = "sys_reset",
    frequency: HertzNumber = Frequencies.SYSTEM_CLOCK
  ): ClockDomain = {
    ClockDomain
      .external(
        name = "system",
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = ASYNC,
          resetActiveLevel = HIGH,
          softResetActiveLevel = HIGH,
          clockEnableActiveLevel = HIGH
        ),
        withReset = true,
        withSoftReset = true,
        withClockEnable = false,
        frequency = ClockDomain.FixedFrequency(frequency)
      )
      .renamePulledWires(
        clock = clockName,
        reset = resetName,
        softReset = "sys_soft_reset"
      )
  }

  /** Memory Interface Clock Domain For PMI, cache controllers, and external memory
    */
  def memoryClockDomain(
    clockName: String = "mem_clk",
    resetName: String = "mem_reset",
    frequency: HertzNumber = Frequencies.MEMORY_CLOCK
  ): ClockDomain = {
    ClockDomain
      .external(
        name = "memory",
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = ASYNC,
          resetActiveLevel = HIGH,
          softResetActiveLevel = HIGH,
          clockEnableActiveLevel = HIGH
        ),
        withReset = true,
        withSoftReset = false,
        withClockEnable = true, // For power management
        frequency = ClockDomain.FixedFrequency(frequency)
      )
      .renamePulledWires(
        clock = clockName,
        reset = resetName,
        enable = "mem_clk_en"
      )
  }

  /** DS-Link Communication Clock Domain For VCP and physical link interfaces
    */
  def dsLinkClockDomain(
    linkId: Int,
    frequency: HertzNumber = Frequencies.DSLINK_CLOCK
  ): ClockDomain = {
    ClockDomain
      .external(
        name = s"dslink$linkId",
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = SYNC, // Synchronous reset for communication
          resetActiveLevel = HIGH,
          softResetActiveLevel = HIGH,
          clockEnableActiveLevel = HIGH
        ),
        withReset = true,
        withSoftReset = true,
        withClockEnable = true,
        frequency = ClockDomain.FixedFrequency(frequency)
      )
      .renamePulledWires(
        clock = s"dslink${linkId}_clk",
        reset = s"dslink${linkId}_reset",
        softReset = s"dslink${linkId}_soft_reset",
        enable = s"dslink${linkId}_clk_en"
      )
  }

  /** Timer Clock Domain For T9000 dual timer system (1μs and 64μs timers)
    */
  def timerClockDomain(
    frequency: HertzNumber = Frequencies.TIMER_CLOCK
  ): ClockDomain = {
    ClockDomain
      .external(
        name = "timer",
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = ASYNC,
          resetActiveLevel = HIGH,
          softResetActiveLevel = HIGH,
          clockEnableActiveLevel = HIGH
        ),
        withReset = true,
        withSoftReset = true,
        withClockEnable = false,
        frequency = ClockDomain.FixedFrequency(frequency)
      )
      .renamePulledWires(
        clock = "timer_clk",
        reset = "timer_reset",
        softReset = "timer_soft_reset"
      )
  }

  /** Debug Clock Domain For debug interface and monitoring
    */
  def debugClockDomain(
    frequency: HertzNumber = Frequencies.DEBUG_CLOCK
  ): ClockDomain = {
    ClockDomain
      .external(
        name = "debug",
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = ASYNC,
          resetActiveLevel = HIGH,
          softResetActiveLevel = HIGH,
          clockEnableActiveLevel = HIGH
        ),
        withReset = true,
        withSoftReset = false,
        withClockEnable = true,
        frequency = ClockDomain.FixedFrequency(frequency)
      )
      .renamePulledWires(
        clock = "debug_clk",
        reset = "debug_reset",
        enable = "debug_clk_en"
      )
  }

  /** Create derived clock domain from system clock Used for slower subsystems
    */
  def createDerivedDomain(
    baseDomain: ClockDomain,
    divisionFactor: BigInt,
    name: String
  ): ClockDomain = {
    baseDomain
      .newClockDomainSlowedBy(divisionFactor)
      .copy(clockEnable = null) // Remove clock enable from derived domain
  }

  /** Cross-clock domain utilities
    */
  object CrossDomain {

    /** Create synchronized reset across domains
      */
    def synchronizeReset(
      sourceDomain: ClockDomain,
      targetDomain: ClockDomain,
      resetName: String = "sync_reset"
    ): Bool = targetDomain {
      val resetSync = BufferCC(
        input = sourceDomain.isResetActive,
        init = True,
        bufferDepth = 2
      )
      resetSync.setName(resetName)
      resetSync
    }

    /** Create clock domain crossing for data
      */
    def createCrossing[T <: Data](
      dataType: HardType[T],
      pushDomain: ClockDomain,
      popDomain: ClockDomain,
      depth: Int = 4
    ): StreamCCByToggle[T] = {
      StreamCCByToggle(
        dataType(),
        pushDomain,
        popDomain
      )
    }

    /** Pulse synchronizer between clock domains
      */
    def pulseSynchronizer(
      sourceDomain: ClockDomain,
      targetDomain: ClockDomain,
      pulse: Bool
    ): Bool = {
      val toggle = sourceDomain {
        val toggleReg = Reg(Bool()) init False
        when(pulse) {
          toggleReg := !toggleReg
        }
        toggleReg
      }

      targetDomain {
        val sync = BufferCC(toggle, bufferDepth = 2)
        val delayed = RegNext(sync) init False
        sync ^ delayed
      }
    }
  }
}

/** T9000 Clock Domain Manager Manages all clock domains in the T9000 system
  */
class T9000ClockManager extends Component {

  // Primary clock domains
  val systemDomain = T9000ClockDomains.systemClockDomain()
  val memoryDomain = T9000ClockDomains.memoryClockDomain()
  val timerDomain = T9000ClockDomains.timerClockDomain()
  val debugDomain = T9000ClockDomains.debugClockDomain()

  // DS-Link domains (4 links)
  val dsLinkDomains = Array.tabulate(4)(i => T9000ClockDomains.dsLinkClockDomain(i))

  // Derived domains for slower subsystems
  val slowSystemDomain = systemDomain.newClockDomainSlowedBy(2) // 250 MHz
  val verySlowSystemDomain = systemDomain.newClockDomainSlowedBy(8) // 62.5 MHz

  // Clock domain status and control
  val clockStatus = systemDomain {
    new Area {
      val systemClockOk = RegInit(False)
      val memoryClockOk = RegInit(False)
      val timerClockOk = RegInit(False)
      val debugClockOk = RegInit(False)
      val dsLinkClocksOk = Vec(Reg(Bool()) init False, 4)

      // Simple clock detection (placeholder)
      systemClockOk := True
      memoryClockOk := True
      timerClockOk := True
      debugClockOk := True
      dsLinkClocksOk.foreach(_ := True)

      // Global status
      val allClocksOk = systemClockOk && memoryClockOk && timerClockOk &&
        debugClockOk && dsLinkClocksOk.reduce(_ && _)
    }
  }

  // Reset management
  val resetManager = systemDomain {
    new Area {
      // Power-on reset counter
      val porCounter = RegInit(U"16'hFFFF")
      val porActive = porCounter =/= 0
      when(porActive) {
        porCounter := porCounter - 1
      }

      // Global reset signal
      val globalReset = porActive || !clockStatus.allClocksOk

      // Synchronized resets for each domain
      val memoryReset = T9000ClockDomains.CrossDomain.synchronizeReset(
        systemDomain,
        memoryDomain,
        "mem_sync_reset"
      )

      val timerReset = T9000ClockDomains.CrossDomain.synchronizeReset(
        systemDomain,
        timerDomain,
        "timer_sync_reset"
      )

      val debugReset = T9000ClockDomains.CrossDomain.synchronizeReset(
        systemDomain,
        debugDomain,
        "debug_sync_reset"
      )

      val dsLinkResets = dsLinkDomains.zipWithIndex.map { case (domain, i) =>
        T9000ClockDomains.CrossDomain.synchronizeReset(
          systemDomain,
          domain,
          s"dslink${i}_sync_reset"
        )
      }
    }
  }

  // Clock domain crossing infrastructure
  val crossings = new Area {
    // System to Memory domain crossing
    val sysToMemDataCrossing = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(32 bits),
      pushDomain = systemDomain,
      popDomain = memoryDomain,
      depth = 4
    )

    // Memory to System domain crossing
    val memToSysDataCrossing = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(32 bits),
      pushDomain = memoryDomain,
      popDomain = systemDomain,
      depth = 4
    )

    // System to DS-Link crossings
    val sysToDsLinkCrossings = dsLinkDomains.map { domain =>
      T9000ClockDomains.CrossDomain.createCrossing(
        dataType = Bits(32 bits),
        pushDomain = systemDomain,
        popDomain = domain,
        depth = 2
      )
    }

    // DS-Link to System crossings
    val dsLinkToSysCrossings = dsLinkDomains.map { domain =>
      T9000ClockDomains.CrossDomain.createCrossing(
        dataType = Bits(32 bits),
        pushDomain = domain,
        popDomain = systemDomain,
        depth = 2
      )
    }

    // Timer to System pulse synchronizers
    val timerInterruptSync = T9000ClockDomains.CrossDomain.pulseSynchronizer(
      timerDomain,
      systemDomain,
      False // Will be connected to actual timer interrupt
    )
  }

  // Clock domain information for debug
  val clockInfo = systemDomain {
    new Area {
      val systemFrequency = systemDomain.frequency.getValue.toBigDecimal
      val memoryFrequency = memoryDomain.frequency.getValue.toBigDecimal
      val timerFrequency = timerDomain.frequency.getValue.toBigDecimal
      val debugFrequency = debugDomain.frequency.getValue.toBigDecimal

      // Frequency ratios
      val memoryToSystemRatio = systemFrequency / memoryFrequency
      val timerToSystemRatio = systemFrequency / timerFrequency
    }
  }
}

/** Clock Domain Service for T9000 Plugins Provides standardized access to clock domains
  */
trait T9000ClockService {
  def getSystemDomain: ClockDomain
  def getMemoryDomain: ClockDomain
  def getTimerDomain: ClockDomain
  def getDebugDomain: ClockDomain
  def getDsLinkDomain(linkId: Int): ClockDomain
  def getSlowSystemDomain: ClockDomain
  def getVerySlowSystemDomain: ClockDomain

  // Clock domain crossing utilities
  def createDataCrossing[T <: Data](
    dataType: HardType[T],
    from: ClockDomain,
    to: ClockDomain
  ): StreamCCByToggle[T]

  def synchronizePulse(
    pulse: Bool,
    from: ClockDomain,
    to: ClockDomain
  ): Bool
}

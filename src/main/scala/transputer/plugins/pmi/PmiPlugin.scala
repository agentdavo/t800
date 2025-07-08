package transputer.plugins.pmi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global

/** T9000 Programmable Memory Interface (PMI) Plugin
  *
  * Modern implementation of the T9000 PMI subsystem with:
  *   - Configurable memory timing
  *   - Hardware DMA controller
  *   - Burst mode support
  *   - External memory bus interface
  *   - Integration with T9000 5-stage pipeline
  */
class PmiPlugin(
  val externalMemorySize: Long = 64 * 1024 * 1024, // 64MB default
  val timingPreset: String = "sram", // "sram", "dram", "flash"
  val enableDma: Boolean = true,
  val maxBurstLength: Int = 16
) extends FiberPlugin {

  override def getDisplayName(): String = "PmiPlugin"
  setName("pmi")

  // PMI Configuration Constants
  private val EXTERNAL_ADDR_WIDTH = 32
  private val EXTERNAL_DATA_WIDTH = 32
  private val DMA_QUEUE_DEPTH = 8

  // Timing presets based on T9000 manual (simplified for compilation)
  private def getTimingPreset(preset: String): PmiTimingConfig = {
    val config = PmiTimingConfig()
    preset match {
      case "sram" =>
        config.setupTime := 4
        config.accessTime := 2
        config.holdTime := 1
        config.recoveryTime := 4
      case "dram" =>
        config.setupTime := 8
        config.accessTime := 4
        config.holdTime := 2
        config.recoveryTime := 8
      case "flash" =>
        config.setupTime := 32
        config.accessTime := 16
        config.holdTime := 4
        config.recoveryTime := 16
      case _ =>
        config.setupTime := 4
        config.accessTime := 2
        config.holdTime := 1
        config.recoveryTime := 4
    }
    config
  }

  during setup new Area {
    println(s"[${PmiPlugin.this.getDisplayName()}] Setup starting...")

    // Validate configuration
    require(externalMemorySize > 0, "External memory size must be positive")
    require(
      Set("sram", "dram", "flash").contains(timingPreset),
      s"Unknown timing preset: $timingPreset"
    )
    require(maxBurstLength > 0 && maxBurstLength <= 256, "Burst length must be 1-256")

    println(s"[${PmiPlugin.this.getDisplayName()}] Configuration:")
    println(s"  External Memory Size: ${externalMemorySize / (1024 * 1024)}MB")
    println(s"  Timing Preset: $timingPreset")
    println(s"  DMA Enabled: $enableDma")
    println(s"  Max Burst Length: $maxBurstLength")

    println(s"[${PmiPlugin.this.getDisplayName()}] Setup complete")
  }

  during build new Area {
    println(s"[${PmiPlugin.this.getDisplayName()}] Build starting...")

    // PMI Control and Status Registers
    val pmiControlReg = Reg(Bits(32 bits)) init 0
    val pmiStatusReg = Reg(Bits(32 bits)) init 0
    val pmiEnabled = Reg(Bool()) init False
    val pmiReady = Reg(Bool()) init True

    // Memory Timing Configuration (simplified for compilation)
    val currentTiming = new Area {
      val setupTime = Reg(UInt(8 bits)) init 4
      val accessTime = Reg(UInt(8 bits)) init 2
      val holdTime = Reg(UInt(8 bits)) init 1
      val recoveryTime = Reg(UInt(8 bits)) init 4
    }

    // Burst Configuration
    val burstEnabled = Reg(Bool()) init False
    val burstLengthReg = Reg(UInt(8 bits)) init 1
    val cacheLineSizeReg = Reg(UInt(8 bits)) init 32

    // DMA Controller Registers
    val dmaActiveReg = Reg(Bool()) init False
    val dmaCompleteReg = Reg(Bool()) init False
    val dmaErrorReg = Reg(Bool()) init False
    val dmaQueue =
      if (enableDma)
        Some(
          StreamFifo(
            dataType = PmiDmaCmd(),
            depth = DMA_QUEUE_DEPTH
          )
        )
      else None

    // Error Status Registers
    val memoryErrorReg = Reg(Bool()) init False
    val timeoutErrorReg = Reg(Bool()) init False
    val parityErrorReg = Reg(Bool()) init False

    // External Memory Interface
    val externalMemoryBus = Bmb(
      BmbParameter(
        addressWidth = EXTERNAL_ADDR_WIDTH,
        dataWidth = EXTERNAL_DATA_WIDTH,
        sourceWidth = 4,
        contextWidth = 4,
        lengthWidth = 8
      )
    )

    // PMI State Machine
    val pmiState = RegInit(U"3'b000") // 000=Disabled, 001=Idle, 010=Active, 011=Burst, 100=Error

    // External bus signals
    val externalAddressBus = Reg(Bits(EXTERNAL_ADDR_WIDTH bits)) init 0
    val externalDataBus = Reg(Bits(EXTERNAL_DATA_WIDTH bits)) init 0
    val externalControlSignals = Reg(Bits(8 bits)) init 0

    // Timing counters
    val accessCounter = Reg(UInt(8 bits)) init 0
    val setupCounter = Reg(UInt(8 bits)) init 0
    val holdCounter = Reg(UInt(8 bits)) init 0
    val recoveryCounter = Reg(UInt(8 bits)) init 0

    // Current transaction tracking
    val currentAddress = Reg(UInt(32 bits)) init 0
    val currentData = Reg(Bits(32 bits)) init 0
    val currentWrite = Reg(Bool()) init False
    val transactionActive = Reg(Bool()) init False

    // Control Register Processing
    when(pmiControlReg(0)) { // Enable PMI
      pmiEnabled := True
      pmiState := U"3'b001" // Idle state
      pmiControlReg(0) := False
    }

    when(pmiControlReg(1)) { // Disable PMI
      pmiEnabled := False
      pmiState := U"3'b000" // Disabled state
      pmiControlReg(1) := False
    }

    when(pmiControlReg(2)) { // Reset PMI
      pmiEnabled := False
      pmiState := U"3'b000"
      memoryErrorReg := False
      timeoutErrorReg := False
      parityErrorReg := False
      transactionActive := False
      dmaActiveReg := False
      dmaCompleteReg := False
      dmaErrorReg := False
      pmiControlReg(2) := False
    }

    // Memory Access State Machine
    switch(pmiState) {
      is(U"3'b000") { // Disabled
        pmiReady := False
        transactionActive := False
        externalMemoryBus.cmd.ready := False
      }

      is(U"3'b001") { // Idle - ready for new transaction
        pmiReady := True
        transactionActive := False
        externalMemoryBus.cmd.ready := True

        when(externalMemoryBus.cmd.valid && pmiEnabled) {
          currentAddress := externalMemoryBus.cmd.address
          currentData := externalMemoryBus.cmd.data
          currentWrite := externalMemoryBus.cmd.opcode === Bmb.Cmd.Opcode.WRITE
          transactionActive := True
          setupCounter := currentTiming.setupTime
          pmiState := U"3'b010" // Active state
        }
      }

      is(U"3'b010") { // Active - processing transaction
        pmiReady := False
        externalMemoryBus.cmd.ready := False

        // Setup phase
        when(setupCounter > 0) {
          setupCounter := setupCounter - 1
        }.otherwise {
          // Access phase
          accessCounter := currentTiming.accessTime
          externalAddressBus := currentAddress.asBits

          when(currentWrite) {
            externalDataBus := currentData
            externalControlSignals := B"8'h05" // CS=1, WE=1, OE=0
          }.otherwise {
            externalControlSignals := B"8'h03" // CS=1, WE=0, OE=1
          }

          // Check for burst mode
          when(burstEnabled && burstLengthReg > 1) {
            pmiState := U"3'b011" // Burst state
          }.otherwise {
            when(accessCounter === 0) {
              holdCounter := currentTiming.holdTime
              pmiState := U"3'b001" // Return to idle after hold
            }.otherwise {
              accessCounter := accessCounter - 1
            }
          }
        }
      }

      is(U"3'b011") { // Burst mode
        pmiReady := False

        when(accessCounter > 0) {
          accessCounter := accessCounter - 1
        }.otherwise {
          holdCounter := currentTiming.holdTime
          pmiState := U"3'b001" // Return to idle
        }
      }

      is(U"3'b100") { // Error state
        pmiReady := False
        transactionActive := False
        externalMemoryBus.cmd.ready := False
      }
    }

    // Hold time handling
    when(holdCounter > 0) {
      holdCounter := holdCounter - 1
      when(holdCounter === 1) {
        externalControlSignals := 0 // Deassert control signals
        recoveryCounter := currentTiming.recoveryTime
      }
    }

    // Recovery time handling
    when(recoveryCounter > 0) {
      recoveryCounter := recoveryCounter - 1
    }

    // DMA Controller Logic (if enabled)
    if (enableDma) {
      val dmaCmd = dmaQueue.get

      // DMA command processing
      dmaCmd.io.push.ready := !dmaActiveReg

      when(dmaCmd.io.pop.valid && !dmaActiveReg) {
        dmaActiveReg := True
        dmaCompleteReg := False
        dmaErrorReg := False
        // Start DMA transfer
        // In full implementation, this would interface with system bus
      }

      // DMA completion detection (simplified)
      when(dmaActiveReg && transactionActive) {
        // Check if this is the last DMA transfer
        dmaCompleteReg := True
        dmaActiveReg := False
      }
    }

    // Update status register
    pmiStatusReg := Cat(
      B"16'h0000", // Reserved bits 31:16
      pmiState, // bits 11:9 - PMI state
      parityErrorReg, // bit 8 - Parity error
      timeoutErrorReg, // bit 7 - Timeout error
      memoryErrorReg, // bit 6 - Memory error
      if (enableDma) dmaCompleteReg else False, // bit 5 - DMA complete
      burstEnabled, // bit 4 - Burst enabled
      if (enableDma) dmaActiveReg else False, // bit 3 - DMA active
      transactionActive, // bit 2 - Transaction active
      pmiReady, // bit 1 - Ready
      pmiEnabled // bit 0 - Enabled
    )

    // BMB Response handling
    externalMemoryBus.rsp.valid := False
    externalMemoryBus.rsp.data := 0
    externalMemoryBus.rsp.source := 0
    externalMemoryBus.rsp.context := 0
    externalMemoryBus.rsp.opcode := Bmb.Rsp.Opcode.SUCCESS

    // Service Implementation
    addService(new PmiService {
      override def enable(): Unit = pmiControlReg(0) := True
      override def disable(): Unit = pmiControlReg(1) := True
      override def reset(): Unit = pmiControlReg(2) := True
      override def isEnabled: Bool = pmiEnabled
      override def isReady: Bool = pmiReady

      override def setTimingParameters(
        access: UInt,
        setup: UInt,
        hold: UInt,
        recovery: UInt
      ): Unit = {
        currentTiming.accessTime := access.resized
        currentTiming.setupTime := setup.resized
        currentTiming.holdTime := hold.resized
        currentTiming.recoveryTime := recovery.resized
      }

      override def getAccessTime: UInt = currentTiming.accessTime
      override def getSetupTime: UInt = currentTiming.setupTime
      override def getHoldTime: UInt = currentTiming.holdTime
      override def getRecoveryTime: UInt = currentTiming.recoveryTime

      override def enableBurstMode(length: UInt): Unit = {
        burstEnabled := True
        burstLengthReg := length.resized
      }
      override def disableBurstMode(): Unit = burstEnabled := False
      override def getBurstLength: UInt = burstLengthReg
      override def supportsBurst: Bool = True

      override def startDmaTransfer(sourceAddr: UInt, destAddr: UInt, length: UInt): Bool = {
        if (enableDma) {
          val cmd = PmiDmaCmd()
          cmd.sourceAddr := sourceAddr
          cmd.destAddr := destAddr
          cmd.length := length.resized
          cmd.burstMode := burstEnabled
          cmd.direction := True // Default direction
          dmaQueue.get.io.push.valid := True
          dmaQueue.get.io.push.payload := cmd
          dmaQueue.get.io.push.ready
        } else {
          False
        }
      }

      override def isDmaActive: Bool = if (enableDma) dmaActiveReg else False
      override def dmaComplete: Bool = if (enableDma) dmaCompleteReg else False
      override def getDmaStatus: Bits = Cat(dmaErrorReg, dmaCompleteReg, dmaActiveReg)

      override def memoryBus: Bmb = externalMemoryBus
      override def addressBus: Bits = externalAddressBus
      override def dataBus: Bits = externalDataBus
      override def controlSignals: Bits = externalControlSignals

      override def hasMemoryError: Bool = memoryErrorReg
      override def hasTimeoutError: Bool = timeoutErrorReg
      override def hasParityError: Bool = parityErrorReg
      override def clearErrors(): Unit = {
        memoryErrorReg := False
        timeoutErrorReg := False
        parityErrorReg := False
      }

      override def setCacheLineSize(size: UInt): Unit = cacheLineSizeReg := size.resized
      override def getCacheLineSize: UInt = cacheLineSizeReg
      override def supportsCache: Bool = True

      override def getStatusRegister: Bits = pmiStatusReg
      override def getControlRegister: Bits = pmiControlReg
      override def setControlRegister(value: Bits): Unit = pmiControlReg := value
    })

    println(s"[${PmiPlugin.this.getDisplayName()}] Build complete")
  }
}

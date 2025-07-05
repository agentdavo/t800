package transputer.plugins.legacy.pmi

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin._
import spinal.lib.bus.bmb._
import transputer.Global._
import transputer.plugins.legacy.pmi.PmiService

/** T9000 Programmable Memory Interface Plugin Provides configurable external memory interface with
  * DMA and burst support Based on T9000 hardware reference manual
  */
class PmiPlugin extends FiberPlugin {
  override def getDisplayName(): String = "PmiPlugin"
  setName("pmi")

  during setup new Area {
    println(s"[${PmiPlugin.this.getDisplayName()}] setup start")

    // PMI Configuration constants
    private val EXTERNAL_ADDR_WIDTH = 32
    private val EXTERNAL_DATA_WIDTH = 32
    private val MAX_BURST_LENGTH = 16
    private val MAX_CACHE_LINE_SIZE = 128
    private val DEFAULT_ACCESS_TIME = 4
    private val DEFAULT_SETUP_TIME = 1
    private val DEFAULT_HOLD_TIME = 1
    private val DEFAULT_RECOVERY_TIME = 1

    // PMI Control and Status Registers
    private val pmiControlReg = Reg(Bits(32 bits)) init 0
    private val pmiStatusReg = Reg(Bits(32 bits)) init 0
    private val pmiEnabled = Reg(Bool()) init False
    private val pmiReady = Reg(Bool()) init True

    // Memory Timing Configuration Registers
    private val accessTimeReg = Reg(UInt(8 bits)) init DEFAULT_ACCESS_TIME
    private val setupTimeReg = Reg(UInt(8 bits)) init DEFAULT_SETUP_TIME
    private val holdTimeReg = Reg(UInt(8 bits)) init DEFAULT_HOLD_TIME
    private val recoveryTimeReg = Reg(UInt(8 bits)) init DEFAULT_RECOVERY_TIME

    // Burst and Cache Configuration
    private val burstLengthReg = Reg(UInt(8 bits)) init 1
    private val cacheLineSizeReg = Reg(UInt(8 bits)) init 32
    private val burstEnabled = Reg(Bool()) init False
    private val cacheEnabled = Reg(Bool()) init False

    // DMA Support Registers
    private val dmaRequestReg = Reg(Bool()) init False
    private val dmaAcknowledgeReg = Reg(Bool()) init False
    private val dmaTransferCountReg = Reg(UInt(16 bits)) init 0

    // Error Status Registers
    private val memoryErrorReg = Reg(Bool()) init False
    private val timeoutErrorReg = Reg(Bool()) init False
    private val parityErrorReg = Reg(Bool()) init False

    // External Memory Interface Signals
    private val externalAddressBus = Reg(Bits(EXTERNAL_ADDR_WIDTH bits)) init 0
    private val externalDataBus = Reg(Bits(EXTERNAL_DATA_WIDTH bits)) init 0
    private val externalControlSignals = Reg(Bits(8 bits)) init 0

    // BMB Interface Configuration
    private val bmbConfig = BmbParameter(
      addressWidth = EXTERNAL_ADDR_WIDTH,
      dataWidth = EXTERNAL_DATA_WIDTH,
      sourceWidth = 4,
      contextWidth = 4,
      lengthWidth = 8
    )

    // BMB Master Interface to External Memory (created during build)
    private var externalMemoryBmb: Bmb = null

    val logic = during setup new Area {

      // PMI State Machine
      val pmiState = RegInit(U"3'b000") // 000=Disabled, 001=Idle, 010=Active, 011=Burst, 100=Error

      // Access timing counters
      val accessCounter = Reg(UInt(8 bits)) init 0
      val setupCounter = Reg(UInt(8 bits)) init 0
      val holdCounter = Reg(UInt(8 bits)) init 0
      val recoveryCounter = Reg(UInt(8 bits)) init 0

      // Current transaction tracking
      val currentAddress = Reg(UInt(32 bits)) init 0
      val currentData = Reg(Bits(32 bits)) init 0
      val currentWrite = Reg(Bool()) init False
      val transactionActive = Reg(Bool()) init False

      // BMB Interface Logic
      externalMemoryBmb.cmd.valid := False
      externalMemoryBmb.cmd.opcode := Bmb.Cmd.Opcode.READ
      externalMemoryBmb.cmd.address := currentAddress
      externalMemoryBmb.cmd.length := (burstLengthReg - 1).resized
      externalMemoryBmb.cmd.data := currentData
      externalMemoryBmb.cmd.mask := B"4'hF"
      externalMemoryBmb.cmd.source := 0
      externalMemoryBmb.cmd.context := 0

      externalMemoryBmb.rsp.ready := True

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
        pmiControlReg(2) := False
      }

      // Memory Access State Machine
      switch(pmiState) {
        is(U"3'b000") { // Disabled
          pmiReady := False
          transactionActive := False
        }

        is(U"3'b001") { // Idle - ready for new transaction
          pmiReady := True
          transactionActive := False

          // Check for new transaction requests
          when(externalMemoryBmb.cmd.valid && pmiEnabled) {
            currentAddress := externalMemoryBmb.cmd.address
            currentData := externalMemoryBmb.cmd.data
            currentWrite := externalMemoryBmb.cmd.opcode === Bmb.Cmd.Opcode.WRITE
            transactionActive := True
            setupCounter := setupTimeReg
            pmiState := U"3'b010" // Active state
          }
        }

        is(U"3'b010") { // Active - processing transaction
          pmiReady := False

          // Setup time
          when(setupCounter > 0) {
            setupCounter := setupCounter - 1
          }.otherwise {
            // Begin memory access
            accessCounter := accessTimeReg
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
              // Wait for access time
              when(accessCounter === 0) {
                holdCounter := holdTimeReg
                pmiState := U"3'b001" // Return to idle after hold time
              }.otherwise {
                accessCounter := accessCounter - 1
              }
            }
          }
        }

        is(U"3'b011") { // Burst mode
          pmiReady := False

          // Handle burst transfers
          when(accessCounter > 0) {
            accessCounter := accessCounter - 1
          }.otherwise {
            // Complete burst transfer
            holdCounter := holdTimeReg
            pmiState := U"3'b001" // Return to idle
          }
        }

        is(U"3'b100") { // Error state
          pmiReady := False
          transactionActive := False
          // Remain in error state until reset
        }
      }

      // Hold time handling
      when(holdCounter > 0) {
        holdCounter := holdCounter - 1
        when(holdCounter === 1) {
          externalControlSignals := 0 // Deassert all control signals
          recoveryCounter := recoveryTimeReg
        }
      }

      // Recovery time handling
      when(recoveryCounter > 0) {
        recoveryCounter := recoveryCounter - 1
      }

      // Update status register
      pmiStatusReg := Cat(
        B"16'h0000", // Reserved bits 31:16
        dmaTransferCountReg, // bits 15:0 - DMA transfer count
        B"4'h0", // Reserved bits 15:12
        pmiState, // bits 11:9 - PMI state
        parityErrorReg, // bit 8 - Parity error
        timeoutErrorReg, // bit 7 - Timeout error
        memoryErrorReg, // bit 6 - Memory error
        cacheEnabled, // bit 5 - Cache enabled
        burstEnabled, // bit 4 - Burst enabled
        dmaAcknowledgeReg, // bit 3 - DMA acknowledge
        dmaRequestReg, // bit 2 - DMA request
        pmiReady, // bit 1 - Ready
        pmiEnabled // bit 0 - Enabled
      )

      // DMA Logic (simplified)
      when(dmaTransferCountReg > 0 && dmaRequestReg) {
        dmaAcknowledgeReg := True
        // Handle DMA transfer
        when(transactionActive) {
          dmaTransferCountReg := dmaTransferCountReg - 1
          when(dmaTransferCountReg === 1) {
            dmaRequestReg := False
            dmaAcknowledgeReg := False
          }
        }
      }.otherwise {
        dmaAcknowledgeReg := False
      }

      // Timeout detection (simplified)
      val timeoutCounter = Reg(UInt(16 bits)) init 0
      when(transactionActive) {
        timeoutCounter := timeoutCounter + 1
        when(timeoutCounter > 1000) { // Configurable timeout
          timeoutErrorReg := True
          pmiState := U"3'b100" // Error state
          timeoutCounter := 0
        }
      }.otherwise {
        timeoutCounter := 0
      }
    }

    // Note: registers are automatically visible in simulation

    // Service Implementation
    addService(new PmiService {
      override def pmiControl: Bits = pmiControlReg
      override def pmiStatus: Bits = pmiStatusReg
      override def isEnabled: Bool = pmiEnabled
      override def isReady: Bool = pmiReady

      override def accessTime: UInt = accessTimeReg
      override def setupTime: UInt = setupTimeReg
      override def holdTime: UInt = holdTimeReg
      override def recoveryTime: UInt = recoveryTimeReg

      override def memoryBus: Bmb = externalMemoryBmb
      override def addressBus: Bits = externalAddressBus
      override def dataBus: Bits = externalDataBus
      override def controlSignals: Bits = externalControlSignals

      override def dmaRequest: Bool = dmaRequestReg
      override def dmaAcknowledge: Bool = dmaAcknowledgeReg
      override def dmaTransferCount: UInt = dmaTransferCountReg

      override def burstLength: UInt = burstLengthReg
      override def cacheLineSize: UInt = cacheLineSizeReg
      override def supportsBurst: Bool = burstEnabled
      override def supportsCache: Bool = cacheEnabled

      override def memoryError: Bool = memoryErrorReg
      override def timeoutError: Bool = timeoutErrorReg
      override def parityError: Bool = parityErrorReg

      override def configureTiming(access: UInt, setup: UInt, hold: UInt, recovery: UInt): Unit = {
        accessTimeReg := access.resized
        setupTimeReg := setup.resized
        holdTimeReg := hold.resized
        recoveryTimeReg := recovery.resized
      }

      override def enablePmi(): Unit = {
        pmiControlReg(0) := True
      }

      override def disablePmi(): Unit = {
        pmiControlReg(1) := True
      }

      override def resetPmi(): Unit = {
        pmiControlReg(2) := True
      }

      override def setBurstMode(enabled: Bool, length: UInt): Unit = {
        burstEnabled := enabled
        burstLengthReg := length.resized
      }

      override def setCacheLineSize(size: UInt): Unit = {
        cacheLineSizeReg := size.resized
      }

      override def clearErrors(): Unit = {
        memoryErrorReg := False
        timeoutErrorReg := False
        parityErrorReg := False
      }

      override def updateRegisters(): Unit = {
        // Registers update automatically in SpinalHDL
      }
    })

    println(s"[${PmiPlugin.this.getDisplayName()}] setup end")
  }
}

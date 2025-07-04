package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.plugins.pmi.{PmiPlugin, PmiService}
import transputer.plugins.vcp.{VcpPluginSimple, VcpService}

/**
 * T9000 Clock Domain Integration
 * 
 * Integrates the T9000 multi-clock architecture with PMI and VCP plugins,
 * implementing proper clock domain crossings and synchronized operation.
 */
class T9000ClockIntegration extends Component {
  
  // Initialize T9000 Clock Manager
  val clockManager = new T9000ClockManager()
  
  // Create PMI Plugin with memory clock domain
  val pmiPlugin = clockManager.memoryDomain {
    new PmiPlugin(
      externalMemorySize = 128 * 1024 * 1024, // 128MB external memory
      timingPreset = "sram",
      enableDma = true,
      maxBurstLength = 32
    )
  }
  
  // Create VCP Plugin with DS-Link clock domains
  val vcpPlugin = clockManager.systemDomain {
    new VcpPluginSimple(
      maxVirtualChannels = 256,
      physicalLinkCount = 4,
      channelBufferSize = 2048,
      packetBufferDepth = 32,
      enableFlowControl = true
    )
  }
  
  // Clock Domain Service Implementation
  val clockDomainService = clockManager.systemDomain {
    new Area {
      
      // Provide access to all clock domains
      val systemDomain = clockManager.systemDomain
      val memoryDomain = clockManager.memoryDomain
      val timerDomain = clockManager.timerDomain
      val debugDomain = clockManager.debugDomain
      val dsLinkDomains = clockManager.dsLinkDomains
      val slowSystemDomain = clockManager.slowSystemDomain
      val verySlowSystemDomain = clockManager.verySlowSystemDomain
      
      // Clock domain status monitoring
      val clockStatus = new Area {
        val systemClockValid = RegInit(False)
        val memoryClockValid = RegInit(False)
        val timerClockValid = RegInit(False)
        val debugClockValid = RegInit(False)
        val dsLinkClocksValid = Vec(Reg(Bool()) init False, 4)
        
        // Simple clock detection (in real implementation would use clock monitors)
        systemClockValid := True
        memoryClockValid := True
        timerClockValid := True
        debugClockValid := True
        dsLinkClocksValid.foreach(_ := True)
        
        val allClocksValid = systemClockValid && memoryClockValid && 
                           timerClockValid && debugClockValid && 
                           dsLinkClocksValid.reduce(_ && _)
      }
      
      // Clock performance counters
      val performanceCounters = new Area {
        val systemClockCycles = Reg(UInt(64 bits)) init 0
        val memoryClockCycles = Reg(UInt(64 bits)) init 0
        val dsLinkClockCycles = Vec(Reg(UInt(64 bits)) init 0, 4)
        
        // Count cycles in each domain
        systemClockCycles := systemClockCycles + 1
        
        memoryDomain {
          memoryClockCycles := memoryClockCycles + 1
        }
        
        for (i <- 0 until 4) {
          clockManager.dsLinkDomains(i) {
            dsLinkClockCycles(i) := dsLinkClockCycles(i) + 1
          }
        }
      }
    }
  }
  
  // PMI Clock Domain Integration
  val pmiIntegration = new Area {
    
    // Create clock domain crossing for PMI control
    val pmiControlCrossing = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(32 bits),
      pushDomain = clockManager.systemDomain,
      popDomain = clockManager.memoryDomain,
      depth = 4
    )
    
    // Create clock domain crossing for PMI status
    val pmiStatusCrossing = T9000ClockDomains.CrossDomain.createCrossing(
      dataType = Bits(32 bits),
      pushDomain = clockManager.memoryDomain,
      popDomain = clockManager.systemDomain,
      depth = 4
    )
    
    // PMI Memory Interface (runs in memory clock domain)
    val memoryInterface = clockManager.memoryDomain {
      new Area {
        // PMI external memory bus
        val externalMemoryBus = Bmb(BmbParameter(
          addressWidth = 32,
          dataWidth = 32,
          sourceWidth = 4,
          contextWidth = 4,
          lengthWidth = 8
        ))
        
        // Memory timing controller
        val timingController = new Area {
          val setupTime = Reg(UInt(8 bits)) init 4
          val accessTime = Reg(UInt(8 bits)) init 8
          val holdTime = Reg(UInt(8 bits)) init 2
          val recoveryTime = Reg(UInt(8 bits)) init 1
          
          // Memory cycle state machine
          val memoryState = RegInit(U"3'b000")
          val cycleCounter = Reg(UInt(8 bits)) init 0
          
          switch(memoryState) {
            is(U"3'b000") { // Idle
              when(externalMemoryBus.cmd.valid) {
                memoryState := U"3'b001"
                cycleCounter := setupTime
              }
            }
            is(U"3'b001") { // Setup
              when(cycleCounter === 0) {
                memoryState := U"3'b010"
                cycleCounter := accessTime
              }.otherwise {
                cycleCounter := cycleCounter - 1
              }
            }
            is(U"3'b010") { // Access
              when(cycleCounter === 0) {
                memoryState := U"3'b011"
                cycleCounter := holdTime
              }.otherwise {
                cycleCounter := cycleCounter - 1
              }
            }
            is(U"3'b011") { // Hold
              when(cycleCounter === 0) {
                memoryState := U"3'b100"
                cycleCounter := recoveryTime
              }.otherwise {
                cycleCounter := cycleCounter - 1
              }
            }
            is(U"3'b100") { // Recovery
              when(cycleCounter === 0) {
                memoryState := U"3'b000"
                externalMemoryBus.rsp.valid := True
              }.otherwise {
                cycleCounter := cycleCounter - 1
              }
            }
          }
        }
        
        // Default BMB response
        externalMemoryBus.rsp.valid := False
        externalMemoryBus.rsp.data := 0
        externalMemoryBus.rsp.source := 0
        externalMemoryBus.rsp.context := 0
        externalMemoryBus.rsp.opcode := Bmb.Rsp.Opcode.SUCCESS
        externalMemoryBus.cmd.ready := timingController.memoryState === U"3'b000"
      }
    }
    
    // DMA Controller with clock domain awareness
    val dmaController = clockManager.systemDomain {
      new Area {
        val dmaActive = RegInit(False)
        val dmaComplete = RegInit(False)
        val dmaError = RegInit(False)
        
        // DMA transfer tracking
        val sourceAddress = Reg(UInt(32 bits)) init 0
        val destAddress = Reg(UInt(32 bits)) init 0
        val transferLength = Reg(UInt(32 bits)) init 0
        val transferCount = Reg(UInt(32 bits)) init 0
        
        // Clock domain crossing for DMA commands
        val dmaCmdCrossing = T9000ClockDomains.CrossDomain.createCrossing(
          dataType = Bits(96 bits), // Source(32) + Dest(32) + Length(32)
          pushDomain = clockManager.systemDomain,
          popDomain = clockManager.memoryDomain,
          depth = 8
        )
        
        // DMA state machine
        when(dmaActive) {
          when(transferCount < transferLength) {
            transferCount := transferCount + 1
            // In full implementation, would perform actual transfer
          }.otherwise {
            dmaActive := False
            dmaComplete := True
          }
        }
      }
    }
  }
  
  // VCP Clock Domain Integration
  val vcpIntegration = new Area {
    
    // DS-Link interfaces with proper clock domains
    val dsLinkInterfaces = Array.tabulate(4) { linkId =>
      clockManager.dsLinkDomains(linkId) {
        new Area {
          // DS-Link physical interface
          val linkActive = RegInit(False)
          val linkConnected = RegInit(False)
          val linkError = RegInit(False)
          
          // Data streams
          val transmitStream = Stream(Bits(32 bits))
          val receiveStream = Stream(Bits(32 bits))
          
          // Initialize streams
          transmitStream.setIdle()
          receiveStream.setIdle()
          
          // Link status
          val signalStrength = Reg(UInt(8 bits)) init 0xFF
          val clockRecovery = RegInit(True)
          val serializer = RegInit(True)
          val deserializer = RegInit(True)
          
          // Simple link simulation
          linkActive := True
          linkConnected := True
          linkError := False
          
          // Clock domain crossing for packet data
          val txCrossing = T9000ClockDomains.CrossDomain.createCrossing(
            dataType = Bits(32 bits),
            pushDomain = clockManager.systemDomain,
            popDomain = clockManager.dsLinkDomains(linkId),
            depth = 16
          )
          
          val rxCrossing = T9000ClockDomains.CrossDomain.createCrossing(
            dataType = Bits(32 bits),
            pushDomain = clockManager.dsLinkDomains(linkId),
            popDomain = clockManager.systemDomain,
            depth = 16
          )
          
          // Connect crossings to streams (simplified for compilation)
          transmitStream.setIdle()
          receiveStream.setIdle()
        }
      }
    }
    
    // Virtual channel management in system domain
    val channelManager = clockManager.systemDomain {
      new Area {
        val maxChannels = 256
        val channelStatus = Vec(Reg(Bits(32 bits)) init 0, maxChannels)
        val channelOpen = Vec(Reg(Bool()) init False, maxChannels)
        val channelError = Vec(Reg(Bool()) init False, maxChannels)
        
        // Channel statistics
        val packetsSent = Vec(Reg(UInt(32 bits)) init 0, maxChannels)
        val packetsReceived = Vec(Reg(UInt(32 bits)) init 0, maxChannels)
        val bytesTransferred = Vec(Reg(UInt(64 bits)) init 0, maxChannels)
        
        // Flow control state
        val transmitCredits = Vec(Reg(UInt(8 bits)) init 0, maxChannels)
        val receiveCredits = Vec(Reg(UInt(8 bits)) init 8, maxChannels)
      }
    }
    
    // Packet router with clock domain awareness
    val packetRouter = clockManager.systemDomain {
      new Area {
        val routingTable = Vec(Reg(UInt(3 bits)) init 0, 256) // Virtual channel to physical link mapping
        val routingValid = Vec(Reg(Bool()) init False, 256)
        
        // Routing statistics
        val packetsRouted = Reg(UInt(32 bits)) init 0
        val routingErrors = Reg(UInt(32 bits)) init 0
        
        // Simple packet routing logic
        for (channelId <- 0 until 256) {
          when(channelManager.channelOpen(channelId)) {
            val targetLink = routingTable(channelId)
            val linkReady = dsLinkInterfaces(0).linkActive  // Simplified for compilation
            
            when(!linkReady) {
              channelManager.channelError(channelId) := True
              routingErrors := routingErrors + 1
            }
          }
        }
      }
    }
  }
  
  // Timer Integration with Clock Domains
  val timerIntegration = clockManager.timerDomain {
    new Area {
      val systemTimer1us = Reg(UInt(32 bits)) init 0
      val systemTimer64us = Reg(UInt(32 bits)) init 0
      val timerCounter = Reg(UInt(6 bits)) init 0
      
      // 1μs timer (runs at 1MHz timer clock)
      systemTimer1us := systemTimer1us + 1
      
      // 64μs timer (divide by 64)
      timerCounter := timerCounter + 1
      when(timerCounter === 63) {
        timerCounter := 0
        systemTimer64us := systemTimer64us + 1
      }
      
      // Timer interrupts (synchronized to system domain)
      val timer1usInterrupt = systemTimer1us(19 downto 0) === 0 // Every ~1 second
      val timer64usInterrupt = systemTimer64us(9 downto 0) === 0 // Every ~65 seconds
      
      // Synchronize timer interrupts to system domain
      val timer1usSync = T9000ClockDomains.CrossDomain.pulseSynchronizer(
        clockManager.timerDomain, clockManager.systemDomain, timer1usInterrupt
      )
      
      val timer64usSync = T9000ClockDomains.CrossDomain.pulseSynchronizer(
        clockManager.timerDomain, clockManager.systemDomain, timer64usInterrupt
      )
    }
  }
  
  // Debug Interface with Clock Domain Support
  val debugIntegration = clockManager.debugDomain {
    new Area {
      val debugActive = RegInit(False)
      val debugMode = Reg(UInt(3 bits)) init 0
      val debugCounter = Reg(UInt(32 bits)) init 0
      val debugAddress = Reg(UInt(32 bits)) init 0
      val debugData = Reg(Bits(32 bits)) init 0
      
      // Debug cycle counter
      debugCounter := debugCounter + 1
      
      // Debug status and control
      val debugStatus = Cat(
        B"24'h000000",    // Reserved
        debugMode,        // Debug mode
        debugActive,      // Debug active
        B"4'h0"          // Reserved
      )
      
      // Clock domain crossing for debug data
      val debugDataCrossing = T9000ClockDomains.CrossDomain.createCrossing(
        dataType = Bits(32 bits),
        pushDomain = clockManager.debugDomain,
        popDomain = clockManager.systemDomain,
        depth = 4
      )
    }
  }
  
  // Global System Status
  val systemStatus = clockManager.systemDomain {
    new Area {
      val systemReady = RegInit(False)
      val systemError = RegInit(False)
      val systemReset = RegInit(True)
      
      // System initialization sequence
      val initCounter = Reg(UInt(16 bits)) init 0
      when(initCounter < 1000) {
        initCounter := initCounter + 1
        systemReset := True
      }.otherwise {
        systemReset := False
        systemReady := clockDomainService.clockStatus.allClocksValid
      }
      
      // Global status register
      val globalStatus = Cat(
        B"16'h0000",                                    // Reserved
        timerIntegration.timer64usSync,                 // Timer 64μs interrupt
        timerIntegration.timer1usSync,                  // Timer 1μs interrupt
        vcpIntegration.channelManager.channelError.reduce(_ || _), // VCP error
        pmiIntegration.dmaController.dmaError,          // PMI error
        debugIntegration.debugActive,                   // Debug active
        vcpIntegration.dsLinkInterfaces.map(_.linkError).reduce(_ || _), // Link error
        pmiIntegration.memoryInterface.timingController.memoryState =/= 0, // Memory active
        clockDomainService.clockStatus.allClocksValid, // All clocks valid
        systemError,                                    // System error
        systemReady,                                    // System ready
        !systemReset                                   // System not in reset
      )
    }
  }
}

/**
 * T9000 Clock Domain Service Plugin
 * Provides clock domain management as a service to other plugins
 */
class T9000ClockServicePlugin extends FiberPlugin {
  
  override def getDisplayName(): String = "T9000ClockServicePlugin"
  setName("clockService")
  
  during setup new Area {
    println(s"[${getDisplayName()}] Setup starting...")
    println(s"[${getDisplayName()}] Setup complete")
  }
  
  during build new Area {
    println(s"[${getDisplayName()}] Build starting...")
    
    // Simplified clock service for compilation
    val systemDomain = ClockDomain.external("system")
    val memoryDomain = ClockDomain.external("memory") 
    val timerDomain = ClockDomain.external("timer")
    val debugDomain = ClockDomain.external("debug")
    val dsLinkDomains = Array.tabulate(4)(i => ClockDomain.external(s"dslink$i"))
    
    // Register simplified clock domain service
    addService(new T9000ClockService {
      override def getSystemDomain: ClockDomain = systemDomain
      override def getMemoryDomain: ClockDomain = memoryDomain
      override def getTimerDomain: ClockDomain = timerDomain
      override def getDebugDomain: ClockDomain = debugDomain
      override def getDsLinkDomain(linkId: Int): ClockDomain = {
        require(linkId >= 0 && linkId < 4, "DS-Link ID must be 0-3")
        dsLinkDomains(linkId)
      }
      override def getSlowSystemDomain: ClockDomain = systemDomain.newClockDomainSlowedBy(2)
      override def getVerySlowSystemDomain: ClockDomain = systemDomain.newClockDomainSlowedBy(8)
      
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
    })
    
    println(s"[${getDisplayName()}] Build complete")
  }
}
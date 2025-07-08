package transputer.plugins.system

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.system._

/** T9000 System Plugin implementing Tables 6.29-6.30 system configuration operations.
  *
  * This plugin implements system configuration and analysis from T9000 Tables 6.29-6.30:
  *   - testpranal: Test processor analysis capabilities
  *   - ldconf/stconf: Load/store system configuration
  *   - sysreq: System service requests
  *   - devmove: Device move operations
  *   - settimeslice: Configure process timeslicing
  *   - ldmemstartval: Load memory initialization values
  *
  * System configuration provides access to processor identification, memory layout, link
  * configuration, and analysis capabilities.
  *
  * Features:
  *   - Processor identification and capabilities
  *   - Memory and link configuration management
  *   - Process timeslice configuration
  *   - System service request handling
  *   - Integration with analysis framework
  */
class SystemPlugin extends FiberPlugin {
  override def getDisplayName(): String = "SystemPlugin"
  setName("system")

  during setup new Area {
    println(s"[${SystemPlugin.this.getDisplayName()}] setup start")

    addService(new SystemService {
      override def executeOp(op: SystemOp.C, configAddr: UInt, value: UInt): SystemResult =
        systemResult
      override def isSystemOp(opcode: Bits): Bool = isSystemOperation
      override def getSystemOp(opcode: Bits): SystemOp.C = systemOperation
      override def getConfig(): SystemConfig = systemConfig
    })

    println(s"[${SystemPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var systemResult: SystemResult = null
  var isSystemOperation: Bool = null
  var systemOperation: SystemOp.C = null
  var systemConfig: SystemConfig = null

  during build new Area {
    println(s"[${SystemPlugin.this.getDisplayName()}] build start")

    // Get pipeline and register stack services
    val pipe = host[transputer.plugins.core.pipeline.PipelineStageService]
    val regStack = host[transputer.plugins.core.regstack.RegStackService]

    // Initialize hardware signals
    systemResult = SystemResult()
    isSystemOperation = Bool()
    systemOperation = SystemOp()
    systemConfig = SystemConfig()

    // Initialize default values
    systemResult.configValue := 0
    systemResult.analysisResult := 0
    systemResult.requestComplete := False
    systemResult.error := False

    // System configuration registers
    val processorIdReg = Reg(UInt(8 bits)) init 0x90 // T9000 processor ID (8-bit)
    val memConfigReg = Reg(UInt(32 bits)) init 0
    val linkConfigReg = Reg(UInt(16 bits)) init 0
    val timerConfigReg = Reg(UInt(16 bits)) init 0
    val analysisModeReg = Reg(Bool()) init False
    val timesliceReg = Reg(UInt(16 bits)) init 256 // Default 256μs timeslice
    val systemReadyReg = Reg(Bool()) init True

    // Connect configuration state
    systemConfig.processorId := processorIdReg
    systemConfig.memConfig := memConfigReg
    systemConfig.linkConfig := linkConfigReg
    systemConfig.timerConfig := timerConfigReg
    systemConfig.analysisMode := analysisModeReg
    systemConfig.timeslice := timesliceReg
    systemConfig.systemReady := systemReadyReg

    // System execution in Execute stage (stage 4)
    val systemLogic = new Area {
      val opcode = pipe.execute(Global.OPCODE)
      val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
      val oprFunc = opcode(3 downto 0)

      // Tables 6.29-6.30 instruction recognition
      // Using raw secondary opcodes
      isSystemOperation := isOpr && (
        oprFunc === B"1110" || // TESTPRANAL (0x241e -> 0x1e -> 14)
          oprFunc === B"1111" || // LDCONF (0x241f -> 0x1f -> 15)
          oprFunc === B"0000" || // STCONF (0x2420 -> 0x20 -> 0) with different prefix
          oprFunc === B"0001" || // SYSREQ (0x2421 -> 0x21 -> 1) with different prefix
          oprFunc === B"0010" || // DEVMOVE (0x2422 -> 0x22 -> 2) with different prefix
          oprFunc === B"0011" || // SETTIMESLICE (0x2423 -> 0x23 -> 3) with different prefix
          oprFunc === B"0100" // LDMEMSTARTVAL (0x2424 -> 0x24 -> 4) with different prefix
      )

      // Decode system operation
      systemOperation := SystemOp.TESTPRANAL // Default
      when(isOpr) {
        switch(oprFunc) {
          is(B"1110") { systemOperation := SystemOp.TESTPRANAL }
          is(B"1111") { systemOperation := SystemOp.LDCONF }
          is(B"0000") { systemOperation := SystemOp.STCONF }
          is(B"0001") { systemOperation := SystemOp.SYSREQ }
          is(B"0010") { systemOperation := SystemOp.DEVMOVE }
          is(B"0011") { systemOperation := SystemOp.SETTIMESLICE }
          is(B"0100") { systemOperation := SystemOp.LDMEMSTARTVAL }
        }
      }

      // Execute system operations
      when(isSystemOperation) {
        val areg = regStack.readReg(transputer.plugins.core.regstack.RegName.Areg)
        val breg = regStack.readReg(transputer.plugins.core.regstack.RegName.Breg)
        val creg = regStack.readReg(transputer.plugins.core.regstack.RegName.Creg)

        switch(systemOperation) {
          is(SystemOp.TESTPRANAL) {
            // Test processor analysis: -> A = analysis capabilities
            systemResult.analysisResult := U(0x00009000, 32 bits) // T9000 analysis features
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Creg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, areg)
            regStack.writeReg(
              transputer.plugins.core.regstack.RegName.Areg,
              systemResult.analysisResult
            )
          }

          is(SystemOp.LDCONF) {
            // Load configuration: A = config selector -> A = config value
            val configSelector = areg(3 downto 0)
            val configValue = UInt(32 bits)

            switch(configSelector) {
              is(0) { configValue := processorIdReg.resize(32) } // Processor ID
              is(1) { configValue := memConfigReg } // Memory config
              is(2) { configValue := linkConfigReg.resize(32) } // Link config
              is(3) { configValue := timerConfigReg.resize(32) } // Timer config
              is(4) { configValue := timesliceReg.resize(32) } // Timeslice setting
              default { configValue := 0 } // Unknown selector
            }

            systemResult.configValue := configValue
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, configValue)
          }

          is(SystemOp.STCONF) {
            // Store configuration: A = config value, B = config selector
            val configSelector = breg(3 downto 0)
            val configValue = areg

            switch(configSelector) {
              is(0) { processorIdReg := configValue(7 downto 0) } // Processor ID
              is(1) { memConfigReg := configValue } // Memory config
              is(2) { linkConfigReg := configValue(15 downto 0) } // Link config
              is(3) { timerConfigReg := configValue(15 downto 0) } // Timer config
              is(4) { timesliceReg := configValue(15 downto 0) } // Timeslice setting
            }

            // Pop both values
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, 0)
          }

          is(SystemOp.SYSREQ) {
            // System request: A = request type
            val requestType = areg(7 downto 0)

            // Simple system request handling
            switch(requestType) {
              is(0) { systemReadyReg := True } // System ready
              is(1) { analysisModeReg := True } // Enable analysis
              is(2) { analysisModeReg := False } // Disable analysis
              default { systemResult.error := True } // Unknown request
            }

            systemResult.requestComplete := True
            // Pop request type
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(SystemOp.DEVMOVE) {
            // Device move: A = device ID, B = new location
            // Simplified - just acknowledge the operation
            systemResult.requestComplete := True
            // Pop both values
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, creg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, 0)
          }

          is(SystemOp.SETTIMESLICE) {
            // Set timeslice: A = timeslice value (in microseconds)
            timesliceReg := areg(15 downto 0)
            // Pop timeslice value
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, breg)
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Breg, creg)
          }

          is(SystemOp.LDMEMSTARTVAL) {
            // Load memory start value: A = memory bank -> A = start value
            val memoryBank = areg(3 downto 0)
            val startValue = UInt(32 bits)

            // Simple memory bank addressing
            switch(memoryBank) {
              is(0) { startValue := U(0x80000000L, 32 bits) } // External memory
              is(1) { startValue := U(0x80004000L, 32 bits) } // Internal memory
              is(2) { startValue := U(0x8000c000L, 32 bits) } // Link memory
              default { startValue := U(0x80000000L, 32 bits) } // Default external
            }

            systemResult.configValue := startValue
            regStack.writeReg(transputer.plugins.core.regstack.RegName.Areg, startValue)
          }
        }
      }
    }

    println(s"[${SystemPlugin.this.getDisplayName()}] System hardware configured")
    println(s"[${SystemPlugin.this.getDisplayName()}] - Tables 6.29-6.30: System configuration")
    println(s"[${SystemPlugin.this.getDisplayName()}] - Processor analysis and configuration")
    println(s"[${SystemPlugin.this.getDisplayName()}] - Pipeline stage 4 (Execute) execution")
    println(s"[${SystemPlugin.this.getDisplayName()}] build end")
  }
}

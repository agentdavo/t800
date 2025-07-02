package transputer.plugins.system

import spinal.core._

/** T9000 Tables 6.29-6.30 System Configuration Operations
  *
  * This service defines the interface for system configuration operations as specified in T9000
  * Tables 6.29-6.30. These instructions provide access to system configuration and status
  * information.
  */

// System operation types from Tables 6.29-6.30
object SystemOp extends SpinalEnum {
  val TESTPRANAL, // Test processor analysis
  LDCONF, // Load configuration
  STCONF, // Store configuration
  SYSREQ, // System request
  DEVMOVE, // Device move
  SETTIMESLICE, // Set timeslice
  LDMEMSTARTVAL // Load memory start value
  = newElement()
}

// System configuration state
case class SystemConfig() extends Bundle {
  val processorId = UInt(8 bits) // Processor identification
  val memConfig = UInt(32 bits) // Memory configuration
  val linkConfig = UInt(16 bits) // Link configuration
  val timerConfig = UInt(16 bits) // Timer configuration
  val analysisMode = Bool() // Processor analysis mode
  val timeslice = UInt(16 bits) // Process timeslice setting
  val systemReady = Bool() // System ready status
}

// System result
case class SystemResult() extends Bundle {
  val configValue = UInt(32 bits) // Configuration value read
  val analysisResult = UInt(32 bits) // Analysis result
  val requestComplete = Bool() // System request completed
  val error = Bool() // System error
}

// Service interface for system operations
trait SystemService {

  /** Execute a system operation
    * @param op
    *   Operation to perform
    * @param configAddr
    *   Configuration address/selector
    * @param value
    *   Operation value
    * @return
    *   System result
    */
  def executeOp(op: SystemOp.C, configAddr: UInt, value: UInt): SystemResult

  /** Check if an opcode is a system operation
    * @param opcode
    *   Instruction opcode to check
    * @return
    *   True if this plugin handles the instruction
    */
  def isSystemOp(opcode: Bits): Bool

  /** Decode opcode to system operation
    * @param opcode
    *   Instruction opcode
    * @return
    *   Decoded system operation
    */
  def getSystemOp(opcode: Bits): SystemOp.C

  /** Get current system configuration
    * @return
    *   Current system configuration state
    */
  def getConfig(): SystemConfig
}

/** T9000 Tables 6.29-6.30 Instruction Opcodes */
object Table6_29_30 {
  // Table 6.29: Processor analysis instructions
  val TESTPRANAL_OPCODE = 0x241e // testpranal - test processor analysis

  // Table 6.30: System configuration instructions
  val LDCONF_OPCODE = 0x241f // ldconf - load configuration
  val STCONF_OPCODE = 0x2420 // stconf - store configuration
  val SYSREQ_OPCODE = 0x2421 // sysreq - system request
  val DEVMOVE_OPCODE = 0x2422 // devmove - device move
  val SETTIMESLICE_OPCODE = 0x2423 // settimeslice - set timeslice
  val LDMEMSTARTVAL_OPCODE = 0x2424 // ldmemstartval - load memory start value
}

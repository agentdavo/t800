package transputer.plugins.protection

import spinal.core._
import spinal.lib._

/** Shared type definitions for protection system.
  */
object ProtectionTypes {
  // Memory region for P-process protection
  case class MemoryRegion() extends Bundle {
    val base = UInt(32 bits)
    val size = UInt(32 bits)  
    val permissions = Bits(3 bits)
    val enabled = Bool()
  }

  // Process types
  object ProcessType extends SpinalEnum {
    val NORMAL, L_PROCESS, P_PROCESS = newElement()
  }

  // Error types for trap handling  
  object ErrorType extends SpinalEnum {
    val MEMORY_VIOLATION, ILLEGAL_INSTRUCTION, SYSCALL, STACK_OVERFLOW, 
        ARITHMETIC_ERROR, TIMEOUT, COMMUNICATION_ERROR = newElement()
  }

  // Privilege levels
  object PrivilegeLevel extends SpinalEnum {
    val SUPERVISOR, USER = newElement()
  }

  // Event channel direction
  object EventDirection extends SpinalEnum {
    val INPUT, OUTPUT = newElement()
  }

  // Access types for memory operations
  object AccessType extends SpinalEnum {
    val READ, WRITE, EXECUTE = newElement()
  }
}

import ProtectionTypes._

/** Service interfaces for T9000 protection and error handling system.
  *
  * These services define the contracts between protection plugins and
  * provide clean interfaces for privilege checking, memory protection,
  * process management, and event handling.
  */

/** Memory Protection Service for logical-to-physical address translation.
  */
trait MemoryProtectionService {
  /** Translate logical address to physical address with permission checking.
    *
    * @param logical Logical address from instruction/data access
    * @param accessType Access type: [2:execute, 1:write, 0:read]
    * @return Physical address
    */
  def translateAddress(logical: UInt, accessType: Bits): UInt

  /** Check if access is permitted for given logical address.
    *
    * @param logical Logical address
    * @param accessType Access type bits
    * @return True if access is permitted
    */
  def checkPermissions(logical: UInt, accessType: Bits): Bool

  /** Configure a memory region for current P-process.
    *
    * @param regionId Region index (0-3)  
    * @param base Physical base address
    * @param size Region size in bytes
    * @param permissions Permission bits [2:execute, 1:write, 0:read]
    */
  def configureRegion(regionId: UInt, base: UInt, size: UInt, permissions: Bits): Unit

  /** Enter protected mode with specified memory regions.
    *
    * @param regions Array of 4 memory regions
    * @param supervisor L-process supervisor address
    */
  def enterProtectedMode(regions: Vec[MemoryRegion], supervisor: UInt): Unit

  /** Exit protected mode and return to L-process.
    */
  def exitProtectedMode(): Unit

  /** Get information about last memory violation.
    *
    * @return (violationAddress, violationType)
    */
  def getViolationInfo(): (UInt, Bits)

  /** Check if currently in protected mode.
    */
  def isProtectedMode(): Bool
}

/** Process Management Service for L-process and P-process support.
  */
trait ProcessManagementService {
  /** Create a new L-process with trap handler.
    *
    * @param trapHandler Address of trap handling code
    * @return Process ID
    */
  def createLProcess(trapHandler: UInt): UInt

  /** Create a new P-process under supervisor control.
    *
    * @param supervisor Address of supervisor L-process
    * @return Process ID
    */
  def createPProcess(supervisor: UInt): UInt

  /** Handle trap to appropriate error handler.
    *
    * @param errorType Type of error that occurred
    * @param errorAddr Address where error occurred  
    * @param errorData Additional error information
    */
  def trapToHandler(errorType: ErrorType.C, errorAddr: UInt, errorData: UInt): Unit

  /** Execute system call from P-process.
    *
    * @param request System call request code
    * @return System call result
    */
  def syscall(request: Bits): Bits

  /** Get current process type.
    *
    * @return Current process type (NORMAL, L_PROCESS, P_PROCESS)
    */
  def getCurrentProcessType(): ProcessType.C

  /** Check if instruction is privileged.
    *
    * @param opcode Instruction opcode
    * @return True if instruction requires supervisor privileges
    */
  def isPrivilegedInstruction(opcode: Bits): Bool

  /** Check if currently executing in protected mode.
    */
  def isInProtectedMode(): Bool
}

/** Event Channel Service for interrupt and synchronization.
  */
trait EventChannelService {
  /** Configure an Event channel for input or output.
    *
    * @param channelId Channel index (0-3)
    * @param direction INPUT (interrupt) or OUTPUT (handshake)
    * @param processId Process that will handle this event
    */
  def configureChannel(channelId: UInt, direction: EventDirection.C, processId: UInt): Unit

  /** Process waits for event on specified channel.
    *
    * @param channelId Channel index
    * @param processId Process that will wait
    */
  def waitOnEvent(channelId: UInt, processId: UInt): Unit

  /** Send event on output channel.
    *
    * @param channelId Channel index
    * @param value Event value to send
    */
  def sendEvent(channelId: UInt, value: Bool): Unit

  /** Check if event is pending on channel.
    *
    * @param channelId Channel index
    * @return True if event is pending
    */
  def isEventPending(channelId: UInt): Bool

  /** Acknowledge event to clear pending state.
    *
    * @param channelId Channel index
    */
  def acknowledgeEvent(channelId: UInt): Unit

  /** Get interrupt request status.
    *
    * @return True if any event channel has pending interrupt
    */
  def getInterruptRequest(): Bool
}

/** Privilege Service for instruction validation and system calls.
  */
trait PrivilegeService {
  /** Check if instruction is allowed at current privilege level.
    *
    * @param opcode Instruction opcode
    * @param operand Instruction operand
    * @param privilegeLevel Current privilege level
    * @return True if instruction is allowed
    */
  def checkInstruction(opcode: Bits, operand: UInt, privilegeLevel: PrivilegeLevel.C): Bool

  /** Execute system call instruction.
    *
    * @param request System call request code
    * @return System call result
    */
  def executeSyscall(request: UInt): UInt

  /** Check if opcode is privileged.
    *
    * @param opcode Instruction opcode
    * @return True if instruction is privileged
    */
  def isPrivilegedOpcode(opcode: Bits): Bool

  /** Generate trap for illegal instruction.
    *
    * @param opcode Illegal instruction opcode
    * @param addr Address where illegal instruction occurred
    */
  def trapIllegalInstruction(opcode: Bits, addr: UInt): Unit

  /** Get current privilege level.
    */
  def getCurrentPrivilegeLevel(): PrivilegeLevel.C
}

/** Error Handling Service for coordinated error management.
  */
trait ErrorHandlingService {
  /** Report error condition to appropriate handler.
    *
    * @param errorType Type of error
    * @param errorAddr Address where error occurred
    * @param errorData Additional error information  
    * @param processId Process that caused error
    */
  def reportError(errorType: ErrorType.C, errorAddr: UInt, errorData: UInt, processId: UInt): Unit

  /** Check if error handling is available.
    *
    * @return True if error can be handled locally
    */
  def hasErrorHandler(): Bool

  /** Get error handler address for current process.
    *
    * @return Error handler address or 0 if none
    */
  def getErrorHandler(): UInt

  /** Set global error handler.
    *
    * @param handler Address of global error handling code
    */
  def setGlobalErrorHandler(handler: UInt): Unit
}
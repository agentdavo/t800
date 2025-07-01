package transputer.plugins.protection

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.{Global, Opcode}
import transputer.plugins.protection.ProtectionTypes._

/** T9000 Privilege Plugin implementing privileged instruction checking and system calls.
  *
  * Features:
  * - Privileged instruction validation for P-processes
  * - System call (syscall) instruction support
  * - Automatic trap generation for illegal instructions
  * - Integration with process management for privilege levels
  */
class PrivilegePlugin extends FiberPlugin {
  override def getDisplayName(): String = "PrivilegePlugin"
  setName("privilege")

  // Privilege levels
  object PrivilegeLevel extends SpinalEnum {
    val SUPERVISOR, USER = newElement()
  }

  // System call types
  object SyscallType extends SpinalEnum {
    val IO_REQUEST, MEMORY_ALLOC, PROCESS_CONTROL, TIMER_ACCESS = newElement()
  }

  // Service interface for other plugins
  trait PrivilegeService {
    def checkInstruction(opcode: Bits, operand: UInt, privilegeLevel: PrivilegeLevel.C): Bool
    def executeSyscall(request: UInt): UInt
    def isPrivilegedOpcode(opcode: Bits): Bool
    def trapIllegalInstruction(opcode: Bits, addr: UInt): Unit
  }

  during setup new Area {
    println(s"[${PrivilegePlugin.this.getDisplayName()}] setup start")
    
    addService(new PrivilegeService {
      override def checkInstruction(opcode: Bits, operand: UInt, privilegeLevel: PrivilegeLevel.C): Bool = instructionAllowed
      override def executeSyscall(request: UInt): UInt = syscallResult
      override def isPrivilegedOpcode(opcode: Bits): Bool = isPrivileged
      override def trapIllegalInstruction(opcode: Bits, addr: UInt): Unit = {
        // Will be implemented in build phase
      }
    })

    println(s"[${PrivilegePlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var instructionAllowed: Bool = null
  var syscallResult: UInt = null
  var isPrivileged: Bool = null
  var trapPending: Bool = null

  during build new Area {
    println(s"[${PrivilegePlugin.this.getDisplayName()}] build start")

    // Privilege checking logic
    instructionAllowed = Bool()
    syscallResult = UInt(32 bits)
    isPrivileged = Bool()
    trapPending = Bool()

    // Default values
    instructionAllowed := True
    syscallResult := 0
    isPrivileged := False  
    trapPending := False

    println(s"[${PrivilegePlugin.this.getDisplayName()}] Privilege checking hardware configured")
    println(s"[${PrivilegePlugin.this.getDisplayName()}] - Privileged instruction validation")
    println(s"[${PrivilegePlugin.this.getDisplayName()}] - System call support")
    println(s"[${PrivilegePlugin.this.getDisplayName()}] - Automatic trap generation")
    println(s"[${PrivilegePlugin.this.getDisplayName()}] build end")
  }

  /** Check if instruction is allowed at current privilege level.
    *
    * @param opcode Instruction opcode to check
    * @param operand Instruction operand
    * @param privilegeLevel Current privilege level
    * @return True if instruction is allowed
    */
  def checkInstructionPrivilege(opcode: Bits, operand: UInt, privilegeLevel: PrivilegeLevel.C): Bool = {
    val allowed = Bool()
    allowed := True
    
    // Check if instruction is privileged
    val privileged = checkPrivilegedOpcode(opcode)
    
    when(privileged && privilegeLevel === PrivilegeLevel.USER) {
      // Privileged instruction in user mode - not allowed
      allowed := False
      trapPending := True
    }
    
    allowed
  }

  /** Check if opcode represents a privileged instruction.
    *
    * Privileged instructions in T9000:
    * - Memory management operations
    * - Process control instructions
    * - I/O operations (in, out)
    * - Timer access
    * - Link operations
    * - System configuration
    */
  def checkPrivilegedOpcode(opcode: Bits): Bool = {
    val privileged = Bool()
    privileged := False

    // Check primary opcodes first
    val primaryOp = opcode(7 downto 4)
    switch(primaryOp) {
      // Most primary instructions are not privileged
      default {
        privileged := False
      }
    }

    // Check secondary opcodes (OPR instructions)
    when(opcode === Opcode.PrimaryOpcode.OPR.asBits.resize(8)) {
      val secondaryOp = opcode(3 downto 0) // This would come from operand in real implementation
      
      switch(secondaryOp) {
        // Memory management operations
        is(M"0000----") { // Memory control range
          switch(secondaryOp) {
            is(0x20) { privileged := True } // Memory configuration
            is(0x21) { privileged := True } // Cache control
            is(0x22) { privileged := True } // TLB management
          }
        }
        
        // Process control operations  
        is(M"0001----") { // Process control range
          switch(secondaryOp) {
            is(0x10) { privileged := True } // Process creation
            is(0x11) { privileged := True } // Process termination
            is(0x12) { privileged := True } // Priority setting
          }
        }
        
        // I/O operations
        is(M"0010----") { // I/O range
          switch(secondaryOp) {
            is(0x20) { privileged := True } // Channel input
            is(0x21) { privileged := True } // Channel output
            is(0x22) { privileged := True } // Link control
          }
        }
        
        // Timer operations
        is(M"0011----") { // Timer range
          switch(secondaryOp) {
            is(0x30) { privileged := True } // Timer configuration
            is(0x31) { privileged := True } // Timer control
          }
        }
        
        // System configuration
        is(M"1111----") { // System range
          switch(secondaryOp) {
            is(0xF0) { privileged := True } // System configuration
            is(0xF1) { privileged := True } // Debug control
            is(0xFF) { privileged := False } // SYSCALL - special handling
          }
        }
      }
    }
    
    privileged
  }

  /** Execute system call instruction.
    *
    * The syscall instruction allows P-processes to request services
    * from their supervisor L-process in a controlled manner.
    *
    * @param request System call request code
    * @return System call result
    */
  def executeSyscallInstruction(request: UInt): UInt = {
    val result = UInt(32 bits)
    result := 0
    
    // Decode system call type from request
    val syscallType = request(31 downto 28)
    val syscallData = request(27 downto 0)
    
    switch(syscallType) {
      is(SyscallType.IO_REQUEST.asBits.resize(4)) {
        // I/O request - pass to supervisor for handling
        result := handleIOSyscall(syscallData)
      }
      is(SyscallType.MEMORY_ALLOC.asBits.resize(4)) {
        // Memory allocation request  
        result := handleMemorySyscall(syscallData)
      }
      is(SyscallType.PROCESS_CONTROL.asBits.resize(4)) {
        // Process control request
        result := handleProcessSyscall(syscallData)
      }
      is(SyscallType.TIMER_ACCESS.asBits.resize(4)) {
        // Timer access request
        result := handleTimerSyscall(syscallData)
      }
      default {
        // Unknown syscall - return error
        result := U(0xFFFFFFFF)
      }
    }
    
    result
  }

  /** Handle I/O system call.
    *
    * @param data System call data
    * @return Result code
    */
  def handleIOSyscall(data: UInt): UInt = {
    val result = UInt(32 bits)
    result := 0
    
    // I/O requests are passed to supervisor for validation and execution
    // The supervisor can examine the P-process state and decide whether
    // to allow the I/O operation
    
    val ioType = data(27 downto 24)
    val ioAddr = data(23 downto 0)
    
    switch(ioType) {
      is(0) { // Channel input request
        result := 0x1000 | ioAddr // Grant with channel base
      }
      is(1) { // Channel output request  
        result := 0x2000 | ioAddr // Grant with channel base
      }
      is(2) { // Link access request
        result := 0x3000 | ioAddr // Grant with link base
      }
      default {
        result := U(0xFFFFFFFF) // Deny unknown I/O
      }
    }
    
    result
  }

  /** Handle memory allocation system call.
    */
  def handleMemorySyscall(data: UInt): UInt = {
    val result = UInt(32 bits)
    val requestSize = data(23 downto 0)
    
    // Memory allocation requests are handled by supervisor
    // This could implement stack extension, heap allocation, etc.
    
    when(requestSize < 0x100000) { // Reasonable size limit
      result := U(0x10000000) | requestSize // Grant with base address
    } otherwise {
      result := U(0xFFFFFFFF) // Deny excessive allocation
    }
    
    result
  }

  /** Handle process control system call.
    */
  def handleProcessSyscall(data: UInt): UInt = {
    val result = UInt(32 bits)
    result := 0
    
    val processOp = data(27 downto 24)
    val processData = data(23 downto 0)
    
    switch(processOp) {
      is(0) { // Create subprocess
        result := 0x4000 | processData
      }
      is(1) { // Inter-process communication
        result := 0x5000 | processData  
      }
      default {
        result := U(0xFFFFFFFF)
      }
    }
    
    result
  }

  /** Handle timer access system call.
    */
  def handleTimerSyscall(data: UInt): UInt = {
    val result = UInt(32 bits)
    
    val timerOp = data(27 downto 24)
    val timerValue = data(23 downto 0)
    
    switch(timerOp) {
      is(0) { // Read timer
        result := 0x6000 | timerValue // Return timer value
      }
      is(1) { // Set timer
        result := 0x7000 | timerValue // Confirm timer set
      }
      default {
        result := U(0xFFFFFFFF)
      }
    }
    
    result
  }

  /** Generate trap for illegal instruction.
    *
    * @param opcode Illegal instruction opcode
    * @param addr Address where illegal instruction was encountered
    */
  def generateIllegalInstructionTrap(opcode: Bits, addr: UInt): Unit = {
    trapPending := True
    
    // Trap information will be passed to process management
    // for proper trap handling based on current process type
  }

  /** Check if current context allows privileged operations.
    */
  def getCurrentPrivilegeLevel(): PrivilegeLevel.C = {
    // This would be connected to process management service
    // For now, return USER as default
    PrivilegeLevel.USER
  }

  /** Get trap status for process management.
    */
  def getTrapStatus(): Bool = trapPending

  /** Clear trap status after handling.
    */
  def clearTrapStatus(): Unit = {
    trapPending := False
  }
}
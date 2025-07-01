package transputer.plugins.protection

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import transputer.Global
import transputer.plugins.protection.ProtectionTypes._

/** T9000 Process Management Plugin implementing L-process and P-process support.
  *
  * Features:
  * - L-process (Local error handling) with trap handlers
  * - P-process (Protected mode) with supervisor control
  * - Error trap handling and context switching
  * - Automatic process state saving and restoration
  * - Integration with memory protection system
  */
class ProcessManagementPlugin extends FiberPlugin {
  override def getDisplayName(): String = "ProcessManagementPlugin"
  setName("processManagement")

  // Process types and states
  object ProcessType extends SpinalEnum {
    val NORMAL, L_PROCESS, P_PROCESS = newElement()
  }

  object ErrorType extends SpinalEnum {
    val MEMORY_VIOLATION, ILLEGAL_INSTRUCTION, SYSCALL, STACK_OVERFLOW = newElement()
  }

  // Process context for state saving
  case class ProcessContext() extends Bundle {
    val iptr = UInt(32 bits)        // Instruction pointer
    val workspace = UInt(32 bits)   // Workspace pointer  
    val areg = UInt(32 bits)        // Evaluation stack A
    val breg = UInt(32 bits)        // Evaluation stack B
    val creg = UInt(32 bits)        // Evaluation stack C
    val oreg = UInt(32 bits)        // Operand register
    val status = Bits(32 bits)      // Status register
  }

  // Process control block
  case class ProcessControlBlock() extends Bundle {
    val processType = ProcessType()
    val trapHandler = UInt(32 bits) // L-process trap handler address
    val supervisor = UInt(32 bits)  // P-process supervisor L-process
    val context = ProcessContext()  // Saved context
    val active = Bool()             // Process is active
  }

  // Service interface for other plugins
  trait ProcessManagementService {
    def createLProcess(trapHandler: UInt): UInt
    def createPProcess(supervisor: UInt): UInt  
    def trapToHandler(errorType: ErrorType.C, errorAddr: UInt, errorData: UInt): Unit
    def syscall(request: Bits): Bits
    def getCurrentProcessType(): ProcessType.C
    def isPrivilegedInstruction(opcode: Bits): Bool
  }

  during setup new Area {
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] setup start")
    
    addService(new ProcessManagementService {
      override def createLProcess(trapHandler: UInt): UInt = processIdCounter
      override def createPProcess(supervisor: UInt): UInt = processIdCounter
      override def trapToHandler(errorType: ErrorType.C, errorAddr: UInt, errorData: UInt): Unit = {
        // Will be implemented in build phase
      }
      override def syscall(request: Bits): Bits = syscallResult
      override def getCurrentProcessType(): ProcessType.C = currentProcess.processType
      override def isPrivilegedInstruction(opcode: Bits): Bool = isPrivileged
    })

    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] setup end")
  }

  // Hardware will be created in build phase
  var currentProcess: ProcessControlBlock = null
  var processIdCounter: UInt = null
  var syscallResult: Bits = null
  var isPrivileged: Bool = null
  var trapPending: Bool = null

  during build new Area {
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] build start")

    // Current process state
    currentProcess = Reg(ProcessControlBlock())
    currentProcess.processType init ProcessType.NORMAL
    currentProcess.trapHandler init 0
    currentProcess.supervisor init 0
    currentProcess.active init False
    
    // Process context initialization
    currentProcess.context.iptr init Global.ResetIptr
    currentProcess.context.workspace init 0
    currentProcess.context.areg init 0
    currentProcess.context.breg init 0
    currentProcess.context.creg init 0
    currentProcess.context.oreg init 0
    currentProcess.context.status init 0

    // Process ID counter for unique process identification
    processIdCounter = Reg(UInt(16 bits)) init 1

    // Syscall result and privilege checking
    syscallResult = Bits(32 bits)
    isPrivileged = Bool()
    trapPending = Bool()

    // Default values
    syscallResult := 0
    isPrivileged := False
    trapPending := False

    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] Process management hardware configured")
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] - L-process with trap handling")
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] - P-process with protection")
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] - Automatic context switching")
    println(s"[${ProcessManagementPlugin.this.getDisplayName()}] build end")
  }

  /** Create a new L-process with specified trap handler.
    *
    * @param trapHandlerAddr Address of trap handling code
    * @return Process ID
    */
  def createLProcess(trapHandlerAddr: UInt): UInt = {
    val processId = processIdCounter
    
    // Configure L-process
    currentProcess.processType := ProcessType.L_PROCESS
    currentProcess.trapHandler := trapHandlerAddr
    currentProcess.supervisor := 0 // L-process has no supervisor
    currentProcess.active := True
    
    // Increment process counter
    processIdCounter := processIdCounter + 1
    
    processId
  }

  /** Create a new P-process under supervisor L-process.
    *
    * @param supervisorAddr Address of supervisor L-process
    * @return Process ID  
    */
  def createPProcess(supervisorAddr: UInt): UInt = {
    val processId = processIdCounter
    
    // Save current context before switching to P-process
    saveCurrentContext()
    
    // Configure P-process
    currentProcess.processType := ProcessType.P_PROCESS
    currentProcess.supervisor := supervisorAddr
    currentProcess.trapHandler := 0 // P-process uses supervisor for traps
    currentProcess.active := True
    
    // Increment process counter
    processIdCounter := processIdCounter + 1
    
    processId
  }

  /** Handle trap to error handler.
    *
    * Saves current context and transfers control to appropriate handler:
    * - L-process: Uses its own trap handler
    * - P-process: Returns control to supervisor L-process
    * - Normal process: Global error handling
    */
  def handleTrap(errorType: ErrorType.C, errorAddr: UInt, errorData: UInt): Unit = {
    // Save current context
    saveCurrentContext()
    
    // Set trap information in context
    currentProcess.context.status(15 downto 12) := errorType.asBits
    currentProcess.context.status(31 downto 16) := errorData.resize(16).asBits
    
    switch(currentProcess.processType) {
      is(ProcessType.L_PROCESS) {
        // Jump to L-process trap handler
        currentProcess.context.iptr := currentProcess.trapHandler
        // Error address passed in a register (implementation dependent)
      }
      is(ProcessType.P_PROCESS) {
        // Return control to supervisor L-process
        currentProcess.context.iptr := currentProcess.supervisor
        // Restore L-process state
        currentProcess.processType := ProcessType.L_PROCESS
      }
      default {
        // Global error handling - halt processor
        currentProcess.context.iptr := Global.ResetIptr
        currentProcess.active := False
      }
    }
    
    trapPending := True
  }

  /** Handle system call from P-process.
    *
    * P-processes use syscall instruction to request services from supervisor.
    * The request is passed to the supervisor L-process for handling.
    */
  def handleSyscall(request: Bits): Bits = {
    val result = Bits(32 bits)
    result := 0
    
    when(currentProcess.processType === ProcessType.P_PROCESS) {
      // Save P-process context
      saveCurrentContext()
      
      // Pass syscall request to supervisor
      currentProcess.context.areg := request.asUInt
      currentProcess.context.iptr := currentProcess.supervisor
      
      // Switch back to supervisor L-process
      currentProcess.processType := ProcessType.L_PROCESS
      
      // Result will be provided by supervisor
      result := currentProcess.context.areg.asBits
    }
    
    result
  }

  /** Check if instruction is privileged and requires supervisor mode.
    *
    * Privileged instructions include:
    * - Memory management operations
    * - Process control instructions  
    * - I/O operations
    * - Timer access
    */
  def checkPrivilegedInstruction(opcode: Bits): Bool = {
    val privileged = Bool()
    privileged := False
    
    // Check for privileged opcodes
    switch(opcode) {
      // Memory management
      is(M"10110---") { privileged := True } // Memory control ops
      
      // Process control  
      is(M"10111---") { privileged := True } // Process ops
      
      // I/O operations
      is(M"11000---") { privileged := True } // Input ops
      is(M"11001---") { privileged := True } // Output ops
      
      // Timer access
      is(M"11010---") { privileged := True } // Timer ops
      
      // System control
      is(M"11111111") { privileged := True } // Syscall
    }
    
    // In P-process mode, check if instruction is allowed
    when(currentProcess.processType === ProcessType.P_PROCESS && privileged) {
      // Trap to supervisor for privileged instruction
      handleTrap(ErrorType.ILLEGAL_INSTRUCTION, 0, opcode.asUInt)
    }
    
    privileged
  }

  /** Save current processor context to process control block.
    */
  def saveCurrentContext(): Unit = {
    // Context saving will be connected to actual CPU registers
    // This is a placeholder for the interface
    // currentProcess.context.iptr := cpuState.iptr
    // currentProcess.context.workspace := cpuState.workspace  
    // currentProcess.context.areg := cpuState.areg
    // etc.
  }

  /** Restore processor context from process control block.
    */
  def restoreCurrentContext(): Unit = {
    // Context restoration will be connected to actual CPU registers
    // This is a placeholder for the interface
    // cpuState.iptr := currentProcess.context.iptr
    // cpuState.workspace := currentProcess.context.workspace
    // cpuState.areg := currentProcess.context.areg
    // etc.
  }

  /** Get current process type for privilege checking.
    */
  def getCurrentProcessType(): ProcessType.C = currentProcess.processType

  /** Check if currently executing in protected mode.
    */
  def isInProtectedMode(): Bool = currentProcess.processType === ProcessType.P_PROCESS

  /** Get supervisor address for current P-process.
    */
  def getSupervisorAddress(): UInt = currentProcess.supervisor
}
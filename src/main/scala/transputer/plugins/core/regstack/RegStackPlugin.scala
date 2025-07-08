package transputer.plugins.core.regstack

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.Global
import transputer.plugins.core.regstack.RegName

/** Unified register file and stack plugin for T9000 Transputer.
  *
  * Combines register file management with the three-register evaluation stack to eliminate circular
  * dependencies and provide a coherent register state.
  *
  * The T9000 register file includes:
  *   - Areg, Breg, Creg: Three-register evaluation stack
  *   - Oreg: Operand register for instruction decoding
  *   - WdescReg: Workspace descriptor (pointer to current workspace)
  *   - IptrReg: Instruction pointer
  *   - StatusReg: Processor status register
  *   - TimerReg: Timer register
  *   - Other specialized registers per T9000 specification
  */
class RegStackPlugin extends FiberPlugin with RegStackService {
  override def getDisplayName(): String = "RegStackPlugin"
  val version = "RegStackPlugin v1.0 (unified regfile+stack)"
  setName("regStack")

  // Plugin IS the service - no addService() needed
  val elaborationLock = Retainer()

  // Hardware will be created in build phase
  var regStack: RegStackCore = null

  during setup new Area {
    // Setup phase - no hardware creation needed for this plugin
  }

  during build new Area {
    elaborationLock.await()

    // Create unified register file with stack management
    regStack = new RegStackCore()
  }

  // ========================================
  // RegStackService Implementation
  // ========================================

  // Register file interface - safe for null regStack
  def read(reg: SpinalEnumElement[RegName.type], mask: UInt): Bits =
    if (regStack != null) regStack.read(reg, mask) else B"32'h00000000"
  def write(reg: SpinalEnumElement[RegName.type], data: Bits, mask: UInt): Unit =
    if (regStack != null) regStack.write(reg, data, mask)

  // Stack interface - safe for null regStack
  def push(value: UInt): Unit = if (regStack != null) regStack.push(value)
  def pop(): UInt = if (regStack != null) regStack.pop() else U(0, 32 bits)
  def drop(): Unit = if (regStack != null) regStack.drop()
  def dup(): Unit = if (regStack != null) regStack.dup()
  def rev(): Unit = if (regStack != null) regStack.rev()
  def stackEmpty(): Bool = if (regStack != null) regStack.stackEmpty() else True
  def stackFull(): Bool = if (regStack != null) regStack.stackFull() else False

  // Direct register access - safe for null regStack
  def A: UInt = if (regStack != null) regStack.A else U(0, 32 bits)
  def B: UInt = if (regStack != null) regStack.B else U(0, 32 bits)
  def C: UInt = if (regStack != null) regStack.C else U(0, 32 bits)
  def O: UInt = if (regStack != null) regStack.O else U(0, 32 bits)
  def WPtr: UInt = if (regStack != null) regStack.WPtr else U(0, 32 bits)
  def IPtr: UInt = if (regStack != null) regStack.IPtr else U(0, 32 bits)
  def updateRegisters(): Unit = if (regStack != null) regStack.updateRegisters()

  // Workspace memory access - safe for null regStack
  def read(offset: SInt): UInt =
    if (regStack != null) regStack.workspaceRead(offset) else U(0, 32 bits)
  def write(offset: SInt, data: UInt): Unit =
    if (regStack != null) regStack.workspaceWrite(offset, data)

  // Convenience methods for register access
  override def readReg(reg: SpinalEnumElement[RegName.type]): UInt = {
    read(reg, U((1L << 32) - 1, 32 bits)).asUInt
  }
  override def writeReg(reg: SpinalEnumElement[RegName.type], data: UInt): Unit = {
    write(reg, data.asBits, U((1L << 32) - 1, 32 bits))
  }
}

/** Core register file and stack implementation.
  */
class RegStackCore extends Area {

  // ========================================
  // REGISTER FILE STORAGE
  // ========================================

  // Three-register evaluation stack (visible registers)
  val regA = Reg(UInt(Global.WordBits bits)) init 0 // Areg - top of stack
  val regB = Reg(UInt(Global.WordBits bits)) init 0 // Breg - second on stack
  val regC = Reg(UInt(Global.WordBits bits)) init 0 // Creg - third on stack

  // Operand register for instruction decoding
  val regO = Reg(UInt(Global.WordBits bits)) init 0 // Oreg - operand accumulator

  // Memory pointers
  val regWPtr = Reg(UInt(Global.AddrBitsValue bits)) init 0 // Workspace pointer
  val regIPtr = Reg(UInt(Global.AddrBitsValue bits)) init Global.ResetIptr // Instruction pointer

  // System registers
  val regStatus = Reg(UInt(Global.WordBits bits)) init 0 // Status register
  val regTimer = Reg(UInt(Global.WordBits bits)) init 0 // Timer register

  // Stack management
  val stackDepth = Reg(UInt(3 bits)) init 0 // 0-3 indicating number of values in registers

  // Workspace memory (simplified for now)
  val workspace = Mem(UInt(Global.WordBits bits), Global.RamWords)

  // ========================================
  // REGISTER FILE INTERFACE
  // ========================================

  def read(reg: SpinalEnumElement[RegName.type], mask: UInt): Bits = {
    val result = reg.mux(
      RegName.Areg -> regA,
      RegName.Breg -> regB,
      RegName.Creg -> regC,
      RegName.WdescReg -> regWPtr.resized,
      RegName.IptrReg -> regIPtr.resized,
      RegName.StatusReg -> regStatus,
      default -> U(0, Global.WordBits bits)
    )

    (result & mask.resized).asBits
  }

  def write(reg: SpinalEnumElement[RegName.type], data: Bits, mask: UInt): Unit = {
    // Ensure data and mask are properly sized to 32 bits
    val dataResized = data.resize(32)
    val maskResized = mask.resize(32)
    val maskedData = (dataResized.asUInt & maskResized)

    // Use when() statements for enum comparisons
    when(reg === RegName.Areg) {
      regA := maskedData
    }.elsewhen(reg === RegName.Breg) {
      regB := maskedData
    }.elsewhen(reg === RegName.Creg) {
      regC := maskedData
    }.elsewhen(reg === RegName.WdescReg) {
      regWPtr := maskedData.resized
    }.elsewhen(reg === RegName.IptrReg) {
      regIPtr := maskedData.resized
    }.elsewhen(reg === RegName.StatusReg) {
      regStatus := maskedData
    }
  }

  // ========================================
  // STACK INTERFACE
  // ========================================

  def push(value: UInt): Unit = {
    switch(stackDepth) {
      is(0) {
        // Empty stack: A <- value
        regA := value
        stackDepth := 1
      }
      is(1) {
        // One value: B <- A, A <- value
        regB := regA
        regA := value
        stackDepth := 2
      }
      is(2) {
        // Two values: C <- B, B <- A, A <- value
        regC := regB
        regB := regA
        regA := value
        stackDepth := 3
      }
      default {
        // Three values: workspace <- C, C <- B, B <- A, A <- value
        workspaceWrite(S(-1), regC) // Spill C to workspace at WdescReg-1
        regC := regB
        regB := regA
        regA := value
        // Stack depth stays at 3 (registers full)
      }
    }
  }

  def pop(): UInt = {
    val result = regA

    switch(stackDepth) {
      is(1) {
        // One value: result <- A, stack becomes empty
        stackDepth := 0
      }
      is(2) {
        // Two values: result <- A, A <- B
        regA := regB
        stackDepth := 1
      }
      is(3) {
        // Three values: result <- A, A <- B, B <- C
        regA := regB
        regB := regC
        stackDepth := 2
      }
      default {
        // Registers full: result <- A, A <- B, B <- C, C <- workspace
        regA := regB
        regB := regC
        regC := workspaceRead(S(-1))
        // Stack depth stays at 3
      }
    }
    result
  }

  def drop(): Unit = {
    switch(stackDepth) {
      is(1) {
        stackDepth := 0
      }
      is(2) {
        regA := regB
        stackDepth := 1
      }
      is(3) {
        regA := regB
        regB := regC
        stackDepth := 2
      }
      default {
        regA := regB
        regB := regC
        regC := workspaceRead(S(-1))
      }
    }
  }

  def dup(): Unit = {
    push(regA)
  }

  def rev(): Unit = {
    val tempA = regA
    regA := regB
    regB := tempA
  }

  def stackEmpty(): Bool = stackDepth === 0
  def stackFull(): Bool = stackDepth === 3

  // Direct register access
  def A: UInt = regA
  def B: UInt = regB
  def C: UInt = regC
  def O: UInt = regO
  def WPtr: UInt = regWPtr.resized
  def IPtr: UInt = regIPtr.resized

  def updateRegisters(): Unit = {
    // No additional action needed as all updates are immediate
  }

  // ========================================
  // WORKSPACE MEMORY ACCESS
  // ========================================

  def workspaceRead(offset: SInt): UInt = {
    val addr = (regWPtr.asSInt + offset).asUInt.resize(log2Up(Global.RamWords) bits)
    workspace.readSync(addr)
  }

  def workspaceWrite(offset: SInt, data: UInt): Unit = {
    val addr = (regWPtr.asSInt + offset).asUInt.resize(log2Up(Global.RamWords) bits)
    workspace.write(addr, data)
  }
}

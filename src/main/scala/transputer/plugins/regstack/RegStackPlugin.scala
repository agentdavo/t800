package transputer.plugins.regstack

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin}
import transputer.Global
import transputer.plugins.regstack.RegName

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

  // Register file interface
  def read(reg: SpinalEnumElement[RegName.type], mask: UInt): Bits = regStack.read(reg, mask)
  def write(reg: SpinalEnumElement[RegName.type], data: Bits, mask: UInt): Unit =
    regStack.write(reg, data, mask)

  // Stack interface
  def push(value: UInt): Unit = regStack.push(value)
  def pop(): UInt = regStack.pop()
  def drop(): Unit = regStack.drop()
  def dup(): Unit = regStack.dup()
  def rev(): Unit = regStack.rev()
  def stackEmpty(): Bool = regStack.stackEmpty()
  def stackFull(): Bool = regStack.stackFull()

  // Direct register access
  def A: UInt = regStack.A
  def B: UInt = regStack.B
  def C: UInt = regStack.C
  def O: UInt = regStack.O
  def WPtr: UInt = regStack.WPtr
  def IPtr: UInt = regStack.IPtr
  def updateRegisters(): Unit = regStack.updateRegisters()

  // Workspace memory access
  def read(offset: SInt): UInt = regStack.workspaceRead(offset)
  def write(offset: SInt, data: UInt): Unit = regStack.workspaceWrite(offset, data)
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

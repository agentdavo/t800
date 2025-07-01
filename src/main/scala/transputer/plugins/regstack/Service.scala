package transputer.plugins.regstack

import spinal.core._
import transputer.plugins.regstack.RegName

/** Unified service interface for register file and stack operations.
  *
  * This service combines the functionality of both RegfileService and StackService to provide a
  * coherent interface to the T9000 register state and evaluation stack.
  *
  * Register File Operations:
  *   - read/write operations for all T9000 registers
  *   - Mask support for partial register access
  *
  * Stack Operations:
  *   - Three-register evaluation stack (Areg, Breg, Creg)
  *   - Stack manipulation (push, pop, drop, dup, rev)
  *   - Workspace memory access for stack overflow
  *
  * Direct Access:
  *   - Direct access to register values for plugins
  *   - Workspace memory access for LDL/STL operations
  */
trait RegStackService {

  // ========================================
  // REGISTER FILE INTERFACE
  // ========================================

  /** Read from a register with optional masking.
    *
    * @param reg
    *   Register to read from
    * @param mask
    *   Bit mask to apply (0xFFFFFFFF for full read)
    * @return
    *   Masked register value as Bits
    */
  def read(reg: SpinalEnumElement[RegName.type], mask: UInt): Bits

  /** Write to a register with optional masking.
    *
    * @param reg
    *   Register to write to
    * @param data
    *   Data to write
    * @param mask
    *   Bit mask to apply (0xFFFFFFFF for full write)
    */
  def write(reg: SpinalEnumElement[RegName.type], data: Bits, mask: UInt): Unit

  // ========================================
  // STACK INTERFACE
  // ========================================

  /** Push a value onto the three-register evaluation stack. Handles register shifting and workspace
    * spillover.
    */
  def push(value: UInt): Unit

  /** Pop a value from the three-register evaluation stack. Handles register shifting and workspace
    * restore.
    */
  def pop(): UInt

  /** Drop the top value from the stack without returning it. More efficient than pop() when value
    * is not needed.
    */
  def drop(): Unit

  /** Duplicate the top value on the stack. Equivalent to push(A).
    */
  def dup(): Unit

  /** Reverse the top two values on the stack (swap A and B).
    */
  def rev(): Unit

  /** Check if the stack is empty (no values in registers).
    */
  def stackEmpty(): Bool

  /** Check if the stack is full (all three registers contain values).
    */
  def stackFull(): Bool

  // ========================================
  // DIRECT REGISTER ACCESS
  // ========================================

  /** Direct access to the top of stack (Areg).
    */
  def A: UInt

  /** Direct access to second on stack (Breg).
    */
  def B: UInt

  /** Direct access to third on stack (Creg).
    */
  def C: UInt

  /** Direct access to operand register.
    */
  def O: UInt

  /** Direct access to workspace pointer.
    */
  def WPtr: UInt

  /** Direct access to instruction pointer.
    */
  def IPtr: UInt

  /** Update register file from direct register modifications. Called when plugins modify registers
    * directly.
    */
  def updateRegisters(): Unit

  // ========================================
  // WORKSPACE MEMORY ACCESS
  // ========================================

  /** Read from workspace memory at offset from workspace pointer. Used for LDL (Load Local)
    * operations.
    *
    * @param offset
    *   Signed offset from workspace pointer
    * @return
    *   Value at workspace[WPtr + offset]
    */
  def read(offset: SInt): UInt

  /** Write to workspace memory at offset from workspace pointer. Used for STL (Store Local)
    * operations.
    *
    * @param offset
    *   Signed offset from workspace pointer
    * @param data
    *   Value to write
    */
  def write(offset: SInt, data: UInt): Unit

  // ========================================
  // CONVENIENCE HELPERS FOR INSTRUCTION IMPLEMENTATION
  // ========================================

  /** Read a register as UInt with full mask (common case).
    */
  def readReg(reg: SpinalEnumElement[RegName.type]): UInt = {
    read(reg, U((1L << 32) - 1, 32 bits)).asUInt
  }

  /** Write UInt data to a register with full mask (common case).
    */
  def writeReg(reg: SpinalEnumElement[RegName.type], data: UInt): Unit = {
    write(reg, data.asBits, U((1L << 32) - 1, 32 bits))
  }

  /** Standard stack push with register shifting: C <- B, B <- A, A <- value
    */
  def stackPush(value: UInt): Unit = {
    writeReg(RegName.Creg, readReg(RegName.Breg))
    writeReg(RegName.Breg, readReg(RegName.Areg))
    writeReg(RegName.Areg, value)
  }

  /** Standard stack pop with register shifting: A <- B, B <- C
    */
  def stackPop(): Unit = {
    writeReg(RegName.Areg, readReg(RegName.Breg))
    writeReg(RegName.Breg, readReg(RegName.Creg))
  }

  /** Add immediate to register (common for ADC, address calculations).
    */
  def addToReg(reg: SpinalEnumElement[RegName.type], value: UInt): Unit = {
    writeReg(reg, readReg(reg) + value)
  }

  /** Add signed immediate to register (common for jump instructions).
    */
  def addToRegSigned(reg: SpinalEnumElement[RegName.type], value: SInt): Unit = {
    writeReg(reg, (readReg(reg).asSInt + value).asUInt)
  }

  /** Compare register with immediate and store boolean result (for EQC, etc).
    */
  def compareReg(reg: SpinalEnumElement[RegName.type], value: UInt): Unit = {
    writeReg(reg, (readReg(reg) === value).asUInt.resized)
  }
}

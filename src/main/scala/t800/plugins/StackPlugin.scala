package t800.plugins

import spinal.core._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.core.fiber.Retainer
import t800.Global

class StackPlugin extends FiberPlugin {
  private var regA: UInt = null
  private var regB: UInt = null
  private var regC: UInt = null
  private var regO: UInt = null
  private var regWPtr: UInt = null
  private var regIPtr: UInt = null
  private var workspace: Mem[UInt] = null
  private val retain = Retainer()

  during setup new Area {
    regA = Reg(UInt(Global.WORD_BITS bits)) init 0
    regB = Reg(UInt(Global.WORD_BITS bits)) init 0
    regC = Reg(UInt(Global.WORD_BITS bits)) init 0
    regO = Reg(UInt(Global.WORD_BITS bits)) init 0
    regWPtr = Reg(UInt(Global.ADDR_BITS bits)) init 0
    regIPtr = Reg(UInt(Global.IPTR_BITS bits)) init U(Global.RESET_IPTR)
    workspace = Mem(UInt(Global.WORD_BITS bits), Global.RAM_WORDS)

    addService(new StackSrv {
      override val A: UInt = regA
      override val B: UInt = regB
      override val C: UInt = regC
      override val O: UInt = regO
      override val WPtr: UInt = regWPtr
      override val IPtr: UInt = regIPtr
      override def read(offset: SInt): UInt = {
        val addr = (regWPtr.asSInt + offset).asUInt.resize(log2Up(Global.RAM_WORDS) bits)
        workspace.readSync(addr)
      }
      override def write(offset: SInt, data: UInt): Unit = {
        val addr = (regWPtr.asSInt + offset).asUInt.resize(log2Up(Global.RAM_WORDS) bits)
        workspace.write(addr, data)
      }
    })
    retain()
  }
}

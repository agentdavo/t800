package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
import spinal.core.fiber.Retainer
import t800.{Opcodes, Global}
import t800.plugins.{LinkBusSrv, LinkBusArbiterSrv}

/** Implements basic ALU instructions and connects to the global pipeline. */
class PrimaryInstrPlugin extends FiberPlugin {
  private val retain = Retainer()

  during setup new Area {
    println(s"[${PrimaryInstrPlugin.this.getDisplayName()}] setup start")
    retain()
    println(s"[${PrimaryInstrPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${PrimaryInstrPlugin.this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val stack = Plugin[StackSrv]
    val pipe = Plugin[PipelineSrv]
    val mem = Plugin[LinkBusSrv]
    val arb = Plugin[LinkBusArbiterSrv]

    val inst = pipe.execute(Global.OPCODE)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = stack.O | nibble.resized
    val primary = Opcodes.Enum.Primary()
    primary.assignFromBits(inst(7 downto 4))

    arb.exeRd.valid := False
    arb.exeRd.payload.addr := U(0)
    arb.exeWr.valid := False
    arb.exeWr.payload.addr := U(0)
    arb.exeWr.payload.data := B(0, Global.WORD_BITS bits)

    switch(primary) {
      is(Opcodes.Enum.Primary.PFIX) {
        stack.O := (accumulated << 4).resized
      }
      is(Opcodes.Enum.Primary.NFIX) {
        stack.O := ((~accumulated) << 4).resized
      }
      is(Opcodes.Enum.Primary.LDC) {
        val operand = accumulated.asSInt
        stack.C := stack.B
        stack.B := stack.A
        stack.A := operand.asUInt
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.LDL) {
        val operand = accumulated.asSInt
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.read(operand)
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.STL) {
        val operand = accumulated.asSInt
        stack.write(operand, stack.A)
        stack.A := stack.B
        stack.B := stack.C
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.LDLP) {
        val operand = accumulated
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.WPtr + operand
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.LDNLP) {
        stack.A := stack.A + (stack.O |<< 2)
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.ADC) {
        val operand = accumulated
        stack.A := stack.A + operand
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.EQC) {
        val operand = accumulated
        stack.A := (stack.A === operand).asUInt.resized
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.J) {
        val operand = accumulated.asSInt
        stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.CJ) {
        val operand = accumulated.asSInt
        when(stack.A === 0) {
          stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        } otherwise {
          stack.A := stack.B
          stack.B := stack.C
        }
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.LDNL) {
        val addr = stack.A + accumulated
        arb.exeRd.valid := True
        arb.exeRd.payload.addr := addr.resized
        pipe.execute.haltWhen(!mem.rdRsp.valid)
        when(pipe.execute.down.isFiring) {
          stack.A := mem.rdRsp.payload.asUInt
          stack.O := 0
        }
      }
      is(Opcodes.Enum.Primary.STNL) {
        val addr = stack.A + accumulated
        arb.exeWr.valid := True
        arb.exeWr.payload.addr := addr.resized
        arb.exeWr.payload.data := stack.B.asBits
        when(pipe.execute.down.isFiring) {
          stack.A := stack.C
          stack.O := 0
        }
      }
      is(Opcodes.Enum.Primary.CALL) {
        val operand = accumulated.asSInt
        stack.write(S(-1), stack.C)
        stack.write(S(-2), stack.B)
        stack.write(S(-3), stack.A)
        stack.write(S(-4), stack.IPtr + 1)
        stack.WPtr := (stack.WPtr.asSInt - 4).asUInt
        stack.A := stack.IPtr + 1
        stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        stack.O := 0
      }
      is(Opcodes.Enum.Primary.AJW) {
        val operand = accumulated.asSInt
        stack.WPtr := (stack.WPtr.asSInt + operand).asUInt
        stack.O := 0
      }
    }
    println(s"[${PrimaryInstrPlugin.this.getDisplayName()}] build end")
  }
}

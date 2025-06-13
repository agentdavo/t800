package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import t800.{Global, PrimaryOp, SecondaryOp}

/** Instruction fetch unit using the pipeline framework. */
class FetchPlugin extends FiberPlugin {
  val logic = during build new Area {
    println(s"[${FetchPlugin.this.getDisplayName()}] build start")
    implicit val h: PluginHost = host
    val imem = Plugin[InstrFetchSrv]
    val pipe = Plugin[PipelineSrv]
    val stack = Plugin[StackSrv]

    imem.cmd.valid := True
    val addr = stack.IPtr
    imem.cmd.payload.addr := (addr >> 2).resized
    pipe.fetch.haltWhen(!imem.rsp.valid)
    when(pipe.fetch.down.isFiring) { stack.IPtr := stack.IPtr + 1 }
    val shift = addr(1 downto 0) * U(Global.OPCODE_BITS)
    val inst = (imem.rsp.payload >> shift)(Global.OPCODE_BITS - 1 downto 0)
    pipe.fetch(Global.IPTR) := addr
    pipe.fetch(Global.OPCODE) := inst
    println(s"[${FetchPlugin.this.getDisplayName()}] build end")
  }
}

package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}

/** Instruction fetch unit using the pipeline framework. */
class FetchPlugin extends FiberPlugin {
  val logic = during build new Area {
    implicit val h: PluginHost = host
    val imem = Plugin[InstrFetchSrv]
    val pipe = Plugin[PipelineSrv]
    val stack = Plugin[StackSrv]

    imem.cmd.valid := True
    val addr = stack.IPtr
    imem.cmd.payload.addr := (addr >> 2).resized
    pipe.fetch.haltWhen(!imem.rsp.valid)
    when(pipe.fetch.down.isFiring) { stack.IPtr := stack.IPtr + 1 }
    val shift = addr(1 downto 0) * 8
    pipe.fetch(pipe.INSTR) := (imem.rsp.payload >> shift)(7 downto 0)
  }
}

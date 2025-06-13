package t800.plugins

import spinal.core._
import spinal.lib._
import t800.TConsts

/** Instruction fetch unit using the pipeline framework. */
class FetchPlugin extends FiberPlugin {
  override def build(): Unit = {
    implicit val h: PluginHost = host
    val imem = Plugin[InstrBusSrv]
    val pipe = Plugin[PipelineSrv]
    val stack = Plugin[StackSrv]

    imem.cmd.valid := True
    imem.cmd.payload.addr := stack.IPtr
    pipe.fetch.haltWhen(!imem.rsp.valid)
    when(pipe.fetch.down.isFiring) { stack.IPtr := stack.IPtr + 1 }
    pipe.fetch(pipe.INSTR) := imem.rsp.payload(7 downto 0)
  }
}

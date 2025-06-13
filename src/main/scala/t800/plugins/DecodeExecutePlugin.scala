package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import t800.plugins._

class DecodeExecutePlugin extends FiberPlugin {

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val stack = Plugin[StackSrv]
    val pipe = Plugin[PipelineSrv]

    val opcode = pipe.decode.insert(pipe.decode(pipe.INSTR).asUInt)

    when(pipe.execute.down.isFiring) {
      switch(pipe.execute(opcode)) {
        is(U(0x30, 8 bits)) { // REV
          val tmp = stack.A
          stack.A := stack.B
          stack.B := tmp
        }
        is(U(0x94, 8 bits)) { // ADD placeholder
          val sum = stack.A + stack.B
          stack.A := sum
          stack.B := stack.C
        }
      }
    }
  }
}

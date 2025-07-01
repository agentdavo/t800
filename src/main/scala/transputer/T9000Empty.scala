package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.database.Database

/** Empty T9000 for testing basic generation. */
class T9000Empty extends Component {
  // Create minimal T9000 parameters
  val param = T9000Param(
    enableFpu = false,
    enableScheduler = false,
    enableTimers = false,
    enableVcp = false,
    enableMmu = false,
    enablePmi = false
  )

  // Create system bus
  val systemBus = master(Bmb(T9000Transputer.systemBusParam(param)))

  // Idle the bus
  systemBus.cmd.valid := False
  systemBus.cmd.opcode := 0
  systemBus.cmd.address := 0
  systemBus.cmd.length := 0
  systemBus.cmd.data := 0
  systemBus.cmd.mask := 0
  systemBus.cmd.last := True
  systemBus.cmd.source := 0
  systemBus.cmd.context := 0
  systemBus.rsp.ready := True

  // Minimal I/O
  val io = new Bundle {
    val test = out Bool ()
  }

  io.test := True
}

/** Generate empty T9000. */
object T9000EmptyVerilog {
  def main(args: Array[String]): Unit = {
    println("Generating empty T9000...")
    val report = SpinalVerilog(new T9000Empty)
    println(s"SUCCESS! Empty T9000 generated: ${report.toplevelName}")
  }
}

package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.database.Database

/** Simple T9000 system with on-chip RAM for testing. */
class T9000WithRam(param: T9000Param = T9000Param()) extends Component {
  val db = T9000Transputer.configureDatabase(param)

  // I/O
  val io = new Bundle {
    val interrupts = in Bits (8 bits)
    val cpuClk = in Bool ()
    val reset = in Bool ()
  }

  // Create the T9000 core with the database context
  val t9000 = new Area {
    val core = Database(db).on {
      val cpu = T9000Transputer(param)
      cpu.io.interrupts := io.interrupts
      cpu.io.cpuClk := io.cpuClk
      cpu.io.reset := io.reset
      cpu
    }
  }

  // Create simple on-chip RAM
  val ram = new Area {
    val size = 256 * 1024 // 256KB
    val mem = Mem(Bits(128 bits), size / 16) // 128-bit wide memory

    // BMB slave interface
    val bus = slave(Bmb(t9000.core.systemBus.p))

    // Connect to T9000's system bus
    bus << t9000.core.systemBus

    // Handle BMB protocol
    bus.cmd.ready := True

    val wordAddress = bus.cmd.address >> 4 // 128-bit word address
    val isWrite = bus.cmd.opcode === Bmb.Cmd.Opcode.WRITE

    when(bus.cmd.fire && isWrite) {
      mem.write(wordAddress, bus.cmd.data)
    }

    // Read response (1 cycle latency)
    val rspValid = RegNext(bus.cmd.fire && !isWrite) init False
    val rspAddress = RegNext(wordAddress)

    bus.rsp.valid := rspValid
    bus.rsp.payload.data := mem.readSync(rspAddress)
    bus.rsp.payload.last := True
    bus.rsp.payload.source := 0
  }
}

/** Generate T9000 with RAM. */
object T9000WithRamVerilog {
  def main(args: Array[String]): Unit = {
    val param = T9000Param()
    val spinalConfig = SpinalConfig(
      targetDirectory = "./generated"
    )

    println("Generating T9000 with on-chip RAM...")
    val report = spinalConfig.generateVerilog(new T9000WithRam(param))
    println(s"Verilog generated: ${report.toplevelName}")
    println(s"Output files in: ./generated")
  }
}

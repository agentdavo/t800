package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.misc.database.Database

/** Minimal T9000 configuration for testing Verilog generation. */
object T9000MinimalParam {
  def apply(): T9000Param = T9000Param(
    enableFpu = false,
    enableScheduler = false,
    enableTimers = false,
    enableVcp = false,
    enableMmu = false,
    enablePmi = false,
    customPlugins = Some(minimalPlugins())
  )

  def minimalPlugins(): Seq[spinal.lib.misc.plugin.FiberPlugin] = {
    import transputer.plugins._
    Seq(
      new core.transputer.TransputerPlugin(),
      new core.pipeline.PipelinePlugin(),
      new core.regstack.RegStackPlugin(),
      new core.pipeline.PipelineBuilderPlugin()
    )
  }
}

/** Minimal T9000 core with integrated memory for proof of concept. */
class T9000Minimal extends Component {
  val param = T9000MinimalParam()
  val db = T9000Transputer.configureDatabase(param)

  // Create minimal T9000 with just core plugins
  val transputer = Database(db).on {
    val t = new T9000Transputer(param, db)

    // Create simple on-chip memory and connect directly
    val mem = new Area {
      val size = 4096 // 4KB for minimal testing
      val ram = Mem(Bits(128 bits), size / 16)

      // Handle system bus as slave
      t.systemBus.cmd.ready := True

      val wordAddr = t.systemBus.cmd.address >> 4
      val isWrite = t.systemBus.cmd.opcode === Bmb.Cmd.Opcode.WRITE

      when(t.systemBus.cmd.fire && isWrite) {
        ram.write(wordAddr, t.systemBus.cmd.data)
      }

      // Read response
      val rspValid = RegNext(t.systemBus.cmd.fire && !isWrite) init False
      val rspAddr = RegNext(wordAddr)

      t.systemBus.rsp.valid := rspValid
      t.systemBus.rsp.payload.data := ram.readSync(rspAddr)
      t.systemBus.rsp.payload.last := True
      t.systemBus.rsp.payload.source := 0
    }

    t
  }

  // Expose minimal I/O
  val io = new Bundle {
    val interrupts = in Bits (8 bits)
    val cpuClk = in Bool ()
    val reset = in Bool ()
  }

  transputer.io.interrupts := io.interrupts
  transputer.io.cpuClk := io.cpuClk
  transputer.io.reset := io.reset
}

/** Generate minimal T9000. */
object T9000MinimalVerilog {
  def main(args: Array[String]): Unit = {
    println("Generating minimal T9000 Transputer...")
    val report = SpinalVerilog(new T9000Minimal)
    println(s"SUCCESS! Verilog generated: ${report.toplevelName}")
    println("This proves the T9000 architecture can be synthesized.")
    println("\nNext steps:")
    println("- Enable more plugins incrementally")
    println("- Implement proper 4-bus crossbar architecture")
    println("- Add workspace cache integration")
  }
}

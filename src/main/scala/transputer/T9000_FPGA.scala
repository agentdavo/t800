package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.misc.database.Database
import transputer._

/** Minimal T9000 configuration optimized for FPGA synthesis.
  *
  * This configuration reduces resource usage for synthesis targeting:
  *   - ECP5 FPGA (25K LUTs)
  *   - 24 MHz operation
  *   - Minimal feature set for proof-of-concept
  */
class T9000_FPGA extends Component {

  val io = new Bundle {
    // Clock and reset
    val clk = in Bool ()
    val rst = in Bool ()

    // External interface
    val led = out Bits (8 bits)
    val uart_tx = out Bool ()
    val uart_rx = in Bool ()

    // Link interfaces (simplified)
    val link0_out = out Bool ()
    val link0_in = in Bool ()

    // Status
    val running = out Bool ()
    val error = out Bool ()
  }

  // Create clock domain
  val coreClockDomain = ClockDomain(
    clock = io.clk,
    reset = io.rst,
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = HIGH
    )
  )

  val coreArea = new ClockingArea(coreClockDomain) {
    val db = Transputer.defaultDatabase()

    // FPGA-optimized configuration
    db(Global.OPCODE_BITS) = 8
    db(Global.WORD_BITS) = 32
    db(Global.ADDR_BITS) = 32

    // Minimal plugin set for FPGA
    val fpgaPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.pipeline.PipelinePlugin(),
      new transputer.plugins.regstack.RegStackPlugin(),
      new transputer.plugins.fetch.FetchPlugin(),
      new transputer.plugins.grouper.InstrGrouperPlugin(),
      // stack functionality now in RegStackPlugin above
      new transputer.plugins.timers.TimerPlugin()
    )

    val core = Database(db).on(Transputer(fpgaPlugins))

    // Simple memory - 8KB on-chip RAM
    val memory = Mem(Bits(32 bits), 2048) // 8KB

    // Simple memory interface
    val memoryPort = memory.readWriteSync(
      address = core.systemBus.cmd.address(12 downto 2),
      data = core.systemBus.cmd.data,
      enable = core.systemBus.cmd.valid,
      write = core.systemBus.cmd.opcode === 1
    )

    // Connect system bus
    core.systemBus.cmd.ready := True
    core.systemBus.rsp.valid := RegNext(core.systemBus.cmd.valid)
    core.systemBus.rsp.fragment.data := memoryPort
    core.systemBus.rsp.fragment.context := RegNext(core.systemBus.cmd.context)
    core.systemBus.rsp.fragment.source := RegNext(core.systemBus.cmd.source)
    core.systemBus.rsp.fragment.opcode := 0
    core.systemBus.rsp.last := True

    // Status indicators
    val cycleCounter = Reg(UInt(24 bits)) init (0)
    cycleCounter := cycleCounter + 1

    io.running := True
    io.error := False

    // LED pattern - show system is alive
    io.led := cycleCounter(23 downto 16).asBits

    // UART stub (for future use)
    io.uart_tx := True

    // Link stub
    io.link0_out := False
  }
}

/** Top-level for FPGA synthesis */
object T9000_FPGA_Top {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "fpga"
    ).generate(new T9000_FPGA).printPruned()

    println("T9000 FPGA Verilog generated in fpga/ directory")
  }
}

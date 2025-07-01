package transputer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/** Simple FPGA test design using existing BootRomDesign as base.
  *
  * This bypasses the hanging elaboration issues by using a working design.
  */
class SimpleFPGA extends Component {
  val io = new Bundle {
    // Clock and reset (will be connected externally)
    val led = out Bits (8 bits)
    val uart_tx = out Bool ()
    val uart_rx = in Bool ()
    val running = out Bool ()
    val error = out Bool ()
  }

  // Use the working BootRomDesign as the core
  val bootRom = new BootRomDesign(
    romFile = "bootrom.hex",
    romSize = 0x1000
  )

  // Simple LED pattern - show system is alive
  val counter = Reg(UInt(24 bits)) init (0)
  counter := counter + 1

  io.led := counter(23 downto 16).asBits
  io.running := True
  io.error := False
  io.uart_tx := True
}

object SimpleFPGA_Top {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "fpga"
    ).generate(new SimpleFPGA)

    println("Simple FPGA Verilog generated successfully!")
  }
}

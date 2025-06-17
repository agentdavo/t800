package transputer

import spinal.core._
import spinal.lib._
import transputer.plugins.transputer.TransputerPlugin

/** Configuration for the Transputer board core. */
case class CoreConfig(coreFrequency: IClockDomainFrequency)

/** Default board configuration object. */

object CoreConfig {
  // Provide a default configuration
  
  def default: CoreConfig = {
    // Default configuration values
    val config = CoreConfig(coreFrequency = FixedFrequency(250 MHz)) // Align with T9000 target
    // Return the default configuration
    config
  }
  
}

/** Default board configuration with clock source for T9000 FPU. */

class TransputerBoard(boardCfg: CoreConfig) extends Component {
  val io = new Bundle {
    val reset = in Bool
    val boardClk = in Bool
    val boardClkLocked = in Bool
  }.setName("")

  val clkCtrl = new Area {
    // Internal clock domain with synchronous reset and rising edge
    val globalClockConfig = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = SYNC,
      resetActiveLevel = HIGH
    )
    val coreClockDomain = ClockDomain.internal("core", frequency = FixedFrequency(boardCfg.coreFrequency), config = globalClockConfig)

    // Connect external board clock
    coreClockDomain.clock := io.boardClk

    // Synchronous reset with PLL lock check
    coreClockDomain.reset := RegNext(io.reset || (!io.boardClkLocked), init = True, clock = coreClockDomain.clock)
  }

  // Application-specific clocking area
  val coreArea = new ClockingArea(clkCtrl.coreClockDomain) {
    // Instantiate T9000 core with default plugins
    val t9000Core = new Transputer(Transputer.defaultPlugins())
    t9000Core.io.reset := clkCtrl.coreClockDomain.reset
    t9000Core.io.clock := clkCtrl.coreClockDomain.clock

    // Placeholder for future I/O expansion
  }
}

/** Verilog generation with export to multiple folders. */
object TransputerBoardVerilog {
  
  def main(args: Array[String]): Unit = {
    val db = Transputer.defaultDatabase()
    println("[Transputer] Create the Transputer for a Generic Board")

    // Generate Verilog with export to two folders
    val spinalConfig = SpinalConfig(
      mergeAsyncProcess = true, // Helps Verilator avoid combinatorial loops
      targetDirectory = "gen/src/verilog" // Primary export folder
    )
    val report = spinalConfig.generateVerilog {
      new TransputerBoard(CoreConfig.default)
    }
    println(s"Verilog generated in gen/src/verilog: ${report.toplevelName}")
    
}

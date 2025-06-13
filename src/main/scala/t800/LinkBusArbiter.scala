package t800

import spinal.core._
import spinal.lib._
import t800.{MemReadCmd, MemWriteCmd}

class LinkBusArbiter extends Component {
  val io = new Bundle {
    val exeRd = in(Flow(t800.MemReadCmd()))
    val exeWr = in(Flow(t800.MemWriteCmd()))
    val chanRd = in(Flow(t800.MemReadCmd()))
    val chanWr = in(Flow(t800.MemWriteCmd()))
    val rdOut = out(Flow(t800.MemReadCmd()))
    val wrOut = out(Flow(t800.MemWriteCmd()))
  }

  // Read arbitration: execute gets priority over channel
  io.rdOut.valid := io.exeRd.valid || io.chanRd.valid
  io.rdOut.payload := io.exeRd.payload
  when(!io.exeRd.valid && io.chanRd.valid) {
    io.rdOut.payload := io.chanRd.payload
  }

  // Write arbitration: execute gets priority over channel
  io.wrOut.valid := io.exeWr.valid || io.chanWr.valid
  io.wrOut.payload := io.exeWr.payload
  when(!io.exeWr.valid && io.chanWr.valid) {
    io.wrOut.payload := io.chanWr.payload
  }
}

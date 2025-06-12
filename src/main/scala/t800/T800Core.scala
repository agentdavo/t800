// ============================================================================
//  T800Core.scala   (compile with SpinalHDL ≥ 1.9)
//  ----------------------------------------------------------------------------
//  Skeleton of a Transputer T800/T9000-style core:
//    • integer pipeline  • three-deep eval-stack (A,B,C) + W & O
//    • on-chip dual-port RAM (workspace / code)
//    • bootstrap ROM (16 words preset by parameter)
//    • micro-coded control store (read-only stub)
//    • FPU shell (Adder / Mul / Div√ pipes, shared rounder)
//    • process scheduler, timer, channel I/O, error/exception, debug JTAG
//  ----------------------------------------------------------------------------
//  Build  :  sbt "runMain T800CoreVerilog"
//  Sim    :  sbt test:runMain T800CoreSim
// ============================================================================

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable

// ------------------------------------------------------------
//  Top-level constants
// ------------------------------------------------------------
object TConsts {
  val WordBits    = 32
  val AddrBits    = 32
  val RomWords    = 16
  val RamWords    = 4096        // 4 Ki × 32-bit = 16 KiB (like real T800)
  val MicroWords  = 1024        // 1 K × 32-bit micro-ROM (placeholder)
  val LinkCount   = 4
  val ResetPC     = 0x0000_0000L
}

// ============================================================
//  Top-level component
// ============================================================
class T800Core extends Component {
  import TConsts._

  // ----------------------- I/O bundle -----------------------
  val io = new Bundle {
    // simple external 32-bit synchronous memory bus
    val mem = new Bundle {
      val cmd    = out(Bits(2 bits))     // 00-NOP  01-RD  10-WR
      val addr   = out(UInt(AddrBits bits))
      val wdata  = out(UInt(WordBits bits))
      val rdata  = in (UInt(WordBits bits))
      val ready  = in (Bool)
    }

    // four channel links (1-bit LVDS style pins)
    val links = Vec(master(new Bundle{
      val din    = in (Bits(1 bits))
      val dout   = out(Bits(1 bits))
      val strobe = out(Bool)
      val credit = in (Bool)
    }), LinkCount)

    // asynchronous interrupt
    val intReq   = in (Bool)
    val errorOut = out(Bool)

    // JTAG debug pins (stripped-down)
    val dbgTCK   = in (Bool)
    val dbgTMS   = in (Bool)
    val dbgTDI   = in (Bool)
    val dbgTDO   = out(Bool)
  }

  // -------------------- on-chip memories --------------------
  val dpram = Mem(UInt(WordBits bits), RamWords)        // dual-port SRAM
  val bootRomVec = Vector.tabulate(RomWords)(i => U(0xF0F0F0F0 ^ i)) // demo
  val bootRom = Mem(UInt(WordBits bits), bootRomVec)    // small boot ROM

  // micro-coded control store (stubbed with zeros)
  val microRom = Mem(Bits(32 bits), MicroWords) init(Vector.fill(MicroWords)(B(0, 32 bits)))

  // ------------------ architectural registers --------------
  val WPtr  = Reg(UInt(AddrBits bits)) init(U(0))
  val Areg  = Reg(UInt(WordBits bits)) init(U(0))
  val Breg  = Reg(UInt(WordBits bits)) init(U(0))
  val Creg  = Reg(UInt(WordBits bits)) init(U(0))
  val Oreg  = Reg(UInt(WordBits bits)) init(U(0))        // operand builder

  // ------------------------ sub-modules ---------------------
  val fetch    = new FetchUnit
  val decode   = new DecodeUnit
  val execute  = new ExecuteUnit
  val fpu      = new FPUnit
  val sched    = new Scheduler
  val timer    = new TimerUnit
  val chan     = new ChannelUnit
  val trapU    = new TrapUnit
  val dbg      = new DebugJTAG

  //  ---- pipeline hand-off ----
  fetch.io.out        <> decode.io.in
  decode.io.out       <> execute.io.in

  //  ---- memory path arbitration ----
  execute.io.mem      <> io.mem          // core → external
  execute.io.dpram    <> dpram           // core → on-chip RAM
  fetch.io.bootRom    <> bootRom         // fetch reads ROM on reset

  //  ---- FPU & other units ----
  execute.io.fpuReq   <> fpu.io.cmd
  execute.io.fpuRsp   <> fpu.io.rsp

  execute.io.sched    <> sched.io.ctrl
  execute.io.timer    <> timer.io.ctrl
  execute.io.chanReq  <> chan.io.req
  chan.io.rsp         <> execute.io.chanRsp
  chan.io.links       <> io.links

  execute.io.trap     <> trapU.io.ctrl
  trapU.io.errorOut   <> io.errorOut

  dbg.io.*            // wildcard tie-off inside the debug unit

  // ==========================================================
  //              Fetch  ►  Decode  ►  Execute
  // ==========================================================
  // ----------------------------------------------------------
  //  Fetch stage – 8-bit byte stream with simple burst cache
  // ----------------------------------------------------------
  class FetchUnit extends Component {
    val io = new Bundle {
      val out     = master(Flow(UInt(8 bits)))
      val bootRom = master(bootRom.getPort) // ROM port 0
    }

    val pc        = Reg(UInt(AddrBits bits)) init(U(ResetPC))
    val lineBuf   = Reg(UInt(WordBits bits))
    val bufValid  = Reg(Bool) init(False)

    // ROM read on reset region 0x0000_0000 .. ROMWords*4-1
    val inBoot    = (pc < RomWords * 4)
    io.bootRom.readAddress := pc(AddrBits-1 downto 2)      // word-aligned

    io.out.valid   := False
    io.out.payload := 0

    when(!bufValid) {
      lineBuf  := io.bootRom.readData
      bufValid := True
    } otherwise {
      // little-end first (INMOS ordering)
      io.out.valid   := True
      io.out.payload := lineBuf(7 downto 0)
      lineBuf        := (U(0) ## lineBuf(WordBits-1 downto 8))
      pc             := pc + 1
      when(pc(1 downto 0) === 3) { bufValid := False }
    }
  }

  // ----------------------------------------------------------
  //  Decode stage – just a latency-insulating Buffer
  // ----------------------------------------------------------
  class DecodeUnit extends Component {
    val io = new Bundle {
      val in  = slave (Flow(UInt(8 bits)))
      val out = master(Flow(UInt(8 bits)))
    }
    io.out <> io.in
  }

  // ----------------------------------------------------------
  //  Execute stage – full 256-way opcode dispatcher (all stubs)
  // ----------------------------------------------------------
  class ExecuteUnit extends Component {
    val io = new Bundle {
      val in      = slave (Flow(UInt(8 bits)))

      // external & internal memory
      val mem     = master (io.mem.clone)
      val dpram   = master (dpram.getPort)           // dual-port RAM A

      // FPU
      val fpuReq  = master (Flow(new FpCmd))
      val fpuRsp  = slave  (Flow(UInt(WordBits bits)))

      // misc. units
      val sched   = master (Flow(Bits(32 bits)))
      val timer   = master (Flow(Bits(32 bits)))
      val chanReq = master (Flow(Bits(32 bits)))
      val chanRsp = slave  (Flow(Bits(32 bits)))
      val trap    = master (Flow(Bits(32 bits)))
    }

    // Zero default strobes
    io.mem.cmd   := B"2'b00"
    io.mem.addr  := 0
    io.mem.wdata := 0
    io.dpram.cmd := M2S(false)
    io.fpuReq.valid := False
    io.sched.valid  := False
    io.timer.valid  := False
    io.chanReq.valid:= False
    io.trap.valid   := False

    //--------------------------------------------------------
    //  Main single-cycle dispatch
    //--------------------------------------------------------
    when(io.in.valid){
      switch(io.in.payload){
        //----------------------------------------------------
        //  0x00 .. 0x0F   J 0-15
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0x00 + i, 8 bits)){ /* TODO: relative branch i */ }
        }

        //----------------------------------------------------
        //  0x10 .. 0x2F  LDL/STL
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0x10 + i, 8 bits)){ /* TODO: LDL i */ }
          is(U(0x20 + i, 8 bits)){ /* TODO: STL i */ }
        }

        //----------------------------------------------------
        //  0x30 .. 0x3F  OPR n (secondary ops)
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0x30 + i, 8 bits)){ /* TODO: OPR secondary i */ }
        }

        //----------------------------------------------------
        //  0x40 .. 0x7F  LDNL/STNL/LDNLP/CT/CJ group
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0x40 + i, 8 bits)){ /* TODO: LDNL i */ }
          is(U(0x50 + i, 8 bits)){ /* TODO: STNL i */ }
          is(U(0x60 + i, 8 bits)){ /* TODO: LDNLP i */ }
          is(U(0x70 + i, 8 bits)){ /* TODO: conditional test/jump i */ }
        }

        //----------------------------------------------------
        //  0x80 .. 0x8F  Core floating-point ops
        //----------------------------------------------------
        is(U"8'h80"){ /* FPADD */ io.fpuReq.valid := True; io.fpuReq.payload := FpCmd("FPADD", Areg, Breg) /* TODO */ }
        is(U"8'h81"){ /* FPSUB */ /* TODO */ }
        is(U"8'h82"){ /* FPMUL */ /* TODO */ }
        is(U"8'h83"){ /* FPDIV */ /* TODO */ }
        // 0x84-0x8F ...
        for(i <- 4 until 16){
          is(U(0x80 + i, 8 bits)){ /* TODO: other fp */ }
        }

        //----------------------------------------------------
        //  0x90 .. 0x9F  process / timer / channel control
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0x90 + i, 8 bits)){ /* TODO: control group */ }
        }

        //----------------------------------------------------
        //  0xA0 .. 0xAF  channel word extract/insert etc.
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0xA0 + i, 8 bits)){ /* TODO */ }
        }

        //----------------------------------------------------
        //  0xB0 .. 0xBF  block move / set
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0xB0 + i, 8 bits)){ /* TODO */ }
        }

        //----------------------------------------------------
        //  0xC0 .. 0xCF  transcendental fp
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0xC0 + i, 8 bits)){ /* TODO transcendental */ }
        }

        //----------------------------------------------------
        //  0xD0 .. 0xDF  PFIX / NFIX
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0xD0 + i, 8 bits)){ // PFIX nibble i
            Oreg := (Oreg |<< 4) | U(i, 4 bits).resize(WordBits)
          }
          is(U(0xE0 + i, 8 bits)){ // NFIX nibble i
            Oreg := (Oreg |<< 4) | U(i, 4 bits).resize(WordBits)
            Oreg(WordBits-1) := True     // mark sign; final sign-extend later
          }
        }

        //----------------------------------------------------
        //  0xE0 .. 0xEF  long immediates (already parsed above)
        //----------------------------------------------------

        //----------------------------------------------------
        //  0xF0 .. 0xFF  impl-specific / debug
        //----------------------------------------------------
        for(i <- 0 until 16){
          is(U(0xF0 + i, 8 bits)){ /* TODO vendor-specific */ }
        }
      } // switch
    } // when
  } // ExecuteUnit

  // ----------------------------------------------------------
  //             Simple FPU shell (three pipes)
  // ----------------------------------------------------------
  class FpCmd(opBits: Int = 3) extends Bundle {
    val op   = Bits(opBits bits)
    val opa  = UInt(WordBits bits)
    val opb  = UInt(WordBits bits)
    // constructor helper
    def this(name:String,a:UInt,b:UInt) = { this(); op := name match {
      case "FPADD" => B"3'b000"; case "FPSUB" => B"3'b001"
      case "FPMUL" => B"3'b010"; case "FPDIV" => B"3'b011"
      case _       => B"3'b111" } ; opa := a ; opb := b }
  }

  class FPUnit extends Component {
    val io = new Bundle {
      val cmd = slave (Flow(new FpCmd))
      val rsp = master(Flow(UInt(WordBits bits)))
    }
    io.cmd.ready := True          // always accept (back-pressure to be added)
    io.rsp.valid := False
    io.rsp.payload := U(0)

    // TODO: addPipe, mulPipe, divPipe, merge & rounder
  }

  // ----------------------------------------------------------
  //  Scheduler – high/low ready queues (stub)
  // ----------------------------------------------------------
  class Scheduler extends Component {
    val io = new Bundle { val ctrl = slave(Flow(Bits(32 bits))) }
    // TODO: queues, current process descriptor
  }

  // ----------------------------------------------------------
  //  Timer – 64-bit counter + two compare regs (stub)
  // ----------------------------------------------------------
  class TimerUnit extends Component {
    val io = new Bundle { val ctrl = slave(Flow(Bits(32 bits))) }
    // TODO
  }

  // ----------------------------------------------------------
  //  Channel link block – four link engines (stub)
  // ----------------------------------------------------------
  class ChannelUnit extends Component {
    val io = new Bundle {
      val req   = slave(Flow(Bits(32 bits)))
      val rsp   = master(Flow(Bits(32 bits)))
      val links = master(Vec(io.links(0).cloneType, LinkCount))
    }
    io.links.foreach { l => l.dout := 0; l.strobe := False }
    io.rsp.valid := False
    io.rsp.payload := B(0, 32 bits)
    // TODO: per-link fifo, 4-phase handshake
  }

  // ----------------------------------------------------------
  //  Trap / error unit (stub)
  // ----------------------------------------------------------
  class TrapUnit extends Component {
    val io = new Bundle {
      val ctrl     = slave(Flow(Bits(32 bits)))
      val errorOut = out Bool
    }
    io.errorOut := False
    // TODO: error classification, pipeline redirect
  }

  // ----------------------------------------------------------
  //  Debug JTAG (placeholder, tied-off)
  // ----------------------------------------------------------
  class DebugJTAG extends Component {
    val io = new Bundle {
      val tck = in Bool
      val tms = in Bool
      val tdi = in Bool
      val tdo = out Bool
    }
    io.tdo := False
  }
}

// ============================================================
//  Verilog elaboration target
// ============================================================
object T800CoreVerilog {
  def main(args: Array[String]):Unit = {
    SpinalVerilog(new T800Core)
  }
}

// ============================================================
//  Tiny behavioural simulation (prints first 32 opcodes)
// ============================================================
object T800CoreSim {
  def main(args:Array[String]):Unit = {
    SimConfig.withWave.compile(new T800Core).doSim{ dut =>
      dut.clockDomain.forkStimulus(10)

      // run for 150 cycles so we see a few fetches
      for(_ <- 0 until 150) { dut.clockDomain.waitSampling() }
    }
  }
}

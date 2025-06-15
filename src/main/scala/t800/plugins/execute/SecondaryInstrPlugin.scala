package t800.plugins.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{Plugin, PluginHost}
import spinal.core.fiber.Retainer
import t800.{Opcodes, Global}
import t800.plugins.{ChannelSrv, ChannelTxCmd, LinkBusSrv, LinkBusArbiterSrv}
import t800.plugins.schedule.SchedSrv
import t800.plugins.fpu.{FpuSrv, FpOp}
import t800.plugins.timers.TimerSrv
import t800.plugins.pipeline.PipelineSrv
import scala.util.Try

/** Implements basic ALU instructions and connects to the global pipeline. */
class SecondaryInstrPlugin extends FiberPlugin {
  val version = "SecondaryInstrPlugin v0.1"
  private var errReg: Bool = null
  private var haltErr: Bool = null
  private var hiFPtr, hiBPtr, loFPtr, loBPtr: UInt = null
  private var move2dLen, move2dStride, move2dAddr: UInt = null
  private var saveLPhase, saveHPhase: Bool = null
  private val retain = Retainer()

  during setup new Area {
    report(L"Initializing $version")
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] setup start")
    errReg = Reg(Bool()) init (False)
    haltErr = Reg(Bool()) init (False)
    hiFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    hiBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    move2dLen = Reg(UInt(Global.WORD_BITS bits)) init (0)
    move2dStride = Reg(UInt(Global.WORD_BITS bits)) init (0)
    move2dAddr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    saveLPhase = Reg(Bool()) init (False)
    saveHPhase = Reg(Bool()) init (False)
    retain()
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val stack = Plugin[StackSrv]
    val pipe = Plugin[PipelineSrv]
    val mem = Plugin[LinkBusSrv]
    val arb = Plugin[LinkBusArbiterSrv]
    val timer = Plugin[TimerSrv]
    val fpu = Plugin[FpuSrv]
    val linksOpt = Try(Plugin[ChannelSrv]).toOption
    val dmaOpt = Try(Plugin[ChannelDmaSrv]).toOption
    val dummy = new ChannelSrv {
      override def txReady(link: UInt): Bool = False
      override def push(link: UInt, data: Bits): Bool = False
      override def rxValid(link: UInt): Bool = False
      override def rxPayload(link: UInt): Bits = B(0, Global.WORD_BITS bits)
      override def rxAck(link: UInt): Unit = {}
    }
    val links = linksOpt.getOrElse(dummy)
    val dummyDma = new ChannelDmaSrv { def cmd = Stream(ChannelTxCmd()).setIdle() }
    val dma = dmaOpt.getOrElse(dummyDma)
    val sched = Plugin[SchedSrv]

    val inst = pipe.execute(Global.OPCODE)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = stack.O | nibble.resized
    val primary = Opcodes.Enum.Primary()
    primary.assignFromBits(inst(7 downto 4))

    arb.exeRd.valid := False
    arb.exeRd.payload.addr := U(0)
    arb.exeWr.valid := False
    arb.exeWr.payload.addr := U(0)
    arb.exeWr.payload.data := B(0, Global.WORD_BITS bits)
    sched.newProc.valid := False
    sched.newProc.payload.ptr := U(0)
    sched.newProc.payload.high := False
    dma.cmd.valid := False

    switch(primary) {
      is(Opcodes.Enum.Primary.OPR) {
        val operand = accumulated.asSInt
        val secondary = Opcodes.Enum.Secondary()
        secondary.assignFromBits(accumulated(7 downto 0).asBits)
        switch(secondary) {
          is(Opcodes.Enum.Secondary.REV) {
            val tmp = stack.A
            stack.A := stack.B
            stack.B := tmp
          }
          is(Opcodes.Enum.Secondary.ADD) {
            val sum = stack.A + stack.B
            stack.A := sum
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.SUB) {
            val diff = stack.B - stack.A
            stack.A := diff
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.LADD) {
            val res = stack.B + stack.A + (stack.C & 1)
            stack.A := res.resized
          }
          is(Opcodes.Enum.Secondary.LSUB) {
            val res = stack.B - stack.A - (stack.C & 1)
            stack.A := res.resized
          }
          is(Opcodes.Enum.Secondary.LSUM) {
            val wide = UInt(33 bits)
            wide := stack.B.resize(33) + stack.A.resize(33) + stack.C(0).asUInt
            stack.A := wide(31 downto 0)
            val carry = wide >= U(0xffffffffL, 33 bits)
            stack.B := carry.asUInt.resized
          }
          is(Opcodes.Enum.Secondary.LDIFF) {
            val res = UInt(33 bits)
            res := (stack.B.resize(33) - stack.A.resize(33) - stack.C(0).asUInt.resize(33))
            stack.A := res(31 downto 0)
            stack.B := res(32).asUInt.resize(32)
          }
          is(Opcodes.Enum.Secondary.LMUL) {
            val product = (stack.B * stack.A).resize(64) + stack.C.resize(64)
            stack.A := product(31 downto 0)
            stack.B := product(63 downto 32)
          }
          is(Opcodes.Enum.Secondary.LDIV) {
            when(stack.C >= stack.A) {
              errReg := True
            } otherwise {
              val dividend = (stack.C ## stack.B).asUInt.resize(64)
              val quotient = dividend / stack.A
              val rem = dividend % stack.A
              stack.A := quotient(31 downto 0)
              stack.B := rem(31 downto 0)
            }
          }
          is(Opcodes.Enum.Secondary.AND) {
            val res = stack.A & stack.B
            stack.A := res
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.XOR) {
            val res = stack.A ^ stack.B
            stack.A := res
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.SHL) {
            val res = stack.B |<< stack.A
            stack.A := res
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.SHR) {
            val res = (stack.B.asSInt >> stack.A).asUInt
            stack.A := res
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.LSHR) {
            when(stack.A < 64) {
              val wide = (stack.C ## stack.B).asUInt.resize(64)
              val shifted = wide >> stack.A
              stack.A := shifted(31 downto 0)
              stack.B := shifted(63 downto 32)
            } otherwise {
              stack.A := 0
              stack.B := 0
            }
          }
          is(Opcodes.Enum.Secondary.LSHL) {
            when(stack.A < 64) {
              val wide = (stack.C ## stack.B).asUInt.resize(64)
              val shifted = wide |<< stack.A
              stack.A := shifted(31 downto 0)
              stack.B := shifted(63 downto 32)
            } otherwise {
              stack.A := 0
              stack.B := 0
            }
          }
          is(Opcodes.Enum.Secondary.IN) {
            val idx = stack.B(1 downto 0)
            val avail = links.rxValid(idx)
            pipe.execute.haltWhen(!avail)
            when(pipe.execute.down.isFiring) {
              stack.A := links.rxPayload(idx).asUInt
              links.rxAck(idx)
            }
          }
          is(Opcodes.Enum.Secondary.OUT) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.RET) {
            stack.IPtr := stack.read(S(0))
            stack.WPtr := stack.WPtr + 4
          }
          is(Opcodes.Enum.Secondary.STARTP) {
            sched.newProc.valid := True
            sched.newProc.payload.ptr := stack.A
            sched.newProc.payload.high := False
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.RUNP) {
            sched.enqueue(stack.A, False)
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.TESTERR) {
            val tmp = errReg
            errReg := False
            stack.C := stack.B
            stack.B := stack.A
            stack.A := tmp.asUInt.resized
          }
          is(Opcodes.Enum.Secondary.ALT) {
            // ALT setup not yet implemented
          }
          is(Opcodes.Enum.Secondary.ALTWT) {
            val waitDone = timer.hi >= stack.A
            when(!waitDone) { sched.enqueue(stack.WPtr, False) }
            pipe.execute.haltWhen(!waitDone)
          }
          is(Opcodes.Enum.Secondary.ALTEND) {
            // ALTEND placeholder
          }
          is(Opcodes.Enum.Secondary.STLB) {
            loBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.STHB) {
            hiBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.STLF) {
            loFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.STHF) {
            hiFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.SAVEL) {
            val addr = Mux(saveLPhase, loBPtr, loFPtr)
            arb.exeWr.valid := True
            arb.exeWr.payload.addr := addr.resized
            arb.exeWr.payload.data := Mux(saveLPhase, sched.loBack, sched.loFront).asBits
            pipe.execute.haltWhen(False)
            when(pipe.execute.down.isFiring) {
              when(!saveLPhase) {
                saveLPhase := True
              } otherwise {
                saveLPhase := False
                stack.A := stack.B
                stack.B := stack.C
              }
            }
          }
          is(Opcodes.Enum.Secondary.SAVEH) {
            val addr = Mux(saveHPhase, hiBPtr, hiFPtr)
            arb.exeWr.valid := True
            arb.exeWr.payload.addr := addr.resized
            arb.exeWr.payload.data := Mux(saveHPhase, sched.hiBack, sched.hiFront).asBits
            pipe.execute.haltWhen(False)
            when(pipe.execute.down.isFiring) {
              when(!saveHPhase) {
                saveHPhase := True
              } otherwise {
                saveHPhase := False
                stack.A := stack.B
                stack.B := stack.C
              }
            }
          }
          is(Opcodes.Enum.Secondary.STOPP) {
            sched.enqueue(stack.WPtr, False)
            pipe.execute.haltWhen(True)
          }
          is(Opcodes.Enum.Secondary.ENDP) {
            val parent = stack.read(S(0))
            val cnt = stack.read(S(-1)) - 1
            stack.write(S(-1), cnt)
            when(cnt === 0) {
              sched.terminateCurrent()
            } otherwise {
              sched.enqueue(parent, False)
            }
          }
          is(Opcodes.Enum.Secondary.MINT) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := U(0x80000000L)
          }
          is(Opcodes.Enum.Secondary.STTIMER) {
            timer.set(stack.A)
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.LDTIMER) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := timer.hi
          }
          is(Opcodes.Enum.Secondary.TIMERDISABLEH) {
            timer.disableHi()
          }
          is(Opcodes.Enum.Secondary.TIMERDISABLEL) {
            timer.disableLo()
          }
          is(Opcodes.Enum.Secondary.TIMERENABLEH) {
            timer.enableHi()
          }
          is(Opcodes.Enum.Secondary.TIMERENABLEL) {
            timer.enableLo()
          }
          is(Opcodes.Enum.Secondary.LEND) {
            val cnt = stack.A - 1
            stack.A := cnt
            when(cnt =/= 0) {
              sched.enqueue(stack.WPtr, False)
              pipe.execute.haltWhen(True)
            }
          }
          is(Opcodes.Enum.Secondary.CLRHALTERR) {
            haltErr := False
          }
          is(Opcodes.Enum.Secondary.SETHALTERR) {
            haltErr := True
          }
          is(Opcodes.Enum.Secondary.TESTHALTERR) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := haltErr.asUInt.resized
          }
          is(Opcodes.Enum.Secondary.DUP) {
            stack.C := stack.B
            stack.B := stack.A
          }
          is(Opcodes.Enum.Secondary.MOVE2DINIT) {
            move2dLen := stack.A
            move2dStride := stack.B
            move2dAddr := stack.C
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Enum.Secondary.MOVE2DALL) {
            dma.cmd.valid := True
            dma.cmd.payload.link := stack.B(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := stack.A
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * stack.A
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.MOVE2DNONZERO) {
            dma.cmd.valid := True
            dma.cmd.payload.link := stack.B(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := stack.A
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * stack.A
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.MOVE2DZERO) {
            dma.cmd.valid := True
            dma.cmd.payload.link := stack.B(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := stack.A
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * stack.A
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.POP) {
            val t = stack.A
            stack.A := stack.B
            stack.B := stack.C
            stack.C := t
          }
          is(Opcodes.Enum.Secondary.LB) {
            val addr = stack.A
            arb.exeRd.valid := True
            arb.exeRd.payload.addr := (addr >> 2).resized
            pipe.execute.haltWhen(!mem.rdRsp.valid)
            when(pipe.execute.down.isFiring) {
              val shift = addr(1 downto 0) * 8
              val byte = (mem.rdRsp.payload >> shift).resize(8)
              stack.A := byte.asUInt.resize(Global.WORD_BITS)
            }
          }
          is(Opcodes.Enum.Secondary.OUTBYTE) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.OUTWORD) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.MOVE) {
            val dma = Plugin[ChannelDmaSrv]
            dma.cmd.valid := True
            dma.cmd.payload.link := stack.B(1 downto 0)
            dma.cmd.payload.addr := stack.C
            dma.cmd.payload.length := stack.A
            dma.cmd.payload.stride := 0
            dma.cmd.payload.rows := 0
            dma.cmd.payload.twoD := False
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.LDPI) {
            stack.A := stack.A + stack.IPtr
          }
          is(Opcodes.Enum.Secondary.FPADD) {
            fpu.send(FpOp.ADD, stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.FPSUB) {
            fpu.send(FpOp.SUB, stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.FPMUL) {
            fpu.send(FpOp.MUL, stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(Opcodes.Enum.Secondary.FPDIV) {
            fpu.send(FpOp.DIV, stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
        }
        when(secondary.asBits === Opcodes.Secondary.TIN) {
          val waitDone = timer.hi >= stack.A
          when(!waitDone) { sched.enqueue(stack.WPtr, False) }
          pipe.execute.haltWhen(!waitDone)
        }
        stack.O := 0
      }
    }
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] build end")
  }
}

package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
import spinal.core.fiber.Retainer
import t800.{Global, PrimaryOp, SecondaryOp}
import t800.plugins.{ChannelSrv, SchedSrv, ChannelTxCmd, LinkBusSrv, LinkBusArbiterSrv}
import scala.util.Try

/** Implements basic ALU instructions and connects to the global pipeline. */
class ExecutePlugin extends FiberPlugin {
  private var errReg: Bool = null
  private var haltErr: Bool = null
  private var hiFPtr, hiBPtr, loFPtr, loBPtr: UInt = null
  private val retain = Retainer()

  during setup new Area {
    println(s"[${ExecutePlugin.this.getDisplayName()}] setup start")
    errReg = Reg(Bool()) init (False)
    haltErr = Reg(Bool()) init (False)
    hiFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    hiBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    loBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    retain()
    println(s"[${ExecutePlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${ExecutePlugin.this.getDisplayName()}] build start")
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
    val primary = PrimaryOp.fromBits(inst(7 downto 4))
    val secondary = SecondaryOp.fromBits(inst)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = stack.O | nibble.resized

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
      is(PrimaryOp.PFIX) {
        stack.O := (accumulated << 4).resized
      }
      is(PrimaryOp.NFIX) {
        stack.O := ((~accumulated) << 4).resized
      }
      is(PrimaryOp.OPR) {
        switch(secondary) {
          is(SecondaryOp.REV) {
            val tmp = stack.A
            stack.A := stack.B
            stack.B := tmp
          }
          is(SecondaryOp.ADD) {
            val sum = stack.A + stack.B
            stack.A := sum
            stack.B := stack.C
          }
          is(SecondaryOp.SUB) {
            val diff = stack.B - stack.A
            stack.A := diff
            stack.B := stack.C
          }
          is(SecondaryOp.AND) {
            val res = stack.A & stack.B
            stack.A := res
            stack.B := stack.C
          }
          is(SecondaryOp.XOR) {
            val res = stack.A ^ stack.B
            stack.A := res
            stack.B := stack.C
          }
          is(SecondaryOp.SHL) {
            val res = stack.B |<< stack.A
            stack.A := res
            stack.B := stack.C
          }
          is(SecondaryOp.SHR) {
            val res = (stack.B.asSInt >> stack.A).asUInt
            stack.A := res
            stack.B := stack.C
          }
          is(SecondaryOp.IN) {
            val idx = stack.B(1 downto 0)
            val avail = links.rxValid(idx)
            pipe.execute.haltWhen(!avail)
            when(pipe.execute.down.isFiring) {
              stack.A := links.rxPayload(idx).asUInt
              links.rxAck(idx)
            }
          }
          is(SecondaryOp.OUT) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(SecondaryOp.RET) {
            stack.IPtr := stack.read(S(0))
            stack.WPtr := stack.WPtr + 4
          }
          is(SecondaryOp.STARTP) {
            sched.newProc.valid := True
            sched.newProc.payload.ptr := stack.A
            sched.newProc.payload.high := False
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.TESTERR) {
            val tmp = errReg
            errReg := False
            stack.C := stack.B
            stack.B := stack.A
            stack.A := tmp.asUInt.resized
          }
          is(SecondaryOp.ALT) {
            // Placeholder for ALT state machine
          }
          is(SecondaryOp.ALTWT) {
            // Placeholder
          }
          is(SecondaryOp.ALTEND) {
            // Placeholder
          }
          is(SecondaryOp.STLB) {
            loBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.STHB) {
            hiBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.STLF) {
            loFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.STHF) {
            hiFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.MINT) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := U(0x80000000L)
          }
          is(SecondaryOp.STTIMER) {
            timer.set(stack.A)
            stack.A := stack.B
            stack.B := stack.C
          }
          is(SecondaryOp.LDTIMER) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := timer.hi
          }
          is(SecondaryOp.TIMERDISABLEH) {
            timer.disableHi()
          }
          is(SecondaryOp.TIMERDISABLEL) {
            timer.disableLo()
          }
          is(SecondaryOp.TIMERENABLEH) {
            timer.enableHi()
          }
          is(SecondaryOp.TIMERENABLEL) {
            timer.enableLo()
          }
          is(SecondaryOp.CLRHALTERR) {
            haltErr := False
          }
          is(SecondaryOp.SETHALTERR) {
            haltErr := True
          }
          is(SecondaryOp.TESTHALTERR) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := haltErr.asUInt.resized
          }
          is(SecondaryOp.DUP) {
            stack.C := stack.B
            stack.B := stack.A
          }
          is(SecondaryOp.POP) {
            val t = stack.A
            stack.A := stack.B
            stack.B := stack.C
            stack.C := t
          }
          is(SecondaryOp.LB) {
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
          is(SecondaryOp.OUTBYTE) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(SecondaryOp.OUTWORD) {
            val idx = stack.B(1 downto 0)
            val ready = links.push(idx, stack.A.asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(SecondaryOp.MOVE) {
            val dma = Plugin[ChannelDmaSrv]
            dma.cmd.valid := True
            dma.cmd.payload.link := stack.B(1 downto 0)
            dma.cmd.payload.addr := stack.C
            dma.cmd.payload.length := stack.A
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(SecondaryOp.LDPI) {
            stack.A := stack.A + stack.IPtr
          }
          is(SecondaryOp.FPADD) {
            fpu.send(B"3'b000", stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(SecondaryOp.FPSUB) {
            fpu.send(B"3'b001", stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(SecondaryOp.FPMUL) {
            fpu.send(B"3'b010", stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
          is(SecondaryOp.FPDIV) {
            fpu.send(B"3'b011", stack.A, stack.B)
            pipe.execute.haltWhen(!fpu.resultValid)
            when(pipe.execute.down.isFiring) {
              stack.A := fpu.result
              stack.B := stack.C
            }
          }
        }
        stack.O := 0
      }
      is(PrimaryOp.LDC) {
        val operand = accumulated.asSInt
        stack.C := stack.B
        stack.B := stack.A
        stack.A := operand.asUInt
        stack.O := 0
      }
      is(PrimaryOp.LDL) {
        val operand = accumulated.asSInt
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.read(operand)
        stack.O := 0
      }
      is(PrimaryOp.STL) {
        val operand = accumulated.asSInt
        stack.write(operand, stack.A)
        stack.A := stack.B
        stack.B := stack.C
        stack.O := 0
      }
      is(PrimaryOp.LDLP) {
        val operand = accumulated
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.WPtr + operand
        stack.O := 0
      }
      is(PrimaryOp.ADC) {
        val operand = accumulated
        stack.A := stack.A + operand
        stack.O := 0
      }
      is(PrimaryOp.EQC) {
        val operand = accumulated
        stack.A := (stack.A === operand).asUInt.resized
        stack.O := 0
      }
      is(PrimaryOp.J) {
        val operand = accumulated.asSInt
        stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        stack.O := 0
      }
      is(PrimaryOp.CJ) {
        val operand = accumulated.asSInt
        when(stack.A === 0) {
          stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        } otherwise {
          stack.A := stack.B
          stack.B := stack.C
        }
        stack.O := 0
      }
      is(PrimaryOp.LDNL) {
        val addr = stack.A + accumulated
        arb.exeRd.valid := True
        arb.exeRd.payload.addr := addr.resized
        pipe.execute.haltWhen(!mem.rdRsp.valid)
        when(pipe.execute.down.isFiring) {
          stack.A := mem.rdRsp.payload.asUInt
          stack.O := 0
        }
      }
      is(PrimaryOp.STNL) {
        val addr = stack.A + accumulated
        arb.exeWr.valid := True
        arb.exeWr.payload.addr := addr.resized
        arb.exeWr.payload.data := stack.B.asBits
        when(pipe.execute.down.isFiring) {
          stack.A := stack.C
          stack.O := 0
        }
      }
      is(PrimaryOp.CALL) {
        val operand = accumulated.asSInt
        stack.write(S(-1), stack.C)
        stack.write(S(-2), stack.B)
        stack.write(S(-3), stack.A)
        stack.write(S(-4), stack.IPtr + 1)
        stack.WPtr := (stack.WPtr.asSInt - 4).asUInt
        stack.A := stack.IPtr + 1
        stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        stack.O := 0
      }
      is(PrimaryOp.AJW) {
        val operand = accumulated.asSInt
        stack.WPtr := (stack.WPtr.asSInt + operand).asUInt
        stack.O := 0
      }
    }
    println(s"[${ExecutePlugin.this.getDisplayName()}] build end")
  }
}

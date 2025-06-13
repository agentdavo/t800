package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import t800.{Opcodes, TConsts}
import t800.plugins.{ChannelSrv, DataBusSrv, SchedSrv}
import scala.util.Try

/** Implements basic ALU instructions and connects to the global pipeline. */
class ExecutePlugin extends FiberPlugin {
  private var errReg: Bool = null
  private var haltErr: Bool = null
  private var hiFPtr, hiBPtr, loFPtr, loBPtr: UInt = null

  override def setup(): Unit = {
    errReg = Reg(Bool()) init (False)
    haltErr = Reg(Bool()) init (False)
    hiFPtr = Reg(UInt(TConsts.WordBits bits)) init (0)
    hiBPtr = Reg(UInt(TConsts.WordBits bits)) init (0)
    loFPtr = Reg(UInt(TConsts.WordBits bits)) init (0)
    loBPtr = Reg(UInt(TConsts.WordBits bits)) init (0)
  }

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val stack = Plugin[StackSrv]
    val pipe = Plugin[PipelineSrv]
    val mem = Plugin[DataBusSrv]
    val timer = Plugin[TimerSrv]
    val linksOpt = Try(Plugin[ChannelSrv]).toOption
    val dummyRx = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    val dummyTx = Vec.fill(TConsts.LinkCount)(Stream(Bits(TConsts.WordBits bits)))
    dummyRx.foreach(_.setIdle())
    dummyTx.foreach(_.setIdle())
    val links = linksOpt.getOrElse(new ChannelSrv {
      override def rx = dummyRx
      override def tx = dummyTx
    })
    val sched = Plugin[SchedSrv]

    val inst = pipe.execute(pipe.INSTR)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = stack.O | nibble.resized
    val primary = inst(7 downto 4)

    mem.rdCmd.valid := False
    mem.rdCmd.payload.addr := U(0)
    mem.wrCmd.valid := False
    mem.wrCmd.payload.addr := U(0)
    mem.wrCmd.payload.data := B(0, TConsts.WordBits bits)
    sched.newProc.valid := False
    sched.newProc.payload.ptr := U(0)
    sched.newProc.payload.high := False

    links.rx.foreach(_.ready := False)
    links.tx.foreach(_.valid := False)

    switch(primary) {
      is(Opcodes.Primary.PFIX) {
        stack.O := (accumulated << 4).resized
      }
      is(Opcodes.Primary.NFIX) {
        stack.O := ((~accumulated) << 4).resized
      }
      is(Opcodes.Primary.OPR) {
        val operand = accumulated.asSInt
        val secondary = accumulated(7 downto 0).asBits
        switch(secondary) {
          is(Opcodes.Secondary.REV) {
            val tmp = stack.A
            stack.A := stack.B
            stack.B := tmp
          }
          is(Opcodes.Secondary.ADD) {
            val sum = stack.A + stack.B
            stack.A := sum
            stack.B := stack.C
          }
          is(Opcodes.Secondary.IN) {
            val idx = stack.B(1 downto 0)
            links.rx(idx).ready := False
            pipe.execute.haltWhen(!links.rx(idx).valid)
            when(pipe.execute.down.isFiring) {
              stack.A := links.rx(idx).payload.asUInt
              links.rx(idx).ready := True
            }
          }
          is(Opcodes.Secondary.OUT) {
            val idx = stack.B(1 downto 0)
            links.tx(idx).valid := True
            links.tx(idx).payload := stack.A.asBits
            pipe.execute.haltWhen(!links.tx(idx).ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Secondary.RET) {
            stack.IPtr := stack.read(S(0))
            stack.WPtr := stack.WPtr + 4
          }
          is(Opcodes.Secondary.STARTP) {
            sched.newProc.valid := True
            sched.newProc.payload.ptr := stack.A
            sched.newProc.payload.high := False
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.TESTERR) {
            val tmp = errReg
            errReg := False
            stack.C := stack.B
            stack.B := stack.A
            stack.A := tmp.asUInt.resized
          }
          is(Opcodes.Secondary.ALT) {
            // Placeholder for ALT state machine
          }
          is(Opcodes.Secondary.ALTWT) {
            // Placeholder
          }
          is(Opcodes.Secondary.ALTEND) {
            // Placeholder
          }
          is(Opcodes.Secondary.STLB) {
            loBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.STHB) {
            hiBPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.STLF) {
            loFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.STHF) {
            hiFPtr := stack.A
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.MINT) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := U(0x80000000L)
          }
          is(Opcodes.Secondary.STTIMER) {
            timer.set(stack.A)
            stack.A := stack.B
            stack.B := stack.C
          }
          is(Opcodes.Secondary.LDTIMER) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := timer.hi
          }
          is(Opcodes.Secondary.CLRHALTERR) {
            haltErr := False
          }
          is(Opcodes.Secondary.SETHALTERR) {
            haltErr := True
          }
          is(Opcodes.Secondary.TESTHALTERR) {
            stack.C := stack.B
            stack.B := stack.A
            stack.A := haltErr.asUInt.resized
          }
          is(Opcodes.Secondary.DUP) {
            stack.C := stack.B
            stack.B := stack.A
          }
          is(Opcodes.Secondary.POP) {
            val t = stack.A
            stack.A := stack.B
            stack.B := stack.C
            stack.C := t
          }
          is(Opcodes.Secondary.LB) {
            val addr = stack.A
            mem.rdCmd.valid := True
            mem.rdCmd.payload.addr := (addr >> 2).resized
            pipe.execute.haltWhen(!mem.rdRsp.valid)
            when(pipe.execute.down.isFiring) {
              val shift = addr(1 downto 0) * 8
              val byte = (mem.rdRsp.payload >> shift).resize(8)
              stack.A := byte.asUInt.resize(TConsts.WordBits)
            }
          }
          is(Opcodes.Secondary.OUTBYTE) {
            val idx = stack.B(1 downto 0)
            links.tx(idx).valid := True
            links.tx(idx).payload := stack.A.asBits
            pipe.execute.haltWhen(!links.tx(idx).ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Secondary.OUTWORD) {
            val idx = stack.B(1 downto 0)
            links.tx(idx).valid := True
            links.tx(idx).payload := stack.A.asBits
            pipe.execute.haltWhen(!links.tx(idx).ready)
            when(pipe.execute.down.isFiring) {
              stack.A := stack.B
              stack.B := stack.C
            }
          }
          is(Opcodes.Secondary.LDPI) {
            stack.A := stack.A + stack.IPtr
          }
        }
        stack.O := 0
      }
      is(Opcodes.Primary.LDC) {
        val operand = accumulated
        stack.C := stack.B
        stack.B := stack.A
        stack.A := operand
        stack.O := 0
      }
      is(Opcodes.Primary.LDL) {
        val operand = accumulated.asSInt
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.read(operand)
        stack.O := 0
      }
      is(Opcodes.Primary.STL) {
        val operand = accumulated.asSInt
        stack.write(operand, stack.A)
        stack.A := stack.B
        stack.B := stack.C
        stack.O := 0
      }
      is(Opcodes.Primary.LDLP) {
        val operand = accumulated
        stack.C := stack.B
        stack.B := stack.A
        stack.A := stack.WPtr + operand
        stack.O := 0
      }
      is(Opcodes.Primary.ADC) {
        val operand = accumulated
        stack.A := stack.A + operand
        stack.O := 0
      }
      is(Opcodes.Primary.EQC) {
        val operand = accumulated
        stack.A := (stack.A === operand).asUInt.resized
        stack.O := 0
      }
      is(Opcodes.Primary.J) {
        val operand = accumulated.asSInt
        stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        stack.O := 0
      }
      is(Opcodes.Primary.CJ) {
        val operand = accumulated.asSInt
        when(stack.A === 0) {
          stack.IPtr := (stack.IPtr.asSInt + operand).asUInt
        } otherwise {
          stack.A := stack.B
          stack.B := stack.C
        }
        stack.O := 0
      }
      is(Opcodes.Primary.LDNL) {
        val addr = stack.A + accumulated
        mem.rdCmd.valid := True
        mem.rdCmd.payload.addr := addr.resized
        pipe.execute.haltWhen(!mem.rdRsp.valid)
        when(pipe.execute.down.isFiring) {
          stack.A := mem.rdRsp.payload.asUInt
          stack.O := 0
        }
      }
      is(Opcodes.Primary.STNL) {
        val addr = stack.A + accumulated
        mem.wrCmd.valid := True
        mem.wrCmd.payload.addr := addr.resized
        mem.wrCmd.payload.data := stack.B.asBits
        when(pipe.execute.down.isFiring) {
          stack.A := stack.C
          stack.O := 0
        }
      }
      is(Opcodes.Primary.CALL) {
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
      is(Opcodes.Primary.AJW) {
        val operand = accumulated.asSInt
        stack.WPtr := (stack.WPtr.asSInt + operand).asUInt
        stack.O := 0
      }
    }
  }
}

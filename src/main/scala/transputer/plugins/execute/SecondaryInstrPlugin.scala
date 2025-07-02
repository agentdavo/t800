package transputer.plugins.execute

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.core.fiber.{Retainer, _}
import scala.util.Try
import transputer.Opcode
import transputer.Global
import transputer.plugins.arithmetic.{AluService, AluOp}
import transputer.plugins.{
  ChannelService,
  ChannelTxCmd,
  LinkBusService,
  LinkBusArbiterService,
  ChannelDmaService
}
import transputer.plugins.schedule.{SchedService, SchedCmd, ProcessState}
import transputer.plugins.core.regstack.{RegStackService, RegName}
import transputer.plugins.fpu.{FpuService, FpOp, FpCmd}
import transputer.plugins.timers.TimerService
import transputer.plugins.core.pipeline.PipelineStageService
// import transputer.plugins.core.cache.CacheAccessService // Not yet implemented
import transputer.plugins.legacy.mmu.{ConfigAccessService, TrapHandlerService}
import transputer.plugins.legacy.vcp.{VcpService, MemWriteCmd, MemReadCmd}
import scala.util.Try

/** Implements basic ALU instructions and connects to the global pipeline. */
class SecondaryInstrPlugin extends FiberPlugin {
  val version = "SecondaryInstrPlugin v0.1"

  during setup new Area {
    report(L"Initializing $version")
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] setup start")
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] build start")

    // Create registers in build phase where component context is available
    val errReg = Reg(Bool()) init (False)
    val haltErr = Reg(Bool()) init (False)
    val hiFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val hiBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val loFPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val loBPtr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val move2dLen = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val move2dStride = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val move2dAddr = Reg(UInt(Global.WORD_BITS bits)) init (0)
    val saveLPhase = Reg(Bool()) init (False)
    val saveHPhase = Reg(Bool()) init (False)
    implicit val h: PluginHost = host
    val regStack = Plugin[RegStackService]
    val pipe = Plugin[PipelineStageService]
    val memOpt = Try(Plugin[LinkBusService]).toOption
    val arbOpt = Try(Plugin[LinkBusArbiterService]).toOption
    val timerOpt = Try(Plugin[TimerService]).toOption
    val fpuOpt = Try(Plugin[FpuService]).toOption
    val schedOpt = Try(Plugin[SchedService]).toOption

    // Create stub services when not available for basic compilation
    val mem = memOpt.getOrElse(new LinkBusService {
      def rdCmd = Flow(transputer.MemReadCmd()).setIdle()
      def rdRsp = Flow(Bits(32 bits)).setIdle()
      def wrCmd = Flow(transputer.MemWriteCmd()).setIdle()
    })
    val arb = arbOpt.getOrElse(new LinkBusArbiterService {
      def exeRd = Flow(transputer.MemReadCmd()).setIdle()
      def exeWr = Flow(transputer.MemWriteCmd()).setIdle()
      def chanRd = Flow(transputer.MemReadCmd()).setIdle()
      def chanWr = Flow(transputer.MemWriteCmd()).setIdle()
    })
    val timer = timerOpt.getOrElse(new TimerService {
      def clockReg0: UInt = U(0, 32 bits)
      def clockReg1: UInt = U(0, 32 bits)
      def clockReg0Enable: Bool = False
      def clockReg1Enable: Bool = False
      def setClockReg0(value: UInt): Unit = {}
      def setClockReg1(value: UInt): Unit = {}
      def enableClockReg0(): Unit = {}
      def disableClockReg0(): Unit = {}
      def enableClockReg1(): Unit = {}
      def disableClockReg1(): Unit = {}
      def clockReg0After(time: UInt): Bool = False
      def clockReg1After(time: UInt): Bool = False
      def updateRegisters(): Unit = {}
    })
    val fpu = fpuOpt.getOrElse(new FpuService {
      def pipe = Flow(FpCmd()).setIdle()
      def rsp = Flow(UInt(64 bits)).setIdle()
      def FPA: Bits = B(0, 64 bits)
      def FPB: Bits = B(0, 64 bits)
      def FPC: Bits = B(0, 64 bits)
      def FPStatus: Bits = B(0, 32 bits)
      def roundingMode: Bits = B(0, 2 bits)
      def errorFlags: Bits = B(0, 5 bits)
      def isBusy: Bool = False
      def setRoundingMode(mode: Bits): Unit = {}
      def updateRegisters(): Unit = {}
    })
    val linksOpt = Try(Plugin[ChannelService]).toOption
    val dmaOpt = Try(Plugin[ChannelDmaService]).toOption
    // val cacheOpt = Try(Plugin[CacheAccessService]).toOption
    val cacheOpt = None
    val configOpt = Try(Plugin[ConfigAccessService]).toOption
    val trapOpt = Try(Plugin[TrapHandlerService]).toOption
    val vcpOpt = Try(Plugin[VcpService]).toOption
    val dummy = new ChannelService {
      override def txReady(link: UInt): Bool = False
      override def push(link: UInt, data: Bits): Bool = False
      override def rxValid(link: UInt): Bool = False
      override def rxPayload(link: UInt): Bits = B(0, Global.WORD_BITS bits)
      override def rxAck(link: UInt): Unit = {}
    }
    val links = linksOpt.getOrElse(dummy)
    val dummyDma = new ChannelDmaService { def cmd = Stream(ChannelTxCmd()).setIdle() }
    val dma = dmaOpt.getOrElse(dummyDma)
    // val dummyCache = new CacheAccessService {
    //   def req = Stream(MemWriteCmd()).setIdle()
    //   def rsp = Flow(Bits(32 bits)).setIdle()
    // }
    // val cache = cacheOpt.getOrElse(dummyCache)
    val dummyConfig = new ConfigAccessService {
      val addr = Bits(16 bits).assignDontCare()
      val data = Bits(32 bits).assignDontCare()
      val writeEnable = Bool().assignDontCare()
      val isValid = Bool().assignDontCare()
      val significantBits = Bits(5 bits).assignDontCare()
      def read(addr: Bits, width: Int): Bits = B(0, 32 bits)
      def write(addr: Bits, data: Bits, width: Int): Unit = {}
    }
    val config = configOpt.getOrElse(dummyConfig)
    val dummyTrap = new TrapHandlerService {
      override val trapAddr = Bits(32 bits).assignDontCare()
      override val trapType = Bits(4 bits).assignDontCare()
      override val trapEnable = Bool().assignDontCare()
      override val trapHandlerAddr = Bits(32 bits).assignDontCare()
      override def setTrap(addr: Bits, typ: Bits): Unit = {}
      override def clearTrap(): Unit = {}
    }
    val trap = trapOpt.getOrElse(dummyTrap)
    val dummyVcp = new VcpService {
      def txReady(link: UInt): Bool = False
      def push(link: UInt, data: Bits): Bool = False
      def rxValid(link: UInt): Bool = False
      def rxPayload(link: UInt): Bits = B(0, 32 bits)
      def rxAck(link: UInt): Unit = {}
      def scheduleInput(channel: Int): Unit = {}
      def scheduleOutput(channel: Int): Unit = {}
      def getChannelState(channel: Int): Bits = B(0, 8 bits)
      def setVcpCommand(cmd: Bits): Unit = {}
      def getVcpStatus(): Bits = B(0, 32 bits)
      def sendPacket(channel: Int, data: Bits, isEnd: Bool): Unit = {}
      def receiveAck(channel: Int): Bool = False
      def enqueueMessage(channel: Int, data: Bits): Unit = {}
      override def updateChannelHeader(channel: Int, header: Bits): Unit = {}
      override def vcpStatus: Bits = B(0, 32 bits)
      override def channelStates: Vec[Bits] = Vec.fill(8)(B(0, 8 bits))
      override def linkReady: Vec[Bool] = Vec.fill(4)(False)
      override def linkError: Vec[Bool] = Vec.fill(4)(False)
      override def vlcbBaseAddr: Vec[UInt] = Vec.fill(8)(U(0, 32 bits))
      override def updateRegisters(): Unit = {}
    }
    val vcp = vcpOpt.getOrElse(dummyVcp)
    val sched = schedOpt.getOrElse(new SchedService {
      def newProc = Flow(SchedCmd()).setIdle()
      def currentProc: UInt = U(0, 32 bits)
      def nextProc: UInt = U(0, 32 bits)
      def hiFront: UInt = U(0, 32 bits)
      def hiBack: UInt = U(0, 32 bits)
      def loFront: UInt = U(0, 32 bits)
      def loBack: UInt = U(0, 32 bits)
      def hasReady: Bool = False
      def hasHighPriority: Bool = False
      def hasLowPriority: Bool = False
      def isAnalyzing: Bool = False
      def enqueue(ptr: UInt, high: Bool): Unit = {}
      def terminateCurrent(): Unit = {}
      def yieldCurrent(): Unit = {}
      def saveHighQueue(): Unit = {}
      def saveLowQueue(): Unit = {}
      def restoreHighQueue(front: UInt, back: UInt): Unit = {}
      def restoreLowQueue(front: UInt, back: UInt): Unit = {}
      def getProcessState(ptr: UInt): ProcessState.C = {
        val state = ProcessState()
        state := ProcessState.READY
        state
      }
      def setProcessState(ptr: UInt, state: ProcessState.C): Unit = {}
      def processCount(): UInt = U(0, 16 bits)
      def updateRegisters(): Unit = {}
    })

    val inst = pipe.execute(Global.OPCODE)
    val nibble = inst(3 downto 0).asUInt
    val accumulated = Reg(UInt(32 bits)) init 0 // Oreg for operand accumulation
    val primary = Opcode.PrimaryOpcode()
    primary.assignFromBits(inst(7 downto 4))

    // Access arithmetic service for delegation
    val aluService = Try(Plugin[AluService]).toOption

    arb.exeRd.valid := False
    arb.exeRd.payload.address := U(0)
    arb.exeWr.valid := False
    arb.exeWr.payload.address := U(0)
    arb.exeWr.payload.data := B(0, Global.WORD_BITS bits)
    sched.newProc.valid := False
    sched.newProc.payload.ptr := U(0)
    sched.newProc.payload.high := False
    dma.cmd.valid := False

    switch(primary) {
      is(Opcode.PrimaryOpcode.OPR) {
        val operand = accumulated.asSInt
        val secondary = Opcode.SecondaryOpcode()
        secondary.assignFromBits(accumulated(8 downto 0).asBits) // 9 bits for T9000 opcodes
        switch(secondary) {
          is(Opcode.SecondaryOpcode.REV) {
            regStack.rev()
          }
          is(Opcode.SecondaryOpcode.ADD) {
            // Delegate to ArithmeticPlugin via service interface
            aluService match {
              case Some(alu) =>
                // ArithmeticPlugin will handle this in memory stage
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"0001" // ADD opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback for when ArithmeticPlugin not available
                val sum = regStack.readReg(RegName.Areg) + regStack.readReg(RegName.Breg)
                regStack.writeReg(RegName.Areg, sum)
                regStack.stackPop()
            }
          }
          is(Opcode.SecondaryOpcode.SUB) {
            // Delegate to ArithmeticPlugin via service interface
            aluService match {
              case Some(alu) =>
                // ArithmeticPlugin will handle this in memory stage
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"0010" // SUB opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback for when ArithmeticPlugin not available
                val diff = regStack.readReg(RegName.Breg) - regStack.readReg(RegName.Areg)
                regStack.writeReg(RegName.Areg, diff)
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.LADD) {
            val res = regStack.readReg(RegName.Breg) + regStack
              .readReg(RegName.Areg) + (regStack.readReg(RegName.Creg) & 1)
            regStack.writeReg(RegName.Areg, res.resized)
          }
          is(Opcode.SecondaryOpcode.LSUB) {
            val res = regStack.readReg(RegName.Breg) - regStack
              .readReg(RegName.Areg) - (regStack.readReg(RegName.Creg) & 1)
            regStack.writeReg(RegName.Areg, res.resized)
          }
          is(Opcode.SecondaryOpcode.LSUM) {
            val wide = UInt(33 bits)
            wide := regStack.readReg(RegName.Breg).resize(33) + regStack
              .readReg(RegName.Areg)
              .resize(33) + regStack.readReg(RegName.Creg)(0).asUInt
            regStack.writeReg(RegName.Areg, wide(31 downto 0))
            val carry = wide >= U(0xffffffffL, 33 bits)
            regStack.writeReg(RegName.Breg, carry.asUInt.resized)
          }
          is(Opcode.SecondaryOpcode.LDIFF) {
            val res = UInt(33 bits)
            res := (regStack.readReg(RegName.Breg).resize(33) - regStack
              .readReg(RegName.Areg)
              .resize(33) - regStack.readReg(RegName.Creg)(0).asUInt.resize(33))
            regStack.writeReg(RegName.Areg, res(31 downto 0))
            regStack.writeReg(RegName.Breg, res(32).asUInt.resize(32))
          }
          is(Opcode.SecondaryOpcode.LMUL) {
            val product = (regStack.readReg(RegName.Breg) * regStack.readReg(RegName.Areg))
              .resize(64) + regStack.readReg(RegName.Creg).resize(64)
            regStack.writeReg(RegName.Areg, product(31 downto 0))
            regStack.writeReg(RegName.Breg, product(63 downto 32))
          }
          is(Opcode.SecondaryOpcode.LDIV) {
            when(regStack.readReg(RegName.Creg) >= regStack.readReg(RegName.Areg)) {
              errReg := True
            } otherwise {
              val dividend =
                (regStack.readReg(RegName.Creg) ## regStack.readReg(RegName.Breg)).asUInt.resize(64)
              val quotient = dividend / regStack.readReg(RegName.Areg)
              val rem = dividend % regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, quotient(31 downto 0))
              regStack.writeReg(RegName.Breg, rem(31 downto 0))
            }
          }
          is(Opcode.SecondaryOpcode.AND) {
            // Delegate to ArithmeticPlugin - Table 6.9 operation
            aluService match {
              case Some(alu) =>
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"0110" // AND opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback
                val res = regStack.readReg(RegName.Areg) & regStack.readReg(RegName.Breg)
                regStack.writeReg(RegName.Areg, res)
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.XOR) {
            // Delegate to ArithmeticPlugin - Table 6.9 operation
            aluService match {
              case Some(alu) =>
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"1000" // XOR opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback
                val res = regStack.readReg(RegName.Areg) ^ regStack.readReg(RegName.Breg)
                regStack.writeReg(RegName.Areg, res)
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.SHL) {
            // Delegate to ArithmeticPlugin - Table 6.9 operation
            aluService match {
              case Some(alu) =>
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"1001" // SHL opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback
                val res = regStack.readReg(RegName.Breg) |<< regStack.readReg(RegName.Areg)
                regStack.writeReg(RegName.Areg, res)
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.SHR) {
            // Delegate to ArithmeticPlugin - Table 6.9 operation
            aluService match {
              case Some(alu) =>
                pipe.execute(Global.ALU_CMD_VALID) := True
                pipe.execute(Global.ALU_CMD).op := B"1010" // SHR opcode
                pipe.execute(Global.ALU_CMD).srcA := regStack.readReg(RegName.Areg)
                pipe.execute(Global.ALU_CMD).srcB := regStack.readReg(RegName.Breg)
                pipe.execute(Global.ALU_CMD).srcC := regStack.readReg(RegName.Creg)
              case None =>
                // Fallback
                val res =
                  (regStack.readReg(RegName.Breg).asSInt >> regStack.readReg(RegName.Areg)).asUInt
                regStack.writeReg(RegName.Areg, res)
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.LSHR) {
            when(regStack.readReg(RegName.Areg) < 64) {
              val wide =
                (regStack.readReg(RegName.Creg) ## regStack.readReg(RegName.Breg)).asUInt.resize(64)
              val shifted = wide >> regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, shifted(31 downto 0))
              regStack.writeReg(RegName.Breg, shifted(63 downto 32))
            } otherwise {
              regStack.writeReg(RegName.Areg, 0)
              regStack.writeReg(RegName.Breg, 0)
            }
          }
          is(Opcode.SecondaryOpcode.LSHL) {
            when(regStack.readReg(RegName.Areg) < 64) {
              val wide =
                (regStack.readReg(RegName.Creg) ## regStack.readReg(RegName.Breg)).asUInt.resize(64)
              val shifted = wide |<< regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, shifted(31 downto 0))
              regStack.writeReg(RegName.Breg, shifted(63 downto 32))
            } otherwise {
              regStack.writeReg(RegName.Areg, 0)
              regStack.writeReg(RegName.Breg, 0)
            }
          }
          is(Opcode.SecondaryOpcode.IN) {
            val idx = regStack.readReg(RegName.Breg)(1 downto 0)
            val avail = links.rxValid(idx)
            pipe.execute.haltWhen(!avail)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, links.rxPayload(idx).asUInt)
              links.rxAck(idx)
            }
          }
          is(Opcode.SecondaryOpcode.OUT) {
            val idx = regStack.readReg(RegName.Breg)(1 downto 0)
            val ready = links.push(idx, regStack.readReg(RegName.Areg).asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.RET) {
            regStack.writeReg(RegName.IptrReg, regStack.read(S(0)))
            regStack.writeReg(RegName.WdescReg, regStack.readReg(RegName.WdescReg) + 4)
          }
          is(Opcode.SecondaryOpcode.STARTP) {
            sched.newProc.valid := True
            sched.newProc.payload.ptr := regStack.readReg(RegName.Areg)
            sched.newProc.payload.high := False
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.RUNP) {
            sched.enqueue(regStack.readReg(RegName.Areg), False)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.TESTERR) {
            val tmp = errReg
            errReg := False
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, tmp.asUInt.resized)
          }
          is(Opcode.SecondaryOpcode.ALT) {
            // ALT setup not yet implemented
          }
          is(Opcode.SecondaryOpcode.ALTWT) {
            val waitDone = timer.hi >= regStack.readReg(RegName.Areg)
            when(!waitDone) { sched.enqueue(regStack.readReg(RegName.WdescReg), False) }
            pipe.execute.haltWhen(!waitDone)
          }
          is(Opcode.SecondaryOpcode.ALTEND) {
            // ALTEND placeholder
          }
          is(Opcode.SecondaryOpcode.STLB) {
            loBPtr := regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.STHB) {
            hiBPtr := regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.STLF) {
            loFPtr := regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.STHF) {
            hiFPtr := regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.SAVEL) {
            val addr = Mux(saveLPhase, loBPtr, loFPtr)
            arb.exeWr.valid := True
            arb.exeWr.payload.address := addr.resized
            arb.exeWr.payload.data := Mux(saveLPhase, sched.loBack, sched.loFront).asBits
            pipe.execute.haltWhen(False)
            when(pipe.execute.down.isFiring) {
              when(!saveLPhase) {
                saveLPhase := True
              } otherwise {
                saveLPhase := False
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
              }
            }
          }
          is(Opcode.SecondaryOpcode.SAVEH) {
            val addr = Mux(saveHPhase, hiBPtr, hiFPtr)
            arb.exeWr.valid := True
            arb.exeWr.payload.address := addr.resized
            arb.exeWr.payload.data := Mux(saveHPhase, sched.hiBack, sched.hiFront).asBits
            pipe.execute.haltWhen(False)
            when(pipe.execute.down.isFiring) {
              when(!saveHPhase) {
                saveHPhase := True
              } otherwise {
                saveHPhase := False
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
              }
            }
          }
          is(Opcode.SecondaryOpcode.STOPP) {
            sched.enqueue(regStack.readReg(RegName.WdescReg), False)
            pipe.execute.haltWhen(True)
          }
          is(Opcode.SecondaryOpcode.ENDP) {
            val parent = regStack.read(S(0))
            val cnt = regStack.read(S(-1)) - 1
            regStack.write(S(-1), cnt)
            when(cnt === 0) {
              sched.terminateCurrent()
            } otherwise {
              sched.enqueue(parent, False)
            }
          }
          is(Opcode.SecondaryOpcode.MINT) {
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, U(0x80000000L))
          }
          is(Opcode.SecondaryOpcode.STTIMER) {
            timer.set(regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.LDTIMER) {
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, timer.hi)
          }
          is(Opcode.SecondaryOpcode.TIMERDISABLEH) {
            timer.disableHi()
          }
          is(Opcode.SecondaryOpcode.TIMERDISABLEL) {
            timer.disableLo()
          }
          is(Opcode.SecondaryOpcode.TIMERENABLEH) {
            timer.enableHi()
          }
          is(Opcode.SecondaryOpcode.TIMERENABLEL) {
            timer.enableLo()
          }
          is(Opcode.SecondaryOpcode.LEND) {
            val cnt = regStack.readReg(RegName.Areg) - 1
            regStack.writeReg(RegName.Areg, cnt)
            when(cnt =/= 0) {
              sched.enqueue(regStack.readReg(RegName.WdescReg), False)
              pipe.execute.haltWhen(True)
            }
          }
          is(Opcode.SecondaryOpcode.CLRHALTERR) {
            haltErr := False
          }
          is(Opcode.SecondaryOpcode.SETHALTERR) {
            haltErr := True
          }
          is(Opcode.SecondaryOpcode.TESTHALTERR) {
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, haltErr.asUInt.resized)
          }
          is(Opcode.SecondaryOpcode.DUP) {
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
          }
          is(Opcode.SecondaryOpcode.MOVE2DINIT) {
            move2dLen := regStack.readReg(RegName.Areg)
            move2dStride := regStack.readReg(RegName.Breg)
            move2dAddr := regStack.readReg(RegName.Creg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.MOVE2DALL) {
            dma.cmd.valid := True
            dma.cmd.payload.link := regStack.readReg(RegName.Breg)(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := regStack.readReg(RegName.Areg)
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.MOVE2DNONZERO) {
            dma.cmd.valid := True
            dma.cmd.payload.link := regStack.readReg(RegName.Breg)(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := regStack.readReg(RegName.Areg)
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.MOVE2DZERO) {
            dma.cmd.valid := True
            dma.cmd.payload.link := regStack.readReg(RegName.Breg)(1 downto 0)
            dma.cmd.payload.addr := move2dAddr
            dma.cmd.payload.length := move2dLen
            dma.cmd.payload.stride := move2dStride
            dma.cmd.payload.rows := regStack.readReg(RegName.Areg)
            dma.cmd.payload.twoD := True
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              move2dAddr := move2dAddr + move2dStride * regStack.readReg(RegName.Areg)
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.POP) {
            val t = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            regStack.writeReg(RegName.Creg, t)
          }
          is(Opcode.SecondaryOpcode.LB) {
            val addr = regStack.readReg(RegName.Areg)
            arb.exeRd.valid := True
            arb.exeRd.payload.address := (addr >> 2).resized
            pipe.execute.haltWhen(!mem.rdRsp.valid)
            when(pipe.execute.down.isFiring) {
              val shift = addr(1 downto 0) * 8
              val byte = (mem.rdRsp.payload >> shift).resize(8)
              regStack.writeReg(RegName.Areg, byte.asUInt.resize(Global.WORD_BITS))
            }
          }
          is(Opcode.SecondaryOpcode.OUTBYTE) {
            val idx = regStack.readReg(RegName.Breg)(1 downto 0)
            val ready = links.push(idx, regStack.readReg(RegName.Areg).asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.OUTWORD) {
            val idx = regStack.readReg(RegName.Breg)(1 downto 0)
            val ready = links.push(idx, regStack.readReg(RegName.Areg).asBits)
            pipe.execute.haltWhen(!ready)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.MOVE) {
            dma.cmd.valid := True
            dma.cmd.payload.link := regStack.readReg(RegName.Breg)(1 downto 0)
            dma.cmd.payload.addr := regStack.readReg(RegName.Creg)
            dma.cmd.payload.length := regStack.readReg(RegName.Areg)
            dma.cmd.payload.stride := 0
            dma.cmd.payload.rows := 0
            dma.cmd.payload.twoD := False
            pipe.execute.haltWhen(!dma.cmd.ready)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
              regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.LDPI) {
            regStack.writeReg(
              RegName.Areg,
              regStack.readReg(RegName.Areg) + regStack.readReg(RegName.IptrReg)
            )
          }
          is(Opcode.SecondaryOpcode.FPADD) {
            // Delegate to FpuPlugin via pipeline command
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPADD.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcC := regStack.readReg(RegName.Creg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Default rounding
              case None =>
                // Fallback implementation when FPU not available
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPSUB) {
            // Delegate to FpuPlugin via pipeline command
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPSUB.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcC := regStack.readReg(RegName.Creg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Default rounding
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPMUL) {
            // Delegate to FpuPlugin via pipeline command
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPMUL.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcC := regStack.readReg(RegName.Creg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Default rounding
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPDIV) {
            // Delegate to FpuPlugin via pipeline command
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPDIV.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcC := regStack.readReg(RegName.Creg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Default rounding
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }

          // ========================================
          // ENHANCED T9000 FLOATING-POINT INSTRUCTIONS
          // ========================================

          is(Opcode.SecondaryOpcode.FPABS) {
            // Delegate to FpuPlugin via pipeline command
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPABS.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPINT) {
            // Delegate to FpuPlugin via pipeline command - round to integer
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPINT.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Use current rounding mode
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPMULBY2) {
            // Delegate to FpuPlugin via pipeline command - multiply by 2
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPMULBY2.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPDIVBY2) {
            // Delegate to FpuPlugin via pipeline command - divide by 2
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPDIVBY2.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPREM) {
            // Delegate to FpuPlugin via pipeline command - remainder
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPREM.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.FPSQRT) {
            // Delegate to FpuPlugin via pipeline command - square root
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPSQRT.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPRN) {
            // Delegate rounding mode change to FpuPlugin
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPRN.asBits.resize(8)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00" // Round to nearest
              case None =>
              // No action needed for fallback
            }
          }
          is(Opcode.SecondaryOpcode.FPRP) {
            // Delegate rounding mode change to FpuPlugin
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPRP.asBits.resize(8)
                pipe.execute(Global.FPU_CMD).roundingMode := B"10" // Round toward +infinity
              case None =>
              // No action needed for fallback
            }
          }
          is(Opcode.SecondaryOpcode.FPRM) {
            // Delegate rounding mode change to FpuPlugin
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPRM.asBits.resize(8)
                pipe.execute(Global.FPU_CMD).roundingMode := B"11" // Round toward -infinity
              case None =>
              // No action needed for fallback
            }
          }
          is(Opcode.SecondaryOpcode.FPRZ) {
            // Delegate rounding mode change to FpuPlugin
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPRZ.asBits.resize(8)
                pipe.execute(Global.FPU_CMD).roundingMode := B"01" // Round toward zero
              case None =>
              // No action needed for fallback
            }
          }
          is(Opcode.SecondaryOpcode.FPR32TOR64) {
            // Delegate to FpuPlugin via pipeline command - convert 32-bit to 64-bit float
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPR32TOR64.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPR64TOR32) {
            // Delegate to FpuPlugin via pipeline command - convert 64-bit to 32-bit float
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPR64TOR32.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPEXPDEC32) {
            // Delegate to FpuPlugin via pipeline command - decrement exponent
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPEXPDEC32.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPEXPINC32) {
            // Delegate to FpuPlugin via pipeline command - increment exponent
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPEXPINC32.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPCHKI32) {
            // Delegate to FpuPlugin via pipeline command - check if fits in 32-bit int
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPCHKI32.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPCHKI64) {
            // Delegate to FpuPlugin via pipeline command - check if fits in 64-bit int
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPCHKI64.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPGT) {
            // Delegate to FpuPlugin via pipeline command - greater than
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPGT.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.FPEQ) {
            // Delegate to FpuPlugin via pipeline command - equal
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPEQ.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.FPGE) {
            // Delegate to FpuPlugin via pipeline command - greater than or equal
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPGE.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.FPNAN) {
            // Delegate to FpuPlugin via pipeline command - check if NaN
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPNAN.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPORDERED) {
            // Delegate to FpuPlugin via pipeline command - check if both ordered (not NaN)
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPORDERED.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcB := regStack.readReg(RegName.Breg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
                regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.FPNOTFINITE) {
            // Delegate to FpuPlugin via pipeline command - check if not finite (NaN or infinity)
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPNOTFINITE.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPI32TOR32) {
            // Delegate to FpuPlugin via pipeline command - convert 32-bit int to 32-bit float
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPI32TOR32.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPI32TOR64) {
            // Delegate to FpuPlugin via pipeline command - convert 32-bit int to 64-bit float
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPI32TOR64.asBits
                  .resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPRTOI32) {
            // Delegate to FpuPlugin via pipeline command - convert float to 32-bit int
            fpuOpt match {
              case Some(fpuService) =>
                pipe.execute(Global.FPU_CMD_VALID) := True
                pipe.execute(Global.FPU_CMD).op := Opcode.SecondaryOpcode.FPRTOI32.asBits.resize(8)
                pipe
                  .execute(Global.FPU_CMD)
                  .srcA := regStack.readReg(RegName.Areg).asBits.resize(64)
                pipe.execute(Global.FPU_CMD).srcB := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).srcC := B(0, 64 bits)
                pipe.execute(Global.FPU_CMD).roundingMode := B"00"
              case None =>
                regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Areg))
            }
          }
          is(Opcode.SecondaryOpcode.FPDUP) {
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
          }
          is(Opcode.SecondaryOpcode.FPREV) {
            val tmp = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, tmp)
          }

          // ========================================
          // MEMORY AND BYTE OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.SB) {
            val addr = regStack.readReg(RegName.Breg)
            val byte = regStack.readReg(RegName.Areg)(7 downto 0)
            arb.exeWr.valid := True
            arb.exeWr.payload.address := (addr >> 2).resized
            arb.exeWr.payload.data := (byte ## byte ## byte ## byte).asBits
            pipe.execute.haltWhen(!arb.exeWr.fire)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.BSUB) {
            val result = regStack.readReg(RegName.Breg) + regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.WSUB) {
            val result = regStack.readReg(RegName.Breg) + (regStack.readReg(RegName.Areg) << 2)
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.WSUBDB) {
            val result = regStack.readReg(RegName.Breg) + (regStack.readReg(RegName.Areg) << 3)
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.XWORD) {
            val extended = regStack.readReg(RegName.Areg)(15 downto 0).asSInt.resize(32).asUInt
            regStack.writeReg(RegName.Areg, extended)
          }
          is(Opcode.SecondaryOpcode.XBWORD) {
            val extended = regStack.readReg(RegName.Areg)(7 downto 0).asSInt.resize(32).asUInt
            regStack.writeReg(RegName.Areg, extended)
          }
          is(Opcode.SecondaryOpcode.LBX) {
            val addr = regStack.readReg(RegName.Areg)
            arb.exeRd.valid := True
            arb.exeRd.payload.address := (addr >> 2).resized
            pipe.execute.haltWhen(!mem.rdRsp.valid)
            when(pipe.execute.down.isFiring) {
              val shift = addr(1 downto 0) * 8
              val byte = (mem.rdRsp.payload >> shift).resize(8)
              regStack.writeReg(RegName.Areg, byte.asSInt.resize(32).asUInt)
            }
          }
          is(Opcode.SecondaryOpcode.CB) {
            val value = regStack.readReg(RegName.Areg)(7 downto 0).asSInt
            when(value < S(-128) || value > S(127)) {
              errReg := True
            }
          }
          is(Opcode.SecondaryOpcode.CBU) {
            val value = regStack.readReg(RegName.Areg)
            when(value > U(255)) {
              errReg := True
            }
          }
          is(Opcode.SecondaryOpcode.SS) {
            val addr = regStack.readReg(RegName.Breg)
            val halfword = regStack.readReg(RegName.Areg)(15 downto 0)
            arb.exeWr.valid := True
            arb.exeWr.payload.address := (addr >> 2).resized
            arb.exeWr.payload.data := (halfword ## halfword).asBits
            pipe.execute.haltWhen(!arb.exeWr.fire)
            when(pipe.execute.down.isFiring) {
              regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
            }
          }
          is(Opcode.SecondaryOpcode.LSX) {
            val addr = regStack.readReg(RegName.Areg)
            arb.exeRd.valid := True
            arb.exeRd.payload.address := (addr >> 2).resized
            pipe.execute.haltWhen(!mem.rdRsp.valid)
            when(pipe.execute.down.isFiring) {
              val shift = (addr(1).asUInt << 4)
              val halfword = (mem.rdRsp.payload >> shift).resize(16)
              regStack.writeReg(RegName.Areg, halfword.asSInt.resize(32).asUInt)
            }
          }
          is(Opcode.SecondaryOpcode.CS) {
            val value = regStack.readReg(RegName.Areg)(15 downto 0).asSInt
            when(value < S(-32768) || value > S(32767)) {
              errReg := True
            }
          }
          is(Opcode.SecondaryOpcode.CSU) {
            val value = regStack.readReg(RegName.Areg)
            when(value > U(65535)) {
              errReg := True
            }
          }
          is(Opcode.SecondaryOpcode.XSWORD) {
            val extended = regStack.readReg(RegName.Areg)(15 downto 0).asSInt.resize(32).asUInt
            regStack.writeReg(RegName.Areg, extended)
          }

          // ========================================
          // BIT MANIPULATION AND CRC OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.CRCWORD) {
            val data = regStack.readReg(RegName.Areg)
            val poly = regStack.readReg(RegName.Breg)
            val crcIn = regStack.readReg(RegName.Creg)
            val crcStages = Vec(UInt(32 bits), 33)
            crcStages(0) := crcIn

            for (i <- 0 until 32) {
              when(crcStages(i)(31) ^ data(i)) {
                crcStages(i + 1) := (crcStages(i) |<< 1) ^ poly
              } otherwise {
                crcStages(i + 1) := crcStages(i) |<< 1
              }
            }

            regStack.writeReg(RegName.Areg, crcStages(32))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.CRCBYTE) {
            val data = regStack.readReg(RegName.Areg)(7 downto 0)
            val poly = regStack.readReg(RegName.Breg)
            val crcIn = regStack.readReg(RegName.Creg)
            val crcStages = Vec(UInt(32 bits), 9)
            crcStages(0) := crcIn

            for (i <- 0 until 8) {
              when(crcStages(i)(31) ^ data(i)) {
                crcStages(i + 1) := (crcStages(i) |<< 1) ^ poly
              } otherwise {
                crcStages(i + 1) := crcStages(i) |<< 1
              }
            }

            regStack.writeReg(RegName.Areg, crcStages(8))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.BITCNT) {
            val value = regStack.readReg(RegName.Areg)
            val popcount = CountOne(value.asBits)
            regStack.writeReg(RegName.Areg, popcount.resized)
          }
          is(Opcode.SecondaryOpcode.BITREVWORD) {
            val value = regStack.readReg(RegName.Areg)
            val reversed = Reverse(value.asBits)
            regStack.writeReg(RegName.Areg, reversed.asUInt)
          }
          is(Opcode.SecondaryOpcode.BITREVNBITS) {
            val value = regStack.readReg(RegName.Areg)
            val nbits = regStack.readReg(RegName.Breg)(4 downto 0) // Max 32 bits
            val mask = (U(1) << nbits) - 1
            val maskedValue = value & mask
            val reversed = Reverse(maskedValue.asBits.resize(32))
            val result = reversed.asUInt >> (32 - nbits)
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }

          // ========================================
          // COUNTING AND SIZING OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.BCNT) {
            val byteCount = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, byteCount)
          }
          is(Opcode.SecondaryOpcode.WCNT) {
            val wordCount = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Areg, wordCount << 2) // Convert words to bytes
          }
          is(Opcode.SecondaryOpcode.CCNT) {
            val count = regStack.readReg(RegName.Areg)
            when(count === 0) {
              errReg := True
            }
            regStack.writeReg(RegName.Areg, count - 1)
          }
          is(Opcode.SecondaryOpcode.CSNGL) {
            val value = regStack.readReg(RegName.Areg)
            when(value > U(0x7fffffff) && value < U((1L << 31), 32 bits)) {
              errReg := True
            }
          }
          is(Opcode.SecondaryOpcode.CWORD) {
            val value = regStack.readReg(RegName.Areg)
            when(value(1 downto 0) =/= 0) {
              errReg := True // Not word aligned
            }
          }
          is(Opcode.SecondaryOpcode.LDCNT) {
            val count = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, count)
          }
          is(Opcode.SecondaryOpcode.SSUB) {
            val result = regStack.readReg(RegName.Breg) + (regStack.readReg(RegName.Areg) << 1)
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }

          // ========================================
          // PROCESS CONTROL AND CHANNEL OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.RESETCH) {
            val channel = regStack.readReg(RegName.Areg)
            links.rxAck(channel(1 downto 0))
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.CSUB) {
            val result = regStack.readReg(RegName.Breg) + regStack.readReg(RegName.Areg)
            when(result < regStack.readReg(RegName.Breg)) {
              errReg := True // Check subscript overflow
            }
            regStack.writeReg(RegName.Areg, result)
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.GAJW) {
            val newWPtr = regStack.readReg(RegName.Areg)
            regStack.writeReg(RegName.WdescReg, newWPtr)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.DIST) {
            timer.disableHi()
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.DISC) {
            val channel = regStack.readReg(RegName.Areg)
            // Disable channel - implementation depends on VCP
            vcp.setVcpCommand(B"00000001")
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.DISS) {
            // Disable skip - ALT operation
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.ENBT) {
            timer.enableHi()
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.ENBC) {
            val channel = regStack.readReg(RegName.Areg)
            // Enable channel - implementation depends on VCP
            vcp.setVcpCommand(B"00000010")
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.ENBS) {
            // Enable skip - ALT operation
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.INSPHDR) {
            val header = regStack.readReg(RegName.Areg)
            val channel = regStack.readReg(RegName.Breg)(2 downto 0).algoInt
            vcp.updateChannelHeader(channel, header.asBits)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.READBFR) {
            val channel = regStack.readReg(RegName.Areg)(2 downto 0).algoInt
            val state = vcp.getChannelState(channel)
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, state.asUInt)
          }
          is(Opcode.SecondaryOpcode.CHANTYPE) {
            val channel = regStack.readReg(RegName.Areg)
            val channelType = U(1) // Default hard channel type
            regStack.writeReg(RegName.Areg, channelType)
          }

          // ========================================
          // ERROR HANDLING AND STATUS OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.TESTPRANAL) {
            val analyzing = sched.isAnalyzing
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, analyzing.asUInt.resized)
          }
          is(Opcode.SecondaryOpcode.STOPERR) {
            errReg := True
            when(haltErr) {
              pipe.execute.haltWhen(True)
            }
          }
          is(Opcode.SecondaryOpcode.LDFLAGS) {
            val flags = errReg.asUInt.resized
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, flags)
          }
          is(Opcode.SecondaryOpcode.STFLAGS) {
            errReg := regStack.readReg(RegName.Areg)(0)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }

          // ========================================
          // CONFIGURATION REGISTER OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.LDCONF) {
            val addr = regStack.readReg(RegName.Areg)(15 downto 0).asBits
            val data = config.read(addr, 32)
            regStack.writeReg(RegName.Areg, data.asUInt)
          }
          is(Opcode.SecondaryOpcode.STCONF) {
            val addr = regStack.readReg(RegName.Breg)(15 downto 0).asBits
            val data = regStack.readReg(RegName.Areg).asBits
            config.write(addr, data, 32)
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.LDTH) {
            val trapHandler = trap.trapHandlerAddr
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, trapHandler.asUInt)
          }
          is(Opcode.SecondaryOpcode.LDCHSTATUS) {
            val channel = regStack.readReg(RegName.Areg)(2 downto 0).algoInt
            val status = vcp.getChannelState(channel)
            regStack.writeReg(RegName.Areg, status.asUInt)
          }

          // ========================================
          // INTERRUPT CONTROL
          // ========================================

          is(Opcode.SecondaryOpcode.INTDIS) {
            // Disable interrupts
            pipe.execute.haltWhen(False) // Placeholder
          }
          is(Opcode.SecondaryOpcode.INTENB) {
            // Enable interrupts
            pipe.execute.haltWhen(False) // Placeholder
          }

          // ========================================
          // RANGE AND BOUND CHECKING
          // ========================================

          is(Opcode.SecondaryOpcode.CIR) {
            val value = regStack.readReg(RegName.Areg).asSInt
            val lower = regStack.readReg(RegName.Breg).asSInt
            val upper = regStack.readReg(RegName.Creg).asSInt
            when(value < lower || value > upper) {
              errReg := True
            }
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.CIRU) {
            val value = regStack.readReg(RegName.Areg)
            val lower = regStack.readReg(RegName.Breg)
            val upper = regStack.readReg(RegName.Creg)
            when(value < lower || value > upper) {
              errReg := True
            }
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Creg))
          }

          // ========================================
          // DEVICE OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.DEVLB) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            // Device load byte - implementation specific
            regStack.writeReg(RegName.Areg, U(0)) // Placeholder
          }
          is(Opcode.SecondaryOpcode.DEVSB) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            val data = regStack.readReg(RegName.Creg)(7 downto 0)
            // Device store byte - implementation specific
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.DEVLS) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            // Device load sixteen - implementation specific
            regStack.writeReg(RegName.Areg, U(0)) // Placeholder
          }
          is(Opcode.SecondaryOpcode.DEVSS) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            val data = regStack.readReg(RegName.Creg)(15 downto 0)
            // Device store sixteen - implementation specific
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
          }
          is(Opcode.SecondaryOpcode.DEVLW) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            // Device load word - implementation specific
            regStack.writeReg(RegName.Areg, U(0)) // Placeholder
          }
          is(Opcode.SecondaryOpcode.DEVSW) {
            val deviceAddr = regStack.readReg(RegName.Breg)
            val offset = regStack.readReg(RegName.Areg)
            val data = regStack.readReg(RegName.Creg)
            // Device store word - implementation specific
            regStack.writeReg(RegName.Areg, regStack.readReg(RegName.Creg))
          }

          // ========================================
          // MEMORY OPERATIONS
          // ========================================

          is(Opcode.SecondaryOpcode.LDMEMSTARTVAL) {
            val memStartVal = U(Global.MemStart, 32 bits)
            regStack.writeReg(RegName.Creg, regStack.readReg(RegName.Breg))
            regStack.writeReg(RegName.Breg, regStack.readReg(RegName.Areg))
            regStack.writeReg(RegName.Areg, memStartVal)
          }
        }
        when(secondary === Opcode.SecondaryOpcode.TIN) {
          val waitDone = timer.hi >= regStack.readReg(RegName.Areg)
          when(!waitDone) { sched.enqueue(regStack.readReg(RegName.WdescReg), False) }
          pipe.execute.haltWhen(!waitDone)
        }
        accumulated := 0 // Reset operand accumulator
      }
    }
    println(s"[${SecondaryInstrPlugin.this.getDisplayName()}] build end")
  }
}

package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbQueue, BmbDownSizerBridge}
import t800.Global
import t800.T800

/** Instruction fetch unit with T9000-style Instruction Prefetch Buffer (IPB) supporting eight-instruction dispatch. */
class FetchPlugin extends FiberPlugin {
  val version = "FetchPlugin v1.2"
  report(L"Initializing $version")
  println(s"[${FetchPlugin.this.getDisplayName()}] build start")

  object DBKeys {
    val FETCH_PC = Database.blocking[Bits]()
    val FETCH_OPCODES = Database.blocking[Vec[Bits]]()
  }

  lazy val FETCH_PC = Payload(Bits(Global.ADDR_BITS bits))
  lazy val FETCH_OPCODES = Payload(Vec(Bits(8 bits), 8)) // Eight 8-bit instructions

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val imem = Plugin[InstrFetchSrv]
    val pipe = Plugin[PipelineSrv]
    val stack = Plugin[StackSrv]
    val systemBus = host.find[T800].systemBus // 128-bit system bus

    // IPB parameters: 4 entries, 32-bit fetch width, 128-bit burst
    val ipbDepth = 4
    val fetchParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 32, // 4 bytes per cycle
        lengthWidth = 4,
        sourceWidth = 1,
        contextWidth = 0
      )
    )

    // Down-sizer from 128-bit system bus to 32-bit fetch
    val downSizer = BmbDownSizerBridge(
      inputParameter = T800.systemBusParam,
      outputParameter = BmbDownSizerBridge.outputParameterFrom(T800.systemBusParam.access, 32).toBmbParameter()
    )
    systemBus >> downSizer.io.input

    // IPB queue and buffer
    val ipbQueue = BmbQueue(fetchParam, depth = ipbDepth)
    val ipbBuffer = Reg(Vec(Bits(32 bits), ipbDepth * 4)) init(Vec.fill(ipbDepth * 4)(0)) // 4 words per entry
    val ipbPtr = Reg(UInt(log2Up(ipbDepth) bits)) init(0)
    val ipbFull = Reg(Bool()) init(False)
    val ipbWordPtr = Reg(UInt(2 bits)) init(0) // Track within 128-bit burst
    val ipbInstrPtr = Reg(UInt(3 bits)) init(0) // Track eight 8-bit instructions

    // Connect to down-sized BMB
    downSizer.io.output >> ipbQueue.io.input
    ipbQueue.io.output.cmd.opcode := 0 // Read-only
    ipbQueue.io.output.cmd.length := 3 // 4-word burst (128 bits)

    // Fetch logic
    val currentPC = stack.IPtr
    val isSequential = currentPC === (RegNext(stack.IPtr) + 4)
    when(!isSequential || !ipbFull) {
      // Flush and reload on non-sequential fetch
      ipbQueue.io.output.cmd.valid := True
      ipbQueue.io.output.cmd.address := (currentPC >> 2) << 4 // Align to 128-bit boundary
      when(ipbQueue.io.output.rsp.valid && ipbQueue.io.output.rsp.last) {
        ipbBuffer(0) := ipbQueue.io.output.rsp.data(31 downto 0)
        ipbBuffer(1) := ipbQueue.io.output.rsp.data(63 downto 32)
        ipbBuffer(2) := ipbQueue.io.output.rsp.data(95 downto 64)
        ipbBuffer(3) := ipbQueue.io.output.rsp.data(127 downto 96)
        ipbPtr := 0
        ipbWordPtr := 0
        ipbInstrPtr := 0
        ipbFull := True
      }
    }

    // Serve eight 8-bit instructions per cycle from IPB
    imem.cmd.valid := True
    imem.cmd.payload.addr := (currentPC >> 2).resized
    val baseIdx = ipbPtr << 2
    val opcodes = Vec(Bits(8 bits), 8)
    for (i <- 0 until 8) {
      val wordIdx = (ipbInstrPtr + i) / 4
      val bitIdx = ((ipbInstrPtr + i) % 4) * 8
      opcodes(i) := (ipbBuffer(baseIdx + wordIdx) >> bitIdx)(7 downto 0)
    }
    imem.rsp.payload := opcodes.asBits // 64-bit payload for eight 8-bit instructions
    imem.rsp.valid := ipbFull && ipbInstrPtr < (ipbDepth * 4 - 7)
    when(pipe.fetch.down.isFiring) {
      ipbInstrPtr := ipbInstrPtr + 8
      when(ipbInstrPtr >= (ipbDepth * 4 - 7)) {
        ipbPtr := ipbPtr + 1
        ipbInstrPtr := 0
        when(ipbPtr === (ipbDepth - 1)) { ipbFull := False }
      }
      stack.IPtr := stack.IPtr + 32 // Increment by 32 bytes (eight instructions)
    }

    // Pipeline integration
    pipe.fetch.haltWhen(!imem.rsp.valid)
    pipe.fetch(Global.IPTR) := currentPC
    pipe.fetch(FETCH_OPCODES) := opcodes
    DBKeys.FETCH_PC.set(currentPC)
    DBKeys.FETCH_OPCODES.set(opcodes)
    println(s"[${FetchPlugin.this.getDisplayName()}] build end")
  }
}

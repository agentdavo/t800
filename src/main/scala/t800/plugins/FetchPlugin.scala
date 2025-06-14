package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbAccessParameter, BmbQueue}
import t800.Global
import t800.T800

/** Instruction fetch unit with T9000-style Instruction Prefetch Buffer (IPB). */
class FetchPlugin extends FiberPlugin {
  val version = "FetchPlugin v1.0"
  report(L"Initializing $version")
  println(s"[${FetchPlugin.this.getDisplayName()}] build start")

  object DBKeys {
    val FETCH_PC = Database.blocking[Bits]()
    val FETCH_OPCODE = Database.blocking[Bits]()
  }

  lazy val FETCH_PC = Payload(Bits(Global.ADDR_BITS bits))
  lazy val FETCH_OPCODE = Payload(Bits(Global.OPCODE_BITS bits))

  override def build(): Unit = {
    implicit val h: PluginHost = host
    val imem = Plugin[InstrFetchSrv]
    val pipe = Plugin[PipelineSrv]
    val stack = Plugin[StackSrv]
    val systemBus = host.find[T800].systemBus // Access 128-bit system bus

    // IPB parameters: 4 entries, 128-bit wide (4 × 32-bit words)
    val ipbDepth = 4
    val ipbParam = BmbParameter(
      access = BmbAccessParameter(
        addressWidth = Global.ADDR_BITS,
        dataWidth = 128, // Matches system bus width
        lengthWidth = 4, // Supports 16-byte bursts
        sourceWidth = 1, // Single master (FetchPlugin)
        contextWidth = 0
      )
    )

    // IPB queue and buffer
    val ipbQueue = BmbQueue(ipbParam, depth = ipbDepth)
    val ipbBuffer = Reg(Vec(Bits(32 bits), ipbDepth)) init(Vec.fill(ipbDepth)(0))
    val ipbPtr = Reg(UInt(log2Up(ipbDepth) bits)) init(0)
    val ipbFull = Reg(Bool()) init(False)

    // Connect to system bus with read-only BMB
    systemBus >> ipbQueue.io.input
    ipbQueue.io.output.cmd.opcode := 0 // Read-only
    ipbQueue.io.output.cmd.length := 3 // 4-word burst (16 bytes)

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
        ipbFull := True
      }
    }

    // Serve instructions from IPB
    imem.cmd.valid := True
    imem.cmd.payload.addr := (currentPC >> 2).resized
    val shift = currentPC(1 downto 0) * U(Global.OPCODE_BITS)
    val opcode = (ipbBuffer(ipbPtr) >> shift)(Global.OPCODE_BITS - 1 downto 0)
    imem.rsp.payload := opcode
    imem.rsp.valid := ipbFull && ipbPtr < ipbDepth
    when(pipe.fetch.down.isFiring) {
      ipbPtr := ipbPtr + 1
      when(ipbPtr === (ipbDepth - 1)) { ipbFull := False }
      stack.IPtr := stack.IPtr + 4
    }

    // Pipeline integration
    pipe.fetch.haltWhen(!imem.rsp.valid)
    pipe.fetch(Global.IPTR) := currentPC
    pipe.fetch(Global.OPCODE) := opcode
    DBKeys.FETCH_PC.set(currentPC)
    DBKeys.FETCH_OPCODE.set(opcode)

    println(s"[${FetchPlugin.this.getDisplayName()}] build end")
  }
}

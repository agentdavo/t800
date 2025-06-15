package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{Plugin, PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import t800.Global

/** Gather opcodes from the fetch stage and deliver them in groups of up to eight instructions on
  * the decode stage. Other plugins may read the grouped values through [[GroupedInstrSrv]].
  */
class GrouperPlugin extends FiberPlugin with PipelineService {
  val version = "GrouperPlugin v0.1"
  private val retain = Retainer()
  private var instrVec: Vec[Bits] = null
  private var instrCount: UInt = null
  private var groupValid: Bool = null
  private var groupFlow: Flow[GroupedInstructions] = null
  private var links: Seq[pipeline.Link] = Seq()

  override def getLinks(): Seq[pipeline.Link] = links

  // Provide grouped instruction access to other plugins
  during setup new Area {
    report(L"Initializing $version")
    retain()
    instrVec = Vec.fill(8)(Reg(Bits(Global.OPCODE_BITS bits)) init 0)
    instrCount = Reg(UInt(4 bits)) init 0
    groupValid = Reg(Bool()) init False
    groupFlow = Flow(GroupedInstructions())
    groupFlow.setIdle()
    addService(new GroupedInstrSrv {
      override def groups: Flow[GroupedInstructions] = groupFlow
    })
  }

  during build new Area {
    retain.await()
    implicit val h: PluginHost = host
    val pipe = Plugin[PipelineSrv]

    val out = CtrlLink()
    val toDecode = StageLink(out.down, pipe.decode.up)
    val GROUP_INSTR = out.insert(Vec(Bits(Global.OPCODE_BITS bits), 8))
    val GROUP_COUNT = out.insert(UInt(4 bits))

    // FIFO decouples fetch from decode while groups are formed
    val fifo = StreamFifo(Bits(Global.OPCODE_BITS bits), 16)
    fifo.io.push << pipe.fetch.down.toStream(n => n(Global.OPCODE))
    pipe.fetch.ignoreReadyWhen(True)
    pipe.fetch.haltWhen(!fifo.io.push.ready)

    // Fill the current group until eight opcodes are collected
    fifo.io.pop.ready := False
    when(!groupValid && fifo.io.pop.valid) {
      instrVec(instrCount) := fifo.io.pop.payload
      instrCount := instrCount + 1
      fifo.io.pop.ready := True
      when(instrCount === 7) { groupValid := True }
    }

    val send = Flow(GroupedInstructions())
    send.valid := groupValid
    send.payload.instructions := instrVec
    send.payload.count := instrCount

    out.up.driveFrom(send) { (n, p) =>
      n(GROUP_INSTR) := p.instructions
      n(GROUP_COUNT) := p.count
    }
    out.haltWhen(!groupValid)

    when(out.down.isFiring) {
      groupValid := False
      instrCount := 0
    }

    groupFlow.valid := out.down.isValid
    groupFlow.payload.instructions := out.down(GROUP_INSTR)
    groupFlow.payload.count := out.down(GROUP_COUNT)

    links = Seq(toDecode)
  }
}

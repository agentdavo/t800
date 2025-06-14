package t800.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{FiberPlugin, Plugin, PluginHost}
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

    // Payloads published on the decode stage
    val GROUP_INSTR = pipe.decode.insert(Vec(Bits(Global.OPCODE_BITS bits), 8))
    val GROUP_COUNT = pipe.decode.insert(UInt(4 bits))

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

    groupFlow.valid := groupValid
    groupFlow.payload.instructions := instrVec
    groupFlow.payload.count := instrCount

    // Expose the current group on decode when ready
    pipe.decode.haltWhen(!groupValid)
    pipe.decode(GROUP_INSTR) := instrVec
    pipe.decode(GROUP_COUNT) := instrCount
    when(pipe.decode.down.isFiring && groupValid) {
      groupValid := False
      instrCount := 0
    }

    links = Seq() // No additional pipeline links
  }
}

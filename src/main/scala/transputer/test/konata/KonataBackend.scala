package transputer.test.konata

import java.io.{File, PrintWriter}
import scala.collection.mutable

/** Konata log format backend for T9000 pipeline visualization.
  *
  * Implements Konata format version 4 for visualizing instruction flow through the T9000's 5-stage
  * pipeline:
  *   - Fetch/Group (F)
  *   - Local/Decode (D)
  *   - Address/Cache (A)
  *   - Execute (X)
  *   - Writeback (W)
  *
  * Reference: https://github.com/shioyadan/Konata/wiki/KanataFileFormat
  */
class KonataBackend(file: File) {
  private val writer = new PrintWriter(file)
  private var currentCycle: Long = 0
  private var instructionId: Long = 0
  private val activeInstructions = mutable.HashMap[Long, InstructionInfo]()
  private val pendingEvents = mutable.Queue[KonataEvent]()

  // Write header
  writer.println("Kanata\t0004")

  /** Instruction tracking information */
  case class InstructionInfo(
    id: Long,
    pc: Long,
    mnemonic: String,
    var retireId: Long = -1,
    lanes: mutable.Set[Int] = mutable.Set(0)
  )

  /** Base trait for Konata events */
  sealed trait KonataEvent {
    def cycle: Long
  }

  case class CycleEvent(cycle: Long) extends KonataEvent
  case class InsnStartEvent(cycle: Long, pc: Long, mnemonic: String, threadId: Int = 0)
      extends KonataEvent
  case class StageStartEvent(cycle: Long, insnId: Long, stage: String, lane: Int = 0)
      extends KonataEvent
  case class StageEndEvent(cycle: Long, insnId: Long, stage: String, lane: Int = 0)
      extends KonataEvent
  case class RetireEvent(cycle: Long, insnId: Long, flush: Boolean = false) extends KonataEvent
  case class DependencyEvent(cycle: Long, consumerId: Long, producerId: Long) extends KonataEvent
  case class CommentEvent(cycle: Long, insnId: Long, text: String, mouseOver: Boolean = true)
      extends KonataEvent

  /** Add event to pending queue */
  def addEvent(event: KonataEvent): Unit = {
    pendingEvents += event
  }

  /** Start tracking a new instruction */
  def startInstruction(cycle: Long, pc: Long, mnemonic: String, threadId: Int = 0): Long = {
    val id = instructionId
    instructionId += 1
    addEvent(InsnStartEvent(cycle, pc, mnemonic, threadId))
    id
  }

  /** Start a pipeline stage for an instruction */
  def startStage(cycle: Long, insnId: Long, stage: String, lane: Int = 0): Unit = {
    addEvent(StageStartEvent(cycle, insnId, stage, lane))
  }

  /** End a pipeline stage for an instruction */
  def endStage(cycle: Long, insnId: Long, stage: String, lane: Int = 0): Unit = {
    addEvent(StageEndEvent(cycle, insnId, stage, lane))
  }

  /** Retire or flush an instruction */
  def retire(cycle: Long, insnId: Long, flush: Boolean = false): Unit = {
    addEvent(RetireEvent(cycle, insnId, flush))
  }

  /** Add a dependency between instructions */
  def addDependency(cycle: Long, consumerId: Long, producerId: Long): Unit = {
    addEvent(DependencyEvent(cycle, consumerId, producerId))
  }

  /** Add comment to instruction */
  def addComment(cycle: Long, insnId: Long, text: String, mouseOver: Boolean = true): Unit = {
    addEvent(CommentEvent(cycle, insnId, text, mouseOver))
  }

  /** Process pending events up to current cycle */
  def processPendingEvents(upToCycle: Long): Unit = {
    // Sort events by cycle
    val sortedEvents = pendingEvents.sortBy(_.cycle)
    pendingEvents.clear()

    var lastWrittenCycle = currentCycle
    var retireCounter = 0L

    for (event <- sortedEvents if event.cycle <= upToCycle) {
      // Write cycle advancement if needed
      if (event.cycle > lastWrittenCycle) {
        val delta = event.cycle - lastWrittenCycle
        if (lastWrittenCycle == 0) {
          writer.println(s"C=\t$delta")
        } else {
          writer.println(s"C\t$delta")
        }
        lastWrittenCycle = event.cycle
      }

      // Process event
      event match {
        case InsnStartEvent(_, pc, mnemonic, threadId) =>
          val id = activeInstructions.size
          val info = InstructionInfo(id, pc, mnemonic)
          activeInstructions(id) = info
          writer.println(s"I\t$id\t$pc\t$threadId")
          writer.println(s"L\t$id\t0\t${pc.toHexString}: $mnemonic")

        case StageStartEvent(_, insnId, stage, lane) =>
          if (activeInstructions.contains(insnId)) {
            activeInstructions(insnId).lanes += lane
            writer.println(s"S\t$insnId\t$lane\t$stage")
          }

        case StageEndEvent(_, insnId, stage, lane) =>
          if (activeInstructions.contains(insnId)) {
            writer.println(s"E\t$insnId\t$lane\t$stage")
          }

        case RetireEvent(_, insnId, flush) =>
          if (activeInstructions.contains(insnId)) {
            val retireId =
              if (flush) 0
              else {
                retireCounter += 1
                retireCounter
              }
            val retireType = if (flush) 1 else 0
            writer.println(s"R\t$insnId\t$retireId\t$retireType")
            activeInstructions -= insnId
          }

        case DependencyEvent(_, consumerId, producerId) =>
          if (activeInstructions.contains(consumerId) && activeInstructions.contains(producerId)) {
            writer.println(s"W\t$consumerId\t$producerId\t0")
          }

        case CommentEvent(_, insnId, text, mouseOver) =>
          if (activeInstructions.contains(insnId)) {
            val commentType = if (mouseOver) 1 else 0
            writer.println(s"L\t$insnId\t$commentType\t$text")
          }

        case _ => // Ignore other events
      }
    }

    currentCycle = upToCycle
  }

  /** Flush pending events and advance to cycle */
  def flush(cycle: Long): Unit = {
    processPendingEvents(cycle)
    writer.flush()
  }

  /** Close the log file */
  def close(): Unit = {
    processPendingEvents(currentCycle)
    writer.close()
  }

  /** Helper to create a flusher that can be called periodically during simulation */
  def spinalSimFlusher(period: Long): this.type = {
    import spinal.core.sim._
    fork {
      while (true) {
        sleep(period)
        flush(simTime() / 10) // Convert from ps to cycles (assuming 10ps clock)
      }
    }
    this
  }
}

/** T9000-specific Konata helpers */
object T9000Konata {

  /** T9000 pipeline stages */
  object Stage {
    val FETCH = "F" // Fetch/Group
    val DECODE = "D" // Local/Decode
    val ADDRESS = "A" // Address/Cache
    val EXECUTE = "X" // Execute
    val WRITEBACK = "W" // Writeback
  }

  /** T9000 instruction decoder for mnemonic generation */
  def decodeMnemonic(opcode: Int, operand: Int): String = {
    val primaryOp = (opcode >> 4) & 0xf
    val directOperand = opcode & 0xf

    primaryOp match {
      case 0x0 => s"j ${operand.toHexString}"
      case 0x1 => s"ldlp $operand"
      case 0x2 => s"pfix ${operand.toHexString}"
      case 0x3 => s"ldnl $operand"
      case 0x4 => s"ldc $operand"
      case 0x5 => s"ldnlp $operand"
      case 0x6 => s"nfix ${operand.toHexString}"
      case 0x7 => s"ldl $operand"
      case 0x8 => s"adc $operand"
      case 0x9 => s"call ${operand.toHexString}"
      case 0xa => s"cj ${operand.toHexString}"
      case 0xb => s"ajw $operand"
      case 0xc => s"eqc $operand"
      case 0xd => s"stl $operand"
      case 0xe => s"stnl $operand"
      case 0xf => decodeSecondaryOp(directOperand)
      case _ => s"unknown($opcode)"
    }
  }

  private def decodeSecondaryOp(op: Int): String = op match {
    case 0x00 => "rev"
    case 0x01 => "lb"
    case 0x02 => "bsub"
    case 0x03 => "endp"
    case 0x04 => "diff"
    case 0x05 => "add"
    case 0x06 => "dup"
    case 0x07 => "gcall"
    case 0x08 => "in"
    case 0x09 => "prod"
    case 0x0a => "gt"
    case 0x0b => "wsub"
    case 0x0c => "out"
    case 0x0d => "sub"
    case 0x0e => "startp"
    case 0x0f => "outbyte"
    case 0x10 => "outword"
    case 0x11 => "seterr"
    case 0x42 => "mint"
    case 0x18 => "sthf"
    case 0x19 => "stlf"
    case 0x54 => "sttimer"
    case 0x29 => "testerr"
    case 0x57 => "clrhalterr"
    case 0x58 => "sethalterr"
    case 0x9c => "fptesterr"
    case _ => s"opr($op)"
  }

  /** Example usage for simulation */
  def example(konata: KonataBackend): Unit = {
    // Cycle 0: Fetch instruction
    val id0 = konata.startInstruction(0, 0x80000000L, "ldc 10", 0)
    konata.startStage(0, id0, Stage.FETCH)

    // Cycle 1: Decode, fetch next
    konata.endStage(1, id0, Stage.FETCH)
    konata.startStage(1, id0, Stage.DECODE)
    val id1 = konata.startInstruction(1, 0x80000001L, "ldc 20", 0)
    konata.startStage(1, id1, Stage.FETCH)

    // Cycle 2: Execute first, decode second
    konata.endStage(2, id0, Stage.DECODE)
    konata.startStage(2, id0, Stage.EXECUTE)
    konata.endStage(2, id1, Stage.FETCH)
    konata.startStage(2, id1, Stage.DECODE)

    // Cycle 3: Writeback first, execute second with dependency
    konata.endStage(3, id0, Stage.EXECUTE)
    konata.startStage(3, id0, Stage.WRITEBACK)
    konata.endStage(3, id1, Stage.DECODE)
    konata.startStage(3, id1, Stage.EXECUTE)
    konata.addDependency(3, id1, id0) // Second depends on first

    // Cycle 4: Retire first, writeback second
    konata.endStage(4, id0, Stage.WRITEBACK)
    konata.retire(4, id0)
    konata.endStage(4, id1, Stage.EXECUTE)
    konata.startStage(4, id1, Stage.WRITEBACK)

    // Cycle 5: Retire second
    konata.endStage(5, id1, Stage.WRITEBACK)
    konata.retire(5, id1)

    // Flush to file
    konata.flush(5)
  }
}

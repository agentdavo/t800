package t800.plugins.grouper

import spinal.core._
import spinal.lib._
import spinal.lib.misc.plugin.{PluginHost, FiberPlugin}
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import t800.{Global, Opcodes}
import t800.plugins.{PipelineSrv, RegfileService, Fetch}
import t800.plugins.registers.RegName
import t800.plugins.pipeline.{PipelineService, PipelineSrv}

/** Assembles groups of up to eight instructions from the fetch stage,
  * respecting pipeline stage constraints and dependencies,
  * for delivery to the PrimaryInstrPlugin (decode) stage.
  */
class GrouperPlugin extends FiberPlugin with PipelineService {
  val version = "GrouperPlugin v0.5"
  private val retain = Retainer()

  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
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
    println(s"[${this.getDisplayName()}] setup end")
  }

  private var instrVec: Vec[Bits] = null
  private var instrCount: UInt = null
  private var groupValid: Bool = null
  private var groupFlow: Flow[GroupedInstructions] = null
  private var links: Seq[Link] = Seq()

  override def getLinks(): Seq[Link] = links

  during build new Area {
    println(s"[${this.getDisplayName()}] build start")
    retain.await()
    implicit val h: PluginHost = host
    val pipe = Plugin[PipelineSrv]
    val regfile = Plugin[RegfileService]

    val out = CtrlLink()
    val toDecode = StageLink(out.down, pipe.decode.up)
    val GROUP_INSTR = out.insert(Vec(Bits(Global.OPCODE_BITS bits), 8))
    val GROUP_COUNT = out.insert(UInt(4 bits))

    // Input opcodes from FetchPlugin
    val fetchOpcodes = pipe.fetch(Fetch.FETCH_OPCODES) // Vec[Bits(8 bits), 8]
    val fetchValid = pipe.fetch.isValid
    pipe.fetch.haltWhen(!out.down.isReady) // Back-pressure fetch stage

    // Track pipeline stage usage
    case class StageUsage() extends Bundle {
      val fetch = UInt(2 bits) // Up to 2 LDL/LDLP
      val address = UInt(2 bits) // Up to 2 WSUB/LDNL/STNL
      val load = UInt(2 bits) // Up to 2 LDNL
      val execute = UInt(1 bit) // 1 ALU/FPU
      val writeBranch = UInt(1 bit) // 1 CJ/J/STNL
    }
    val currentUsage = Reg(StageUsage()) init StageUsage().getZero
    val canAddInstr = Reg(Bool()) init True

    // Dependency tracking
    case class RegAccess() extends Bundle {
      val read = Vec(Bool(), RegName.elements.size)
      val write = Vec(Bool(), RegName.elements.size)
    }
    val regAccesses = Vec(Reg(RegAccess()), 8)
    regAccesses.foreach(_.init(RegAccess().getZero))

    // Memory dependency tracking
    val memWritePending = Reg(Bool()) init False

    // Group formation
    val instrIndex = Reg(UInt(4 bits)) init 0
    when(fetchValid && canAddInstr) {
      val opcode = fetchOpcodes(instrIndex)
      val isPrimary = Opcodes.PrimaryEnum().decode(opcode(7 downto 4))
      val isSecondary = isPrimary === Opcodes.PrimaryEnum.OPR
      val secondaryOpcode = isSecondary ? Opcodes.SecondaryEnum().decode(opcode) | Opcodes.SecondaryEnum.REV

      // Determine stage usage
      val newUsage = StageUsage()
      newUsage := currentUsage
      when(isPrimary.isOneOf(Opcodes.PrimaryEnum.LDL, Opcodes.PrimaryEnum.LDLP)) {
        newUsage.fetch := currentUsage.fetch + 1
      } elsewhen(isPrimary.isOneOf(Opcodes.PrimaryEnum.LDNL, Opcodes.PrimaryEnum.STNL)) {
        newUsage.address := currentUsage.address + 1
        newUsage.load := currentUsage.load + 1
      } elsewhen(isSecondary.isOneOf(Opcodes.SecondaryEnum.ADD, Opcodes.SecondaryEnum.SUB, Opcodes.SecondaryEnum.FPADD, Opcodes.SecondaryEnum.FPSUB)) {
        newUsage.execute := currentUsage.execute + 1
      } elsewhen(isPrimary.isOneOf(Opcodes.PrimaryEnum.CJ, Opcodes.PrimaryEnum.J)) {
        newUsage.writeBranch := currentUsage.writeBranch + 1
      } elsewhen(isPrimary === Opcodes.PrimaryEnum.STNL) {
        newUsage.writeBranch := currentUsage.writeBranch + 1
      }

      // Check dependencies
      val regAccess = RegAccess()
      regAccess.read.foreach(_ := False)
      regAccess.write.foreach(_ := False)
      when(isPrimary.isOneOf(Opcodes.PrimaryEnum.LDL)) {
        regAccess.read(RegName.WdescReg.id) := True
        regAccess.write(RegName.Areg.id) := True
      } elsewhen(isPrimary.isOneOf(Opcodes.PrimaryEnum.LDLP)) {
        regAccess.read(RegName.WdescReg.id) := True
        regAccess.write(RegName.Areg.id) := True
      } elsewhen(isPrimary.isOneOf(Opcodes.PrimaryEnum.LDNL)) {
        regAccess.read(RegName.Areg.id) := True
        regAccess.write(RegName.Areg.id) := True
      } elsewhen(isPrimary.isOneOf(Opcodes.PrimaryEnum.STNL)) {
        regAccess.read(RegName.Areg.id) := True
        regAccess.read(RegName.Breg.id) := True
        regAccess.write(RegName.Areg.id) := True
      } elsewhen(isSecondary.isOneOf(Opcodes.SecondaryEnum.ADD, Opcodes.SecondaryEnum.SUB)) {
        regAccess.read(RegName.Areg.id) := True
        regAccess.read(RegName.Breg.id) := True
        regAccess.write(RegName.Areg.id) := True
      } elsewhen(isSecondary.isOneOf(Opcodes.SecondaryEnum.FPADD, Opcodes.SecondaryEnum.FPSUB)) {
        regAccess.read(RegName.FPAreg.id) := True
        regAccess.read(RegName.FPBreg.id) := True
        regAccess.write(RegName.FPAreg.id) := True
      }

      // Check register conflicts
      val hasRegConflict = regAccesses.take(instrCount).map { prev =>
        (prev.write & regAccess.read).orR || (prev.write & regAccess.write).orR
      }.orR

      // Check memory conflicts
      val hasMemConflict = memWritePending && isPrimary.isOneOf(Opcodes.PrimaryEnum.LDNL, Opcodes.PrimaryEnum.STNL)
      when(isPrimary === Opcodes.PrimaryEnum.STNL) {
        memWritePending := True
      }

      // Add instruction to group if valid
      when(!hasRegConflict && !hasMemConflict && newUsage.fetch <= 2 && newUsage.address <= 2 && newUsage.load <= 2 && newUsage.execute <= 1 && newUsage.writeBranch <= 1) {
        instrVec(instrCount) := opcode
        regAccesses(instrCount) := regAccess
        currentUsage := newUsage
        instrCount := instrCount + 1
        when(instrCount === 7 || newUsage.writeBranch === 1) {
          groupValid := True
          canAddInstr := False
        }
      } else {
        canAddInstr := False
        groupValid := True
      }
      instrIndex := instrIndex + 1
      when(instrIndex === 7) {
        instrIndex := 0
      }
    }

    // Send group to decode stage
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
      instrIndex := 0
      currentUsage := StageUsage().getZero
      canAddInstr := True
      memWritePending := False
      regAccesses.foreach(_.init(RegAccess().getZero))
    }

    groupFlow.valid := out.down.isValid
    groupFlow.payload.instructions := out.down(GROUP_INSTR)
    groupFlow.payload.count := out.down(GROUP_COUNT)

    links = Seq(toDecode)
    println(s"[${this.getDisplayName()}] build end")
  }
}
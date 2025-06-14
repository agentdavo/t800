import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.database._

// Service interface for providing grouped instructions
case class GroupedInstructions() extends Bundle with IMasterSlave {
  val instructions = Vec(Bits(16 bits), 8) // Up to 8 instructions
  val count = UInt(4 bits)                // Number of valid instructions

  override def asMaster(): Unit = {
    out(instructions, count)
  }
}

/**
 * T9000 Grouper Plugin
 *
 * Models the T9000's instruction grouper, assembling fetched instructions into groups
 * for superscalar execution based on resource constraints and dependencies
 */
class GrouperPlugin extends FiberPlugin {
  val version = "GrouperPlugin v2.1"
  report(L"Initializing $version")

  // Database keys
  object DBKeys {
    val GROUP_INSTR = Database.blocking[Vec[Bits]]()
    val GROUP_COUNT = Database.blocking[UInt]()
  }

  // Payload definitions
  lazy val DECODED_INSTR = Payload(Bits(16 bits))
  lazy val GROUP_INSTR = Payload(Vec(Bits(16 bits), 8))
  lazy val GROUP_COUNT = Payload(UInt(4 bits))

  // Service instance
  lazy val srv = during setup new Area {
    val service = master(GroupedInstructions())
    addService(service)
  }

  // Pipeline integration
  buildBefore(retains(host[CorePipelinePlugin].lock, host[MemoryManagementPlugin].lock).lock)

  lazy val logic = during build new Area {
    val fetch = host.find[StageCtrlPipeline].ctrl(0)
    val group = host.find[StageCtrlPipeline].ctrl(1)

    // Resource limits from T9000 HRM
    val MAX_LOCAL_LOADS = 2      // Dual-ported workspace cache
    val MAX_ADDR_CALCS = 2       // Non-local or indexed accesses
    val MAX_NON_LOCAL_LOADS = 2  // Main cache ports
    val MAX_ALU_FPU_OPS = 1      // Single ALU/FPU unit
    val MAX_WRITE_JUMPS = 1      // Single write-back/jump unit

    // Instruction buffer
    val instrBuffer = new StreamFifo(Bits(16 bits), 32)
    instrBuffer.io.push << fetch.toStream.translateWith(fetch(DECODED_INSTR))

    // Grouping logic
    val groupFormation = new Area {
      val group = Reg(Vec(Bits(16 bits), 8)) init(Vec.fill(8)(B(0)))
      val groupCount = Reg(UInt(4 bits)) init(0)

      // Resource counters
      val localLoads = Reg(UInt(2 bits)) init(0)
      val addrCalcs = Reg(UInt(2 bits)) init(0)
      val nonLocalLoads = Reg(UInt(2 bits)) init(0)
      val aluOps = Reg(UInt(1 bit)) init(0)
      val writeJumps = Reg(UInt(1 bit)) init(0)

      // Dependency tracking: simplified stack-based check
      val stackDependency = Reg(Bool()) init(False)

      // Reset counters when group is sent
      when(group.isValid && group.isReady) {
        localLoads := 0
        addrCalcs := 0
        nonLocalLoads := 0
        aluOps := 0
        writeJumps := 0
        groupCount := 0
        stackDependency := False
      }

      // Instruction classification
      when(instrBuffer.io.pop.valid && !group.isValid) {
        val currentInstr = instrBuffer.io.pop.payload
        val opcode = currentInstr(15 downto 8)
        var canAdd = True

        switch(opcode) {
          is(B"x07") { // ldl
            when(localLoads >= MAX_LOCAL_LOADS || groupCount >= 8) {
              canAdd := False
            } otherwise {
              localLoads := localLoads + 1
              stackDependency := True // Pushes to stack
            }
          }
          is(B"xF2") { // wsub
            when(addrCalcs >= MAX_ADDR_CALCS || groupCount >= 8 || !stackDependency) {
              canAdd := False
            } otherwise {
              addrCalcs := addrCalcs + 1
              stackDependency := True // Consumes and produces stack value
            }
          }
          is(B"x0A") { // ldnl
            when(nonLocalLoads >= MAX_NON_LOCAL_LOADS || groupCount >= 8 || !stackDependency) {
              canAdd := False
            } otherwise {
              nonLocalLoads := nonLocalLoads + 1
              stackDependency := True // Loads to stack
            }
          }
          is(B"xF5") { // add
            when(aluOps >= MAX_ALU_FPU_OPS || groupCount >= 8 || !stackDependency) {
              canAdd := False
            } otherwise {
              aluOps := aluOps + 1
              stackDependency := True // Consumes two, produces one
            }
          }
          is(B"x0D", B"x0E") { // stl, stnl
            when(writeJumps >= MAX_WRITE_JUMPS || groupCount >= 8 || !stackDependency) {
              canAdd := False
            } otherwise {
              writeJumps := writeJumps + 1
              stackDependency := False // Consumes stack
            }
          }
          default {
            canAdd := groupCount < 8 // Simplified for other instructions
          }
        }

        when(canAdd) {
          group(groupCount) := currentInstr
          groupCount := groupCount + 1
          instrBuffer.io.pop.ready := True
        } otherwise {
          group.isValid := groupCount > 0 // Finalize group
        }
      }

      // Connect to service interface
      srv.instructions := group
      srv.count := groupCount

      // Drive pipeline
      group.isValid := groupCount > 0
      group(GROUP_INSTR) := group
      group(GROUP_COUNT) := groupCount
      DBKeys.GROUP_INSTR.set(group)
      DBKeys.GROUP_COUNT.set(groupCount)
    }

    // Debug logging
    when(group.isValid && debugEn) {
      report(L"GROUP count=$GROUP_COUNT")
    }
  }
}

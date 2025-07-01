package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import transputer.{Global, Opcode}
import transputer.plugins.core.pipeline.PipelineStageService

/** T9000 Pipeline Stage Assignment Plugin
  *
  * This plugin manages the assignment of instructions to their appropriate
  * pipeline stages according to the T9000 architecture specification.
  *
  * Stage assignments:
  * - Stage 1 (Fetch/Group): Instruction fetch, grouping, and LDL
  * - Stage 2 (Local/Decode): Workspace cache access, decode, address calc
  * - Stage 3 (Address/Cache): Main cache access, LDNL, memory operations
  * - Stage 4 (Execute): ALU and FPU operations
  * - Stage 5 (Writeback): Result writeback, branches, stores
  */
class StageAssignmentPlugin extends FiberPlugin {
  override def getDisplayName(): String = "StageAssignmentPlugin"
  setName("stageAssignment")
  
  // Service references
  var pipeline: PipelineStageService = null
  
  during setup new Area {
    println(s"[${getDisplayName()}] Setting up stage assignments")
  }
  
  during build new Area {
    println(s"[${getDisplayName()}] Building stage assignment logic")
    
    // Get pipeline service
    pipeline = host[PipelineStageService]
    
    // Create stage assignment logic for each stage
    setupFetchGroupStage()
    setupLocalDecodeStage()
    setupAddressCacheStage()
    setupExecuteStage()
    setupWritebackStage()
    
    println(s"[${getDisplayName()}] Stage assignment complete")
  }
  
  def setupFetchGroupStage(): Unit = new Area {
    val stage = pipeline.fetch
    val opcode = stage(Global.OPCODE)
    val iptr = stage(Global.IPTR)
    
    // Instruction fetch happens here
    // Grouper output is available at end of this stage
    
    // LDL (Load Local) can execute in fetch stage using workspace cache
    val isLdl = opcode === Opcode.PrimaryOpcode.LDL.asBits.resize(8)
    when(isLdl) {
      // Mark for workspace cache access
      stage(Global.MEM_ADDR) := Global.getCurrentWptr() + (stage(Global.OPERAND).asUInt << 2)
      stage(Global.MEM_WRITE) := False
    }
  }
  
  def setupLocalDecodeStage(): Unit = new Area {
    val stage = pipeline.decode
    val opcode = stage(Global.OPCODE)
    
    // Decode and register file read
    // Stack values are read here
    
    // Instructions that need workspace cache in this stage:
    // - STL (Store Local)
    val isStl = opcode === Opcode.PrimaryOpcode.STL.asBits.resize(8)
    when(isStl) {
      stage(Global.MEM_ADDR) := Global.getCurrentWptr() + (stage(Global.OPERAND).asUInt << 2)
      stage(Global.MEM_DATA) := stage(Global.AREG_VALUE).asBits
      stage(Global.MEM_WRITE) := True
    }
    
    // Address calculations for memory operations
    val isLdnl = opcode === Opcode.PrimaryOpcode.LDNL.asBits.resize(8)
    val isStnl = opcode === Opcode.PrimaryOpcode.STNL.asBits.resize(8)
    when(isLdnl || isStnl) {
      // Calculate effective address
      val base = Mux(isLdnl, stage(Global.AREG_VALUE), stage(Global.BREG_VALUE))
      stage(Global.MEM_ADDR) := base + (stage(Global.OPERAND).asUInt << 2)
    }
  }
  
  def setupAddressCacheStage(): Unit = new Area {
    val stage = pipeline.memory  // Using memory stage for address/cache
    val opcode = stage(Global.OPCODE)
    
    // Main cache access happens here
    // LDNL and STNL execute in this stage
    
    val isLdnl = opcode === Opcode.PrimaryOpcode.LDNL.asBits.resize(8)
    val isStnl = opcode === Opcode.PrimaryOpcode.STNL.asBits.resize(8)
    
    when(isLdnl) {
      stage(Global.MEM_WRITE) := False
      // Cache read initiated
    }
    
    when(isStnl) {
      stage(Global.MEM_WRITE) := True
      stage(Global.MEM_DATA) := stage(Global.AREG_VALUE).asBits
      // Cache write initiated
    }
  }
  
  def setupExecuteStage(): Unit = new Area {
    val stage = pipeline.execute
    val opcode = stage(Global.OPCODE)
    val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
    
    // ALU and FPU operations execute here
    // This is where most computation happens
    
    when(isOpr) {
      val oprFunc = opcode(3 downto 0)
      
      // ALU operations
      val isAluOp = oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.SUB.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.AND.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.OR.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.XOR.asBits.resize(4)
      
      // FPU operations  
      val isFpuOp = oprFunc === Opcode.SecondaryOpcode.FPADD.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.FPSUB.asBits.resize(4) ||
                    oprFunc === Opcode.SecondaryOpcode.FPMUL.asBits.resize(4)
      
      // Mark which execution unit to use
      when(isAluOp) {
        // ALU lane will handle this
      }
      
      when(isFpuOp) {
        // FPU lane will handle this
      }
    }
  }
  
  def setupWritebackStage(): Unit = new Area {
    val stage = pipeline.writeBack
    val opcode = stage(Global.OPCODE)
    
    // Result writeback to register file
    // Branch resolution
    // Store completion
    
    // Jump instructions resolve here
    val isJump = opcode === Opcode.PrimaryOpcode.J.asBits.resize(8)
    val isCall = opcode === Opcode.PrimaryOpcode.CALL.asBits.resize(8)
    val isCj = opcode === Opcode.PrimaryOpcode.CJ.asBits.resize(8)
    
    when(isJump || isCall) {
      stage(Global.BRANCH_TAKEN) := True
      stage(Global.BRANCH_TARGET) := stage(Global.IPTR) + stage(Global.OPERAND).asSInt.resize(Global.AddrBits).asUInt
    }
    
    when(isCj) {
      stage(Global.BRANCH_TAKEN) := stage(Global.AREG_VALUE) === 0
      stage(Global.BRANCH_TARGET) := stage(Global.IPTR) + stage(Global.OPERAND).asSInt.resize(Global.AddrBits).asUInt
    }
  }
}

/** Helper object for current workspace pointer */
object Global {
  def getCurrentWptr(): UInt = {
    // This would come from the register file service
    // For now, return a placeholder
    U(0, AddrBits bits)
  }
}
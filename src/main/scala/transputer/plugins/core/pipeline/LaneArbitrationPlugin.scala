package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import transputer.{Global, Opcode}
import transputer.plugins.core.pipeline.PipelineStageService

/** Lane arbitration plugin for parallel ALU/FPU/Memory execution
  *
  * This plugin manages the CtrlLaneApi instances created by T9000PipelinePlugin
  * to enable parallel execution of different instruction types while maintaining
  * proper dependencies and hazard detection.
  *
  * Key features:
  * - ALU operations execute in parallel with FPU operations
  * - Memory operations can overlap with computation
  * - Automatic stall generation for lane conflicts
  * - Forwarding paths between lanes
  */
class LaneArbitrationPlugin extends FiberPlugin {
  override def getDisplayName(): String = "LaneArbitrationPlugin"
  setName("laneArbitration")
  
  // Service references
  var pipeline: T9000PipelinePlugin = null
  
  // Lane usage tracking
  case class LaneRequest() extends Bundle {
    val valid = Bool()
    val opcode = Bits(8 bits)
    val operandA = UInt(32 bits)
    val operandB = UInt(32 bits)
    val destReg = UInt(6 bits)  // Register destination
  }
  
  during setup new Area {
    println(s"[${getDisplayName()}] Setting up lane arbitration")
  }
  
  during build new Area {
    println(s"[${getDisplayName()}] Building lane arbitration logic")
    
    // Get T9000 pipeline service
    pipeline = host[T9000PipelinePlugin]
    
    // Get the execution lanes
    val aluLane = pipeline.getAluLane
    val fpuLane = pipeline.getFpuLane
    val memLane = pipeline.getMemLane
    
    // Create lane request logic for each lane
    setupAluLane(aluLane)
    setupFpuLane(fpuLane)
    setupMemLane(memLane)
    
    // Add forwarding logic between lanes
    setupForwardingPaths()
    
    // Add performance monitoring
    setupPerformanceCounters()
    
    println(s"[${getDisplayName()}] Lane arbitration complete")
  }
  
  def setupAluLane(lane: CtrlLaneApi): Unit = new Area {
    val stage = pipeline.executeStage
    val opcode = stage(Global.OPCODE)
    val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
    
    // ALU operations that can execute in this lane
    val isAluOp = isOpr && {
      val oprFunc = opcode(3 downto 0)
      oprFunc === Opcode.SecondaryOpcode.ADD.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SUB.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.MUL.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.AND.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.OR.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.XOR.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.NOT.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SHL.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.SHR.asBits.resize(4)
    }
    
    // Request ALU lane when needed
    lane.enable := stage.isValid && isAluOp
    
    // Stall if lane is busy (multi-cycle operations)
    val aluBusy = Reg(Bool()) init(False)
    val cyclesRemaining = Reg(UInt(3 bits)) init(0)
    
    when(lane.isUsed) {
      // Simple ALU ops are single cycle
      // MUL takes 3 cycles
      val isMul = opcode(3 downto 0) === Opcode.SecondaryOpcode.MUL.asBits.resize(4)
      cyclesRemaining := isMul ? U(2) | U(0)
      aluBusy := isMul
    }
    
    when(aluBusy && cyclesRemaining =/= 0) {
      cyclesRemaining := cyclesRemaining - 1
      lane.halt := True
    } elsewhen(aluBusy && cyclesRemaining === 0) {
      aluBusy := False
    }
    
    // ALU result available after operation completes
    when(lane.isUsed && !aluBusy) {
      stage(Global.ALU_RESULT) := performAluOperation(
        opcode(3 downto 0),
        stage(Global.AREG_VALUE),
        stage(Global.BREG_VALUE)
      )
    }
  }
  
  def setupFpuLane(lane: CtrlLaneApi): Unit = new Area {
    val stage = pipeline.executeStage
    val opcode = stage(Global.OPCODE)
    val isOpr = opcode(7 downto 4) === Opcode.PrimaryOpcode.OPR.asBits.resize(4)
    
    // FPU operations that can execute in this lane
    val isFpuOp = isOpr && {
      val oprFunc = opcode(3 downto 0)
      oprFunc === Opcode.SecondaryOpcode.FPADD.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.FPSUB.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.FPMUL.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.FPDIV.asBits.resize(4) ||
      oprFunc === Opcode.SecondaryOpcode.FPSQRT.asBits.resize(4)
    }
    
    // Request FPU lane when needed
    lane.enable := stage.isValid && isFpuOp
    
    // FPU operations have variable latency
    val fpuBusy = Reg(Bool()) init(False)
    val fpuCyclesRemaining = Reg(UInt(7 bits)) init(0)
    
    when(lane.isUsed) {
      val oprFunc = opcode(3 downto 0)
      fpuCyclesRemaining := oprFunc.mux(
        Opcode.SecondaryOpcode.FPADD.asBits.resize(4) -> U(2),
        Opcode.SecondaryOpcode.FPSUB.asBits.resize(4) -> U(2),
        Opcode.SecondaryOpcode.FPMUL.asBits.resize(4) -> U(4),
        Opcode.SecondaryOpcode.FPDIV.asBits.resize(4) -> U(30),
        Opcode.SecondaryOpcode.FPSQRT.asBits.resize(4) -> U(40),
        default -> U(1)
      )
      fpuBusy := True
    }
    
    when(fpuBusy && fpuCyclesRemaining =/= 0) {
      fpuCyclesRemaining := fpuCyclesRemaining - 1
      lane.halt := True
    } elsewhen(fpuBusy && fpuCyclesRemaining === 0) {
      fpuBusy := False
      // FPU result would be written here
      stage(Global.FPU_RESULT) := B(0, 64 bits) // Placeholder
    }
  }
  
  def setupMemLane(lane: CtrlLaneApi): Unit = new Area {
    val stage = pipeline.addressCacheStage
    val opcode = stage(Global.OPCODE)
    
    // Memory operations execute in Address/Cache stage
    val isMemOp = {
      opcode === Opcode.PrimaryOpcode.LDNL.asBits.resize(8) ||
      opcode === Opcode.PrimaryOpcode.STNL.asBits.resize(8) ||
      opcode === Opcode.PrimaryOpcode.LDNLP.asBits.resize(8)
    }
    
    // Request memory lane when needed
    lane.enable := stage.isValid && isMemOp
    
    // Memory operations may stall on cache miss
    val memStall = Bool()
    memStall := False // Would connect to cache miss signal
    
    lane.halt := memStall
  }
  
  def setupForwardingPaths(): Unit = new Area {
    // Forward ALU results to FPU lane
    val aluToFpuForward = new Area {
      val executeStage = pipeline.executeStage
      val aluResult = executeStage(Global.ALU_RESULT)
      val needsForward = Bool()
      
      // Detect when FPU needs ALU result
      needsForward := False // Placeholder for dependency check
      
      when(needsForward) {
        // Forward ALU result to FPU operand
      }
    }
    
    // Forward memory data to ALU/FPU
    val memToExecForward = new Area {
      val memStage = pipeline.addressCacheStage
      val execStage = pipeline.executeStage
      
      val memData = memStage(Global.MEM_DATA)
      val needsForward = Bool()
      
      // Detect when execution needs memory data
      needsForward := False // Placeholder
      
      when(needsForward) {
        // Forward memory data
      }
    }
  }
  
  def setupPerformanceCounters(): Unit = new Area {
    // Count lane utilization
    val aluLaneUsage = Reg(UInt(32 bits)) init(0)
    val fpuLaneUsage = Reg(UInt(32 bits)) init(0)
    val memLaneUsage = Reg(UInt(32 bits)) init(0)
    
    when(pipeline.getAluLane.isUsed) {
      aluLaneUsage := aluLaneUsage + 1
    }
    
    when(pipeline.getFpuLane.isUsed) {
      fpuLaneUsage := fpuLaneUsage + 1
    }
    
    when(pipeline.getMemLane.isUsed) {
      memLaneUsage := memLaneUsage + 1
    }
    
    // Count parallel execution cycles
    val parallelExecCycles = Reg(UInt(32 bits)) init(0)
    val isParallelExec = (pipeline.getAluLane.isUsed && pipeline.getFpuLane.isUsed) ||
                         (pipeline.getAluLane.isUsed && pipeline.getMemLane.isUsed) ||
                         (pipeline.getFpuLane.isUsed && pipeline.getMemLane.isUsed)
    
    when(isParallelExec) {
      parallelExecCycles := parallelExecCycles + 1
    }
  }
  
  // Simple ALU operation implementation
  def performAluOperation(op: Bits, a: UInt, b: UInt): UInt = {
    val result = UInt(32 bits)
    
    switch(op) {
      is(Opcode.SecondaryOpcode.ADD.asBits.resize(4)) { result := a + b }
      is(Opcode.SecondaryOpcode.SUB.asBits.resize(4)) { result := a - b }
      is(Opcode.SecondaryOpcode.AND.asBits.resize(4)) { result := a & b }
      is(Opcode.SecondaryOpcode.OR.asBits.resize(4))  { result := a | b }
      is(Opcode.SecondaryOpcode.XOR.asBits.resize(4)) { result := a ^ b }
      is(Opcode.SecondaryOpcode.NOT.asBits.resize(4)) { result := ~a }
      is(Opcode.SecondaryOpcode.SHL.asBits.resize(4)) { result := a |<< b(4 downto 0) }
      is(Opcode.SecondaryOpcode.SHR.asBits.resize(4)) { result := a |>> b(4 downto 0) }
      default { result := 0 }
    }
    
    result
  }
}
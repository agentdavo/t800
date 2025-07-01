package transputer.plugins.core.pipeline

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.core.fiber.Retainer
import spinal.lib.misc.plugin._
import transputer.Global
import transputer.plugins.core.pipeline.PipelineStageService

/** T9000 5-stage pipeline implementation with proper stage assignments.
  *
  * The T9000 pipeline stages are:
  * 1. Fetch/Group - Instruction fetch and hardware grouping
  * 2. Local/Decode - Workspace cache access and decode
  * 3. Address/Cache - Address calculation and main cache access
  * 4. Execute - ALU/FPU execution
  * 5. Writeback - Result writeback and branch resolution
  *
  * This implementation uses SpinalHDL's Pipeline API for automatic register
  * management and timing closure at high frequencies.
  */
class T9000PipelinePlugin extends FiberPlugin with PipelineStageService {
  override def getDisplayName(): String = "T9000PipelinePlugin"
  setName("t9000pipeline")
  
  val elaborationLock = Retainer()
  
  // The main pipeline
  var pipeline: StageCtrlPipeline = null
  
  // Stage control links
  var fetchGroupStage: CtrlLink = null      // Stage 1: Fetch/Group
  var localDecodeStage: CtrlLink = null     // Stage 2: Local/Decode  
  var addressCacheStage: CtrlLink = null    // Stage 3: Address/Cache
  var executeStage: CtrlLink = null         // Stage 4: Execute
  var writebackStage: CtrlLink = null       // Stage 5: Writeback
  
  // Pipeline lanes for parallel execution
  var aluLane: CtrlLaneApi = null
  var fpuLane: CtrlLaneApi = null
  var memLane: CtrlLaneApi = null
  
  during setup new Area {
    println(s"[${getDisplayName()}] Setting up T9000 5-stage pipeline")
    
    // Create the pipeline with proper configuration
    pipeline = new StageCtrlPipeline(
      // Enable automatic register balancing
      withAutoPipeline = true,
      // Set frequency target for timing-driven pipeline
      frequency = 500 MHz
    )
    
    // Create the 5 stages
    fetchGroupStage = pipeline.ctrl(0)
    localDecodeStage = pipeline.ctrl(1)  
    addressCacheStage = pipeline.ctrl(2)
    executeStage = pipeline.ctrl(3)
    writebackStage = pipeline.ctrl(4)
    
    // Name stages for debugging
    fetchGroupStage.setCompositeName(pipeline, "fetchGroup")
    localDecodeStage.setCompositeName(pipeline, "localDecode")
    addressCacheStage.setCompositeName(pipeline, "addressCache")
    executeStage.setCompositeName(pipeline, "execute")
    writebackStage.setCompositeName(pipeline, "writeback")
    
    // Create execution lanes for parallel ALU/FPU/Memory operations
    aluLane = new CtrlLaneApi(executeStage)
    fpuLane = new CtrlLaneApi(executeStage)
    memLane = new CtrlLaneApi(addressCacheStage)
    
    // Register global payloads that flow through the pipeline
    val stages = Seq(fetchGroupStage, localDecodeStage, addressCacheStage, executeStage, writebackStage)
    
    // Core instruction flow payloads
    stages.foreach { stage =>
      stage(Global.OPCODE)        // 8-bit opcode
      stage(Global.IPTR)          // Instruction pointer
      stage(Global.OPERAND)       // Instruction operand
      stage(Global.GROUPED_INSTR) // Grouped instruction bundle
    }
    
    // Data flow payloads (not needed in all stages)
    Seq(localDecodeStage, addressCacheStage, executeStage, writebackStage).foreach { stage =>
      stage(Global.AREG_VALUE)    // A register value
      stage(Global.BREG_VALUE)    // B register value  
      stage(Global.CREG_VALUE)    // C register value
    }
    
    // Memory access payloads
    Seq(addressCacheStage, executeStage, writebackStage).foreach { stage =>
      stage(Global.MEM_ADDR)      // Memory address
      stage(Global.MEM_DATA)      // Memory data
      stage(Global.MEM_WRITE)     // Memory write enable
    }
    
    // Result payloads
    Seq(executeStage, writebackStage).foreach { stage =>
      stage(Global.ALU_RESULT)    // ALU operation result
      stage(Global.FPU_RESULT)    // FPU operation result
      stage(Global.STACK_OP)      // Stack operation type
    }
    
    // Control flow payloads
    writebackStage(Global.BRANCH_TARGET)
    writebackStage(Global.BRANCH_TAKEN)
    
    // Exception/trap payloads
    stages.foreach { stage =>
      stage(Global.TRAP_CAUSE)
      stage(Global.TRAP_ENABLE)
    }
    
    // Touch isFiring to ensure proper initialization
    stages.foreach(_.down.isFiring)
    
    println(s"[${getDisplayName()}] Pipeline structure created with 5 stages")
  }
  
  during build new Area {
    println(s"[${getDisplayName()}] Building T9000 pipeline")
    elaborationLock.await()
    
    // Add pipeline optimizations
    pipeline.withHazardManagement = true  // Enable automatic hazard detection
    pipeline.withBypass = true            // Enable forwarding paths
    
    // Configure lane arbitration
    aluLane.setName("ALU_LANE")
    fpuLane.setName("FPU_LANE") 
    memLane.setName("MEM_LANE")
    
    // Build the pipeline
    pipeline.build()
    
    // Add performance counters
    val cycleCounter = Reg(UInt(32 bits)) init(0)
    cycleCounter := cycleCounter + 1
    
    val instrCounter = Reg(UInt(32 bits)) init(0)
    when(fetchGroupStage.down.isFiring) {
      instrCounter := instrCounter + 1
    }
    
    // Stall detection
    val pipelineStalled = !fetchGroupStage.down.isReady
    val stallCounter = Reg(UInt(32 bits)) init(0)
    when(pipelineStalled) {
      stallCounter := stallCounter + 1
    }
    
    println(s"[${getDisplayName()}] Pipeline build complete")
  }
  
  // ========================================
  // PipelineStageService Implementation
  // ========================================
  
  // Map to legacy 5-stage names for compatibility
  override def fetch = fetchGroupStage
  override def decode = localDecodeStage
  override def execute = executeStage
  override def memory = addressCacheStage
  override def writeBack = writebackStage
  
  // Payload accessors
  override def OPCODE = Global.OPCODE
  override def IPTR = Global.IPTR
  override def MEM_ADDR = Global.MEM_ADDR
  override def MEM_DATA = Global.MEM_DATA
  
  // T9000-specific stage accessors
  def fetchGroup = fetchGroupStage
  def localDecode = localDecodeStage
  def addressCache = addressCacheStage
  
  // Lane accessors for plugins
  def getAluLane = aluLane
  def getFpuLane = fpuLane
  def getMemLane = memLane
}
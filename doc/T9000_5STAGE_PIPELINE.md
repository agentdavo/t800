# T9000 Five-Stage Pipeline Implementation

## Stage Organization

### Stage 1: Fetch/Group
**Combined fetch and instruction grouping**
- **Fetch unit**: Fetches 32 bits/cycle from memory into instruction buffer
- **Group unit**: Processes up to 64 bits/cycle from buffer
- **Output**: Grouped instruction packets ready for execution

**Why combined?**
- Fetch is memory-bound (slower)
- Group is CPU-bound (faster) 
- Group can process multiple cycles of fetched data
- Natural buffering between memory speed and CPU speed

### Stage 2: Local/Decode  
**Local workspace access and decode**
- Decode grouped instructions
- Read 2 operands from tri-ported workspace cache
- Prepare ALU/FPU commands
- Generate non-local addresses

### Stage 3: Address/Cache
**Non-local memory access**
- Perform 2 non-local reads from main cache (4 banks)
- Address calculations from stage 2
- Cache hit/miss detection

### Stage 4: Execute
**Parallel execution units**
- ALU operations (integer)
- FPU operations (floating-point) 
- **Both run in parallel using CtrlLaneApi**
- Condition evaluation

### Stage 5: Writeback
**Result writeback**
- Write 1 result per cycle
- Update register stack
- Handle pipeline hazards

## Implementation Structure

### Combined Fetch/Group Plugin
```scala
class FetchGroupPlugin extends FiberPlugin {
  setName("fetchGroup")
  
  during build new Area {
    val pipe = Plugin[PipelineStageService]
    val systemBus = Plugin[SystemBusService]
    
    // Fetch unit (32-bit fetches)
    val fetchUnit = new Area {
      val buffer = Vec(Reg(Bits(32 bits)), 4) // 16-byte buffer
      val bufferValid = Vec(Reg(Bool()), 4)
      val writePtr = Reg(UInt(2 bits)) init 0
      
      // Fetch state machine
      val fetching = Reg(Bool()) init False
      val fetchAddr = Reg(UInt(32 bits)) init 0
      
      // Fetch 32 bits when buffer has space
      val hasSpace = !bufferValid.reduce(_ && _)
      // ... fetch logic
    }
    
    // Group unit (processes up to 64 bits/cycle)
    val groupUnit = new Area {
      val readPtr = Reg(UInt(3 bits)) init 0 // Byte pointer into buffer
      val groupBuffer = Vec(Reg(Bits(8 bits)), 8) // Up to 8 instructions
      val groupCount = Reg(UInt(3 bits)) init 0
      
      // Instruction extraction and grouping
      // ... grouping logic
      
      // Output to stage 2
      pipe.decode(INSTRUCTION_GROUP) := groupOutput
    }
  }
}
```

### Parallel Execute Plugin
```scala
class ExecutePlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[PipelineStageService]
    
    // Stage 4: Parallel ALU and FPU
    val executeStage = pipe.execute
    
    // ALU lane
    val aluLane = new CtrlLane {
      when(executeStage.isValid && executeStage(HAS_ALU_OP)) {
        val aluCmd = executeStage(ALU_CMD)
        val result = performAluOp(aluCmd)
        executeStage(ALU_RESULT) := result
      }
    }
    
    // FPU lane (runs in parallel)
    val fpuLane = new CtrlLane {
      when(executeStage.isValid && executeStage(HAS_FPU_OP)) {
        val fpuCmd = executeStage(FPU_CMD)
        val result = performFpuOp(fpuCmd)
        executeStage(FPU_RESULT) := result
      }
    }
    
    // Both lanes execute in parallel
    executeStage.laneMux(aluLane, fpuLane)
  }
}
```

## Pipeline Payloads

```scala
// Stage 1 → Stage 2
case class InstructionGroup() extends Bundle {
  val instructions = Vec(Bits(8 bits), 8)
  val count = UInt(3 bits)
  val prefixData = UInt(32 bits)
}

// Stage 2 → Stage 3  
case class DecodedOp() extends Bundle {
  val aluOp = AluCmd()
  val fpuOp = FpCmd()
  val memOp = MemCmd()
  val hasAlu = Bool()
  val hasFpu = Bool()
  val hasMem = Bool()
}

// Stage 3 → Stage 4
case class MemoryOp() extends Bundle {
  val data = Bits(32 bits)
  val valid = Bool()
  val addr = UInt(32 bits)
}

// Stage 4 → Stage 5
case class ExecuteResult() extends Bundle {
  val aluResult = Bits(32 bits)
  val fpuResult = Bits(64 bits)
  val writeReg = RegName()
  val writeEnable = Bool()
}
```

## Benefits of 5-Stage Design

1. **Matches T9000 specification** - Five stages as documented
2. **Natural buffering** - Fetch/group handles speed mismatch
3. **Parallel execution** - ALU/FPU in stage 4
4. **Simple pipeline control** - Standard 5-stage hazard logic
5. **Efficient resource usage** - Group unit keeps execution units fed

## Plugin Organization

```
FetchGroupPlugin      → Stage 1 (Fetch + Group)
LocalDecodePlugin     → Stage 2 (Workspace + Decode)  
CacheAccessPlugin     → Stage 3 (Main cache access)
ExecutePlugin         → Stage 4 (ALU + FPU parallel)
WritebackPlugin       → Stage 5 (Result writeback)
```

This gives us the authentic T9000 five-stage pipeline with proper instruction grouping and parallel execution units.
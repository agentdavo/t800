# T9000 Pipeline Redesign

## Current Issue
The ALU operations are currently executed directly in the SecondaryInstrPlugin decode stage, which doesn't match the T9000's actual 5-stage pipeline architecture.

## Proper T9000 Pipeline Architecture

### Stage 1: Fetch
- Fetch 1 word (32 bits) per cycle from memory
- Fill instruction buffer
- InstrFetchPlugin handles this

### Stage 2: Decode/Group
- Process up to 8 bytes per cycle from instruction buffer
- Extract variable-length instructions (1-4 bytes each)
- Group compatible instructions for parallel execution
- InstrGrouperPlugin performs hardware optimization
- Read 2 operands from workspace cache (local variables)
- Calculate effective addresses for non-local operations

### Stage 3: Address/Cache
- Perform 2 non-local memory reads from main cache
- Address calculation results from stage 2 used here

### Stage 4: Execute (ALU/FPU)
- **ALU operations happen here** (not in decode!)
- FPU operations happen in parallel
- Using SpinalHDL's NodeLaneApi or CtrlLaneApi for parallel execution units

### Stage 5: Write
- Write results back to registers/memory
- 1 write per cycle

## Proposed Implementation

### 1. Define ALU Command Payload
```scala
// In Global.scala
case class AluCmd() extends Bundle {
  val op = AluOp()  // Enum of ALU operations
  val srcA = Bits(32 bits)
  val srcB = Bits(32 bits)
  val srcC = Bits(32 bits)  // For 3-operand instructions
}

object AluOp extends SpinalEnum {
  val ADD, SUB, AND, OR, XOR, SHL, SHR, MUL, DIV = newElement()
}

// Pipeline payloads
def ALU_CMD: Payload[AluCmd] = Payload(AluCmd())
def ALU_RESULT: Payload[Bits] = Payload(Bits(32 bits))
```

### 2. SecondaryInstrPlugin Changes
Instead of executing operations, it should:
```scala
// In decode stage
when(secondary === ADD) {
  // Read operands from register stack
  val srcA = regStack.readReg(RegName.Areg)
  val srcB = regStack.readReg(RegName.Breg)
  
  // Pass command down pipeline
  execute(ALU_CMD).op := AluOp.ADD
  execute(ALU_CMD).srcA := srcA
  execute(ALU_CMD).srcB := srcB
}
```

### 3. New AluPlugin for Stage 4
```scala
class AluPlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[PipelineStageService]
    
    // ALU execution in stage 4
    val aluStage = pipe.execute
    
    when(aluStage.isValid) {
      val cmd = aluStage(ALU_CMD)
      val result = Bits(32 bits)
      
      switch(cmd.op) {
        is(AluOp.ADD) { result := cmd.srcA.asUInt + cmd.srcB.asUInt }
        is(AluOp.SUB) { result := cmd.srcA.asUInt - cmd.srcB.asUInt }
        is(AluOp.AND) { result := cmd.srcA & cmd.srcB }
        // ... other operations
      }
      
      // Pass result to writeback stage
      pipe.memory(ALU_RESULT) := result
    }
  }
}
```

### 4. Parallel ALU/FPU Using NodeLaneApi
```scala
// In stage 4, ALU and FPU can run in parallel
val aluLane = new CtrlLane {
  // ALU operations
}

val fpuLane = new CtrlLane {
  // FPU operations  
}

// Both lanes execute in parallel in stage 4
```

### 5. WritebackPlugin for Stage 5
```scala
class WritebackPlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[PipelineStageService]
    val regStack = Plugin[RegStackService]
    
    // Writeback in stage 5
    val wbStage = pipe.writeBack
    
    when(wbStage.isValid && wbStage(HAS_ALU_RESULT)) {
      regStack.writeReg(RegName.Areg, wbStage(ALU_RESULT))
    }
  }
}
```

## Benefits
1. **Correct pipeline behavior**: Operations happen in the right stages
2. **Better performance**: True pipelining with proper hazard handling
3. **Parallel execution**: ALU and FPU can run simultaneously
4. **Cleaner architecture**: Each plugin has a single responsibility
5. **Easier to extend**: New execution units can be added as plugins

## Implementation Steps
1. Add pipeline payload definitions to Global.scala
2. Create AluPlugin for stage 4 execution
3. Refactor SecondaryInstrPlugin to just decode and pass commands
4. Create WritebackPlugin for stage 5
5. Update PipelineBuilderPlugin to connect the new plugins
6. Test with simple ALU operations
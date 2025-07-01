# T9000 Pipeline Using SpinalHDL Pipeline API

## Clean Implementation with Automatic Register Management

### Pipeline Definition
```scala
class T9000Pipeline extends FiberPlugin {
  // SpinalHDL automatically manages all registers and timing!
  val pipeline = new StageCtrlPipeline()
  
  // Define 5 stages
  val s1_fetchGroup = pipeline.ctrl(0)
  val s2_localDecode = pipeline.ctrl(1) 
  val s3_addressCache = pipeline.ctrl(2)
  val s4_execute = pipeline.ctrl(3)
  val s5_writeback = pipeline.ctrl(4)
  
  during build new Area {
    pipeline.build() // Automatically connects stages with registers!
  }
}
```

### Stage 1: Fetch/Group
```scala
class FetchGroupPlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline]
    val s1 = pipe.s1_fetchGroup
    
    // Fetch logic
    val fetchBuffer = Vec(Reg(Bits(32 bits)), 4)
    val grouper = new InstructionGrouper()
    
    // Output flows to next stage automatically registered
    s1(INSTRUCTION_GROUP) := grouper.output
    s1(PC_UPDATE) := grouper.pcAdvance
  }
}
```

### Stage 2: Local/Decode  
```scala
class LocalDecodePlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline]
    val s2 = pipe.s2_localDecode
    
    // Input automatically comes from s1 (registered)
    val instrGroup = s2(INSTRUCTION_GROUP)
    
    // Workspace cache (triple-ported)
    val workspaceCache = new WorkspaceCache()
    val dataA = workspaceCache.readA(s2(LOCAL_ADDR_A))
    val dataB = workspaceCache.readB(s2(LOCAL_ADDR_B))
    
    // Decode logic
    val decoder = new InstructionDecoder()
    val decodedOp = decoder.decode(instrGroup, dataA, dataB)
    
    // Output to next stage (automatically registered)
    s2(DECODED_OP) := decodedOp
    s2(LOCAL_DATA_A) := dataA
    s2(LOCAL_DATA_B) := dataB
  }
}
```

### Stage 3: Address/Cache
```scala  
class AddressCachePlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline]
    val s3 = pipe.s3_addressCache
    
    // Input automatically registered from s2
    val decodedOp = s3(DECODED_OP)
    
    // Main cache (4-bank, dual-ported)
    val mainCache = new MainCache()
    val nonLocalA = mainCache.readA(decodedOp.addressA)
    val nonLocalB = mainCache.readB(decodedOp.addressB)
    
    // Output to execute stage (automatically registered)
    s3(NON_LOCAL_A) := nonLocalA
    s3(NON_LOCAL_B) := nonLocalB
    s3(EXECUTE_CMD) := decodedOp.executeCmd
  }
}
```

### Stage 4: Execute (Parallel ALU/FPU)
```scala
class ExecutePlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline] 
    val s4 = pipe.s4_execute
    
    // Input automatically registered from s3
    val executeCmd = s4(EXECUTE_CMD)
    val dataA = s4(NON_LOCAL_A)
    val dataB = s4(NON_LOCAL_B)
    
    // Parallel execution lanes using multi-lane API!
    val aluLane = 0
    val fpuLane = 1
    
    // ALU lane
    when(executeCmd.hasAluOp) {
      val aluResult = performAlu(executeCmd.aluOp, dataA, dataB)
      s4(RESULT, aluLane) := aluResult
    }
    
    // FPU lane (runs in parallel!)
    when(executeCmd.hasFpuOp) {
      val fpuResult = performFpu(executeCmd.fpuOp, dataA, dataB)  
      s4(RESULT, fpuLane) := fpuResult
    }
    
    // Pipeline control
    s4.haltWhen(executeCmd.isComplexOp && !complexOpDone)
  }
}
```

### Stage 5: Writeback
```scala
class WritebackPlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline]
    val s5 = pipe.s5_writeback
    val regStack = Plugin[RegStackService]
    
    // Input automatically registered from s4
    val result = s5(RESULT)
    val writeCmd = s5(WRITE_CMD)
    
    // Register writeback
    when(s5.isValid && writeCmd.enable) {
      regStack.writeReg(writeCmd.reg, result)
    }
    
    // Memory writeback  
    when(s5.isValid && writeCmd.memWrite) {
      memoryBus.write(writeCmd.addr, result)
    }
  }
}
```

## Pipeline Hazard Handling

### Automatic Bypass/Forwarding
```scala
// SpinalHDL handles this automatically!
class HazardPlugin extends FiberPlugin {
  during build new Area {
    val pipe = Plugin[T9000Pipeline]
    
    // Automatic forwarding from execute to decode
    val forwardedValue = pipe.s2_localDecode.bypass(OPERAND_A)
    when(hazardDetected) {
      forwardedValue := pipe.s4_execute(RESULT)
    }
  }
}
```

### Stall Insertion
```scala
// Stalls are inserted automatically with proper backpressure
pipe.s3_addressCache.haltWhen(cachemiss)
pipe.s4_execute.haltWhen(fpuBusy)
```

## Benefits of SpinalHDL Pipeline API

### 1. **Automatic Register Management**
- No manual register placement
- Perfect timing by construction
- Eliminates critical path issues

### 2. **Built-in Backpressure**
- Automatic stall propagation
- Ready/valid semantics
- No lost transactions

### 3. **Hazard Detection**
- Automatic bypass insertion
- Multi-cycle operation support
- Clean cancellation handling

### 4. **Multi-Lane Support**
- Perfect for parallel ALU/FPU
- Lane-based resource allocation
- Automatic lane arbitration

### 5. **Debug and Verification**
- Named pipeline stages
- Automatic signal naming
- Built-in assertions

## Implementation Strategy

1. **Create T9000Pipeline class** with 5 stages using `StageCtrlPipeline`
2. **One plugin per stage** - clean separation of concerns
3. **Use Payload[T] for all inter-stage data** - automatic propagation
4. **Leverage multi-lane API** for parallel ALU/FPU execution
5. **Use pipeline control methods** for stalls and hazards

This gives us a clean, high-performance T9000 implementation that automatically handles all the timing complexity while maintaining the authentic 5-stage pipeline architecture!
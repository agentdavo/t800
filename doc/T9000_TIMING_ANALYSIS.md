# T9000 Pipeline Timing Analysis

## Critical Path Considerations for High MHz

### Current Issues in Implementation

**Long Combinatorial Paths:**
1. **Fetch → Group → Decode** all in one cycle
2. **ALU operations** directly in decode stage  
3. **Register stack access** with immediate ALU computation
4. **Cache lookup and data return** in same cycle

### Proper Pipeline Register Placement

## Stage 1: Fetch/Group
```scala
// Clock 1: Fetch request
fetchBus.cmd.valid := True
fetchBus.cmd.address := pcReg

// Clock 2: Store fetch response (REGISTERED)
val fetchReg = RegNext(fetchBus.rsp.data)
val fetchValid = RegNext(fetchBus.rsp.valid)

// Clock 3: Group logic (REGISTERED OUTPUT)
val groupResult = Reg(InstructionGroup())
when(fetchValid) {
  groupResult := performGrouping(fetchReg) // Combinatorial
}
```

**Critical Path**: Memory access time + grouping logic
**Target**: < 2ns @ 500MHz (T9000 could run at 50MHz, modern target 500MHz)

## Stage 2: Local/Decode  
```scala
// Input: Registered group result
val instrGroup = pipe.decode(INSTRUCTION_GROUP)

// Workspace cache access (DUAL PORT SRAM)
val workspaceA = workspaceCache.readPort(addrA) // < 1ns
val workspaceB = workspaceCache.readPort(addrB) // < 1ns

// Decode logic (REGISTERED OUTPUT)
val decodeResult = Reg(DecodedOp())
decodeResult := performDecode(instrGroup, workspaceA, workspaceB)
```

**Critical Path**: SRAM access + decode logic
**Target**: < 2ns

## Stage 3: Address/Cache
```scala
// Input: Registered decode result  
val decodedOp = pipe.execute(DECODED_OP)

// Main cache access (4-BANK SRAM)
val cacheData = mainCache.read(decodedOp.memAddr) // < 1.5ns

// Address calculation (REGISTERED OUTPUT)
val memResult = Reg(MemoryOp())
memResult := performMemOp(decodedOp, cacheData)
```

**Critical Path**: Cache SRAM access + address arithmetic
**Target**: < 2ns

## Stage 4: Execute (ALU/FPU)
```scala
// Input: Registered memory result
val memOp = pipe.execute(MEMORY_OP)

// ALU lane (FAST COMBINATORIAL)
val aluResult = Reg(Bits(32 bits))
aluResult := performAlu(memOp.aluCmd) // < 1ns for simple ops

// FPU lane (PIPELINED INTERNALLY)  
val fpuResult = Reg(Bits(64 bits))
fpuResult := fpuPipeline.result // FPU has internal pipeline
```

**Critical Path**: ALU combinatorial logic
**Target**: < 2ns (simple ALU), FPU internally pipelined

## Stage 5: Writeback
```scala
// Input: Registered execute results
val execResult = pipe.writeBack(EXECUTE_RESULT)

// Register write (SINGLE CYCLE)
when(execResult.writeEnable) {
  regStack.write(execResult.writeReg, execResult.data) // < 0.5ns
}
```

**Critical Path**: Register file write
**Target**: < 2ns

## Timing Optimizations

### 1. Register Placement
```scala
// BAD: Long combinatorial path
val result = performAlu(performDecode(fetchData))

// GOOD: Registered intermediate
val decoded = RegNext(performDecode(fetchData))
val result = RegNext(performAlu(decoded))
```

### 2. Memory Access Pipelining
```scala
// BAD: Cache access + use in same cycle
val data = cache.read(addr)
val result = alu.compute(data)

// GOOD: Separate cycles
val data = RegNext(cache.read(addr))
val result = RegNext(alu.compute(data))
```

### 3. ALU Operation Splitting
```scala
// BAD: Complex ALU in one cycle
switch(opcode) {
  is(MUL) { result := a * b } // 2-3ns multiplier
  is(DIV) { result := a / b } // 10ns+ divider
}

// GOOD: Multi-cycle for complex ops
val mulPipeline = new Area {
  val stage1 = RegNext(a * b) // Partial products
  val stage2 = RegNext(stage1) // Sum partial products
}
```

### 4. Workspace Cache Optimization
```scala
// Use true dual-port SRAM for 2 reads/cycle
val workspaceCache = Mem(Bits(32 bits), 32)
val portA = workspaceCache.readSyncPort() // < 1ns
val portB = workspaceCache.readSyncPort() // < 1ns
val portC = workspaceCache.writeSyncPort()
```

## High-Frequency Design Rules

### 1. One Major Operation Per Stage
- Stage 1: Memory fetch OR instruction grouping
- Stage 2: Decode OR workspace access  
- Stage 3: Cache access OR address calculation
- Stage 4: ALU OR FPU operation
- Stage 5: Register write

### 2. Pipeline Complex Operations
```scala
// Multi-cycle multiply
class MultiplierPipeline extends Area {
  val s1_partialProducts = RegNext(computePartialProducts(a, b))
  val s2_sum = RegNext(sumPartialProducts(s1_partialProducts))
  val s3_result = RegNext(s2_sum)
}
```

### 3. Minimize Mux Delays
```scala
// BAD: Deep mux tree
val result = switch(opcode) {
  is(ADD) { a + b }
  is(SUB) { a - b }
  // ... 20 more cases
}

// GOOD: Decode to control signals
val doAdd = opcode === ADD
val doSub = opcode === SUB
val result = Mux(doAdd, a + b, Mux(doSub, a - b, default))
```

## Target Frequencies

- **T9000 Original**: 50 MHz (20ns cycle)
- **Modern FPGA**: 500 MHz (2ns cycle)  
- **ASIC Target**: 1 GHz (1ns cycle)

Each pipeline stage must complete in < 2ns for 500MHz operation.

## SpinalHDL Timing Directives

```scala
// Add pipeline registers automatically
val pipeline = StageCtrlPipeline()
pipeline.ctrl(0) // Auto-registered
pipeline.ctrl(1) // Auto-registered

// Manual timing control
val criticalPath = RegNext(combinatorialLogic()) 
criticalPath.addTag(ClockDomainTag(ClockDomain.current))
```

This ensures each stage is properly isolated for high-frequency operation.
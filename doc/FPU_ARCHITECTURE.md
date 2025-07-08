# T9000 FPU Architecture

## Entry Point and Component Hierarchy

### FpuPlugin - The Main Entry Point

Yes, `FpuPlugin` is the main entry point and top-level component for the T9000 FPU. It serves as:

1. **FiberPlugin Integration** - Connects to the T9000 plugin system
2. **Service Provider** - Implements the FpuService interface
3. **Component Orchestrator** - Instantiates and connects all FPU subcomponents
4. **Pipeline Interface** - Manages CPU pipeline stalls and shadow registers

### Component Architecture

```
FpuPlugin (Main Entry Point)
├── Register File
│   ├── FPA, FPB, FPC (64-bit FP registers)
│   ├── FPStatus (32-bit status register)
│   └── Shadow copies for interrupts
│
├── FpuInstructionDecoder
│   └── Decodes 48 T9000 FPU instructions
│
├── FpuDispatcher
│   └── Routes instructions to execution units
│
└── Execution Units
    ├── FpuAdder (Add/Sub/Compare)
    ├── FpuMultiplier (Booth-3)
    ├── FpuDivider (Newton-Raphson)
    ├── FpuLoadStore (Memory operations)
    ├── FpuComparison (IEEE comparisons)
    ├── FpuConversion (Type conversions)
    ├── FpuControl (Status/Rounding)
    └── FpuStack (Stack operations)
```

### Other Core Files

**FpuCore.scala**
- Demonstrates AFix usage for FPU operations
- Not the main plugin - more of a reference implementation
- Shows how AFix simplifies IEEE 754 arithmetic

**Service.scala**
- Defines FPU service interfaces:
  - `FpuService` - Main FPU interface
  - `FpuOpsService` - Operation dispatch
  - `FpuControlService` - Control/status

**Opcodes.scala**
- Complete T9000 FPU instruction enumeration
- Maps opcodes to operations
- Defines command/response bundles

**Utils.scala**
- IEEE 754 utility functions
- Format parsing/packing
- Rounding implementations
- Exception handling

### Integration with T9000 Pipeline

The FPU integrates with the T9000 5-stage pipeline:

1. **Fetch** - Instructions fetched normally
2. **Decode** - FPU opcodes detected (0x2A/0x2B prefix)
3. **Execute** - FPU operations dispatched here
4. **Memory** - Multi-cycle FPU ops may stall
5. **Writeback** - Results written to FP registers

### Configuration

In `T9000Param.scala`:
```scala
// FPU is configured as:
if (enableFpu) {
  plugins += new transputer.plugins.fpu.FpuPlugin()
}
```

### Key Design Decisions

1. **AFix Usage** - Leverages SpinalHDL's AFix for automatic precision
2. **Modular Units** - Each execution unit is self-contained
3. **Service Architecture** - Clean interfaces between components
4. **Pipeline Integration** - Proper stall handling for multi-cycle ops
5. **Shadow Registers** - Hardware interrupt support

The architecture provides a complete IEEE 754 compliant FPU with all 48 T9000 instructions while maintaining clean separation of concerns and efficient hardware implementation.
# T9000 Transputer Developer Guide

This guide covers development, building, testing, and extending the T9000 Transputer implementation.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Build System](#build-system)
3. [Testing Infrastructure](#testing-infrastructure)
4. [Plugin Development](#plugin-development)
5. [Assembly Programming](#assembly-programming)
6. [Pipeline Visualization](#pipeline-visualization)
7. [Troubleshooting](#troubleshooting)

## Quick Start

### Prerequisites

- JDK 8+ (tested with JDK 17)
- SBT 1.10.0+
- Verilator 5.0+ (optional, for simulation)
- GTKWave (optional, for waveforms)
- macOS or Linux

### Basic Commands

```bash
# Clone and setup
git clone <repository>
cd t800

# Format code (always run before commits)
sbt scalafmtAll

# Run tests
sbt test

# Generate Verilog
sbt "runMain transputer.Generate"              # Full T9000
sbt "runMain transputer.Generate --minimal"    # Bare bones version

# With options
sbt "runMain transputer.Generate --enable-fpu true --link-count 4"
```

## Build System

### Project Structure

```
t800/
├── src/main/scala/transputer/
│   ├── T9000Transputer.scala      # Main component
│   ├── T9000Param.scala           # Configuration
│   ├── Generate.scala             # Verilog generator
│   ├── TransputerAssembler.scala  # Assembly → HEX
│   ├── GenerateWithTest.scala     # Test generator
│   └── plugins/                   # Instruction implementations
│       ├── arithmetic/            # Table 6.9
│       ├── longarith/            # Table 6.10
│       ├── controlflow/          # Table 6.11
│       └── ...                   # Tables 6.12-6.37
├── src/test/scala/transputer/     # Test suites
├── scripts/
│   ├── asm/                      # Assembly sources
│   └── hex/                      # Assembled programs
└── generated/                    # Output Verilog
```

### Configuration Options

**T9000Param** - Full configuration:
```scala
T9000Param(
  wordWidth = 32,          // Word size in bits
  linkCount = 4,           // Number of DS-Links
  enableFpu = true,        // Include FPU
  mainCacheKb = 16,        // Main cache size
  wsCacheWords = 32,       // Workspace cache size
  enableMmu = true,        // Memory management
  enableVcp = true,        // Virtual channels
  enablePmi = false        // External memory
)
```

**Param** - Minimal configuration:
```scala
Param(
  wordWidth = 32,
  linkCount = 2,
  enableFpu = false,
  cacheSize = 4096
)
```

### Build Scripts

```bash
# Comprehensive build and test
./scripts/build_t9000_system.sh

# Quick validation
./scripts/validate_t9000_system.sh

# Full test suite
./scripts/build_t9000_tests.sh

# Pipeline validation
./scripts/validate_t9000_pipeline.sh
```

## Testing Infrastructure

### Overview

The T9000 uses a three-tier testing approach:

1. **Unit Tests** - Individual plugin verification
2. **Integration Tests** - Multi-plugin interaction
3. **System Tests** - Full processor validation

### Test Workflow

#### 1. Write Assembly Test

```asm
; File: scripts/asm/test_add.asm
Start:
    mint            ; Initialize
    sthf
    mint
    stlf
    
    ldc     10      ; Test addition
    ldc     20
    add
    
    eqc     30      ; Verify result
    cj      Fail
    
Pass:
    j       Pass    ; Success loop
    
Fail:
    j       Fail    ; Failure loop
```

#### 2. Assemble to HEX

```bash
sbt "runMain transputer.TransputerAssembler scripts/asm/test_add.asm"
# Output: scripts/hex/test_add.hex
```

#### 3. Run Test

```bash
# Generate RTL only
sbt "runMain transputer.GenerateWithTest"

# With simulation (requires Verilator)
sbt "runMain transputer.GenerateWithTest --hex scripts/hex/test_add.hex --wave"

# With pipeline visualization
sbt "runMain transputer.GenerateWithTest --hex scripts/hex/test_add.hex --konata"
```

### Test Components

**TransputerAssembler**
- Converts assembly to Intel HEX
- Supports full T9000 ISA
- VAL constants and expressions
- Label resolution

**GenerateWithTest**
- Extends basic generator
- Options:
  - `--hex <file>` - Load program
  - `--wave` - Enable waveforms
  - `--konata` - Pipeline trace
  - `--pass/--fail <addr>` - Test endpoints

**KonataBackend**
- Pipeline visualization
- Instruction flow tracking
- Dependency analysis
- Stall/flush visualization

### Writing Tests

```scala
class MyPluginTest extends AnyFunSuite {
  test("Basic operation") {
    SimConfig.withWave.compile {
      new Component {
        val dut = T9000Transputer(T9000Param())
        // Test setup
      }
    }.doSim { dut =>
      // Test execution
      dut.clockDomain.forkStimulus(10)
      // Assertions
    }
  }
}
```

## Plugin Development

### Plugin Architecture

Each T9000 instruction table (6.9-6.37) maps to a plugin:

```scala
class MyInstructionPlugin extends FiberPlugin {
  // Plugin identification
  override def getDisplayName() = "MyInstructionPlugin"
  setName("myinstruction")
  
  // Setup phase - register services
  during setup new Area {
    addService(new MyInstructionService {
      def isMyOp(opcode: Bits): Bool = ???
      def execute(operands: Bundle): UInt = ???
    })
  }
  
  // Build phase - generate hardware
  during build new Area {
    val pipeline = host[PipelineStageService]
    val regfile = host[RegfileService]
    
    // Implement instruction logic
    val stage = pipeline.execute
    when(isMyInstruction(stage(Global.OPCODE))) {
      // Execute operation
    }
  }
}
```

### Service Pattern

```scala
// Define service interface
trait MyInstructionService {
  def isMyOp(opcode: Bits): Bool
  def execute(a: UInt, b: UInt): UInt
  def getLatency(op: OpType): Int
}

// Register in plugin
addService(new MyInstructionService { ... })

// Use from other plugins
val service = host[MyInstructionService]
val result = service.execute(a, b)
```

### Adding Instructions

1. **Create Plugin Directory**
   ```
   src/main/scala/transputer/plugins/myfeature/
   ├── Service.scala
   └── MyFeaturePlugin.scala
   ```

2. **Define Service Interface**
   ```scala
   trait MyFeatureService {
     def operation(input: UInt): UInt
   }
   ```

3. **Implement Plugin**
   ```scala
   class MyFeaturePlugin extends FiberPlugin {
     // Implementation
   }
   ```

4. **Add to T9000Param**
   ```scala
   plugins += new MyFeaturePlugin()
   ```

### Pipeline Integration

Instructions execute in specific pipeline stages:

- **Fetch**: Instruction fetch only
- **Decode**: Register reads, immediate decode
- **Address**: Memory address calculation
- **Execute**: ALU/FPU operations
- **Writeback**: Register updates

## Assembly Programming

### Instruction Reference

**Primary Instructions**
```asm
ldc  10      ; Load constant
ldl  0       ; Load from workspace[0]
stl  1       ; Store to workspace[1]
add          ; Add Areg + Breg → Areg
call label   ; Function call
j    label   ; Unconditional jump
cj   label   ; Jump if Areg = 0
ajw  16      ; Adjust workspace pointer
```

**Stack Operations**
```asm
dup          ; Duplicate Areg
rev          ; Swap Areg ↔ Breg
pop          ; Discard Areg
```

**Process Management**
```asm
startp       ; Start process
endp         ; End process
runp         ; Run process
stopp        ; Stop process
```

### Program Structure

```asm
; Constants
VAL STACK_SIZE IS 32
VAL TIMER_VAL IS 1000

; Entry point
Start:
    ; Initialize processor
    mint            ; NotProcess.p
    sthf            ; High priority queue
    mint
    stlf            ; Low priority queue
    
    ; Initialize timer
    ldc     0
    sttimer
    
    ; Main program
    ajw     STACK_SIZE
    call    Main
    terminate

Main:
    ; Your code here
    ret

; Data section
Data:
    db      "Hello", 0x00
```

### Common Patterns

**Function Call**
```asm
    ldc     param1
    ldc     param2
    call    Function
    ; Result in Areg

Function:
    ; Parameters in Areg, Breg
    ; Local workspace available
    ret
```

**Loop**
```asm
    ldc     10          ; Counter
Loop:
    dup                 ; Preserve counter
    ; Loop body
    adc     -1          ; Decrement
    dup
    cj      LoopEnd     ; Exit if zero
    j       Loop
LoopEnd:
    pop                 ; Clean stack
```

## Pipeline Visualization

### Konata Integration

The T9000 supports pipeline visualization using Konata:

1. **Generate with Konata**
   ```bash
   sbt "runMain transputer.GenerateWithTest --hex program.hex --konata"
   ```

2. **View Pipeline**
   - Download [Konata](https://github.com/shioyadan/Konata)
   - Open `simWorkspace/konata.log`

3. **Understanding the View**
   - Horizontal: Time (cycles)
   - Vertical: Instructions
   - Colors: Pipeline stages (F→D→A→X→W)
   - Arrows: Dependencies

### Pipeline Events

- **Normal Flow**: Instructions progress through stages
- **Stalls**: Cache misses, hazards (shown as overlays)
- **Flushes**: Branch mispredictions (red indicators)
- **Dependencies**: Data hazards (arrows between instructions)

## Troubleshooting

### Common Issues

**Compilation Errors**
```bash
# Missing imports
sbt clean compile

# Type errors
sbt "show compile:scalacOptions"
```

**Verilator Issues (macOS)**
```bash
# C++ flag errors - use RTL generation only
sbt "runMain transputer.Generate"
```

**Assembly Errors**
- Check label definitions before use
- Verify VAL constants defined first
- Use 0x prefix for hex numbers
- Ensure proper initialization sequence

**Test Failures**
```bash
# Run specific test
sbt "testOnly transputer.T9000StackSpec"

# With more output
sbt "testOnly transputer.T9000StackSpec -- -oF"
```

### Debug Techniques

**Waveform Analysis**
```bash
# Generate with waves
sbt "runMain transputer.GenerateWithTest --hex test.hex --wave"

# View
gtkwave simWorkspace/wave.fst
```

**Pipeline Debugging**
- Use Konata to visualize instruction flow
- Check for stalls and flushes
- Verify dependency resolution

**Print Debugging**
```scala
// In plugin
println(s"[${this.getDisplayName()}] Debug info")

// In simulation
when(condition) {
  printf("Cycle %d: value=%x\n", cycleCount, signal)
}
```

### Performance Optimization

**Critical Path Analysis**
- Check timing reports in synthesis
- Identify long combinational paths
- Add pipeline registers if needed

**Memory Access**
- Minimize cache misses
- Use workspace for locals
- Align data properly

**Pipeline Efficiency**
- Reduce branch penalties
- Minimize dependencies
- Use instruction grouping

## Advanced Topics

### Multi-Core Configuration

```scala
// Create multiple T9000 cores
val cores = List.tabulate(4) { id =>
  T9000Transputer(T9000Param(hartId = id))
}

// Connect via DS-Links
cores.sliding(2).foreach { case List(a, b) =>
  a.io.links(0) <> b.io.links(1)
}
```

### Custom Instructions

1. Define opcode in unused space
2. Create plugin following pattern
3. Add to instruction decoder
4. Update assembler

### FPGA Deployment

```bash
# Synthesis for ECP5
sbt synth

# Timing report
sbt report

# Bitstream generation
nextpnr-ecp5 --json generated/T9000.json
```

## Resources

- [T9000 Technical Reference](./T9000_TECHNICAL_REFERENCE.md)
- [SpinalHDL Documentation](https://spinalhdl.github.io/SpinalDoc-RTD/)
- [Original T9000 Manual](./text/transputer_t9000_manual.txt)
- [Konata Viewer](https://github.com/shioyadan/Konata)
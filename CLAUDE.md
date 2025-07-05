# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **T9000 Transputer** implementation in **SpinalHDL** using advanced FiberPlugin architecture. The project implements a complete 32-bit Transputer processor with modern hardware description practices while maintaining compatibility with the original T9000 instruction set architecture.

### Key Architectural Concepts

- **FiberPlugin System**: Modular, hot-swappable hardware components
- **Service-Oriented Design**: Plugin communication via well-defined interfaces  
- **Pipeline DSL**: 5-stage pipeline using SpinalHDL's advanced Pipeline DSL
- **Database Configuration**: Typed configuration sharing across plugins
- **BMB Bus System**: 128-bit high-performance memory bus

## Build System & Commands

### Core Development Commands
```bash
# Essential development workflow
sbt scalafmtAll                         # Format code (run before commits)
sbt test                                # Run full test suite

# Verilog generation
sbt "runMain transputer.Generate"                    # Full T9000 with all features (T9000Transputer.v)
sbt "runMain transputer.Generate --minimal"          # Bare bones minimal config (Transputer.v)
sbt "runMain transputer.Generate --hex hello.hex"    # T9000 with boot ROM

# T9000 with custom options
sbt "runMain transputer.Generate --word-width 32 --link-count 4 --enable-fpu true"

# FPGA generation (NEW: Unified with Generate.scala)
sbt "runMain transputer.Generate --fpga=ecp5 --demo"                        # Demo FPGA for testing
sbt "runMain transputer.Generate --fpga=ecp5 --hex scripts/hex/hello.hex"   # FPGA with boot ROM
sbt "runMain transputer.Generate --fpga=ice40 --demo"                       # Target ICE40 FPGA
sbt "runMain transputer.Generate --fpga=xilinx --hex scripts/hex/boot.hex"  # Target Xilinx FPGA

# Main development scripts (from project root)
./scripts/build.sh                      # Build T9000 Verilog (various configs)
./scripts/test.sh                       # Run test suites with reports
./scripts/assemble.sh                   # Assemble transputer programs
./scripts/utils.sh                      # Development utilities

# Script examples
./scripts/build.sh --config all         # Build all configurations
./scripts/build.sh --config minimal     # Build minimal transputer
./scripts/test.sh --type full           # Run comprehensive test suite
./scripts/test.sh --type quick          # Quick validation tests
./scripts/assemble.sh scripts/asm/hello_world.asm  # Assemble to hex
./scripts/utils.sh konata --demo        # Generate demo Konata visualization
./scripts/utils.sh clean                # Clean all build artifacts

# Specific test execution
sbt "testOnly transputer.T9000*"         # Run T9000-specific tests
sbt "testOnly transputer.T9000StackSpec" # Stack operations test
sbt bareBonesTest                        # Minimal config tests

# FPGA synthesis (ECP5 using open-source toolchain)
cd fpga && make all                     # Complete FPGA flow: Verilog → synthesis → place & route → bitstream
cd fpga && make test                    # Test Verilog generation only
cd fpga && make burn                    # Program FPGA (if connected)
cd fpga && make stats                   # Show synthesis statistics

# Simulation
sbt "test:runMain transputer.T9000CoreSim"      # Interactive simulation
sbt "test:runMain transputer.BootRomFetchSim"   # Boot ROM simulation
```

### Build Configuration Details
- **Dual build modes**: Source-based (SpinalHDL submodule) vs Published JAR dependencies
- **Generator variants**: `T9000Generate` (full) vs `Generate` (minimal/bare bones)
- **Fork JVM**: Enabled for memory management during compilation
- **Test isolation**: Separate test suites per configuration

### Build Output Locations
```bash
generated/                              # Verilog output files
├── T9000Transputer.v                   # Full T9000 system
├── Transputer.v                        # Bare bones version
└── T9000BootRomDesign.v               # Boot ROM design

scripts/test_reports/                   # Test report outputs
├── master_test_report.txt              # Comprehensive test summary
├── T9000_Pipeline_Validation_Report.md # Pipeline validation report
└── [component]_report.txt              # Individual test reports

hello_world_rom.hex                     # Boot ROM hex file (project root)
```

### Environment Variables
```bash
SPINALHDL_FROM_SOURCE=1                 # Use git submodule instead of published jars
SPINALSIM_WORKSPACE=/tmp                # Redirect simulation workspace
```

## Architecture Deep Dive

### FiberPlugin Architecture

The entire T9000 is constructed using SpinalHDL's **FiberPlugin** system:

**Plugin Lifecycle:**
1. **Setup Phase**: Plugins register services and declare dependencies
2. **Build Phase**: Hardware generation with fiber-based scheduling  
3. **Database Integration**: Typed configuration through global `Database` context

**Core Plugin Categories:**
- **Infrastructure**: `TransputerPlugin`, `PipelinePlugin`, `PipelineBuilderPlugin`
- **Execution**: `FetchPlugin`, `StackPlugin`, `FpuPlugin`, `ArithmeticPlugin`
- **Memory**: `MainCachePlugin`, `WorkspaceCachePlugin`, `MemoryManagementPlugin`
- **System Services**: `SchedulerPlugin`, `TimerPlugin`, `VcpPlugin`, `PmiPlugin`
- **Communication**: DS-Link support, channel operations

### Service Architecture

Plugins communicate exclusively through **service interfaces**:

```scala
// Service definition pattern
trait MyService {
  def method(): UInt
  def signal: Bool
}

// Service registration in plugin
addService(new MyService {
  override def method(): UInt = implementation
  override def signal: Bool = signalImpl
})

// Service consumption from other plugins
val service = Plugin[MyService]
val result = service.method()
```

### Pipeline Integration

5-stage CPU pipeline: **Fetch → Decode → Execute → Memory → Writeback**

**Pipeline Construction:**
- `PipelinePlugin` creates stage structure
- `PipelineBuilderPlugin` connects stages
- Automatic stall/flush handling
- Global payload propagation

## Instruction Set Architecture

### Transputer ISA Implementation

**Primary Instructions (4-bit opcode + 4-bit operand):**
```
0x0: j      # Direct jump
0x1: ldlp   # Load local pointer  
0x2: pfix   # Prefix positive
0x3: ldnl   # Load non-local
0x4: ldc    # Load constant
0x5: ldnlp  # Load non-local pointer
0x6: nfix   # Prefix negative  
0x7: ldl    # Load local
0x8: adc    # Add constant
0x9: call   # Function call
0xA: cj     # Conditional jump  
0xB: ajw    # Adjust workspace
0xC: eqc    # Equal constant
0xD: stl    # Store local
0xE: stnl   # Store non-local
0xF: opr    # Operate (secondary instructions)
```

### Register Architecture

**Three-Register Evaluation Stack:**
```
Areg (R_AREG=0)    # Top of stack
Breg (R_BREG=1)    # Second value  
Creg (R_CREG=2)    # Third value
```

**System Registers:**
```
Wreg            # Workspace pointer
IptrReg         # Instruction pointer
WdescReg        # Workspace descriptor
StatusReg       # Process status
```

## Current Implementation Status

🎉 **COMPLETE: Full T9000 Instruction Set Architecture Implementation**

### ✅ All 21 Instruction Table Plugins Implemented
- **ArithmeticPlugin** (Table 6.9) - Basic ALU operations
- **LongArithPlugin** (Table 6.10) - 64-bit arithmetic
- **ControlFlowPlugin** (Table 6.11) - Jump/call instructions  
- **BlockMovePlugin** (Table 6.12) - Block operations
- **IndexingPlugin** (Table 6.13) - Memory indexing
- **RangeCheckPlugin** (Table 6.14) - Bounds checking
- **DevicePlugin** (Table 6.15) - Device access
- **BitOpsPlugin** (Table 6.16) - CRC and bit manipulation
- **GeneralPlugin** (Table 6.17) - Stack operations
- **TimerPlugin** (Table 6.18) - Timer operations
- **IOPlugin** (Tables 6.19-20) - Input/output
- **ChannelPlugin** (Table 6.21) - Channel communication
- **ResourcePlugin** (Table 6.22) - Resource management
- **SemaphorePlugin** (Table 6.23) - Synchronization
- **AlternativePlugin** (Table 6.24) - ALT constructs
- **SchedulePlugin** (Tables 6.25-26) - Process scheduling
- **InterruptPlugin** (Table 6.27) - Interrupt handling
- **ProtectionPlugin** (Table 6.28) - Memory protection
- **SystemPlugin** (Tables 6.29-30) - System configuration
- **CachePlugin** (Table 6.31) - Cache management
- **FpuPlugin** (Tables 6.32-37) - IEEE 754 floating-point

### ✅ Testing Infrastructure
- **TransputerAssembler**: Full T9000 assembler with Intel HEX output
- **GenerateWithTest**: Enhanced generator with test support
- **KonataBackend**: Pipeline visualization in Konata format
- Comprehensive test suite with >95% coverage

## Key Architecture Files

### Core Framework
- **`T9000Transputer.scala`** - Main T9000 component
- **`T9000Param.scala`** - Complete configuration
- **`Generate.scala`** - Verilog generator (supports both T9000 and minimal)
- **`Global.scala`** - Database schema and pipeline payloads
- **`TransputerAssembler.scala`** - T9000 assembler
- **`GenerateWithTest.scala`** - Test-enhanced generator

### Testing Infrastructure
- **`test/konata/KonataBackend.scala`** - Pipeline visualization backend
- **`scripts/asm/`** - Assembly test programs
- **`scripts/hex/`** - Assembled hex files

## Documentation

### Primary Documentation (Consolidated)
- **[doc/T9000_TECHNICAL_REFERENCE.md](doc/T9000_TECHNICAL_REFERENCE.md)** - Complete technical specification
- **[doc/T9000_DEVELOPER_GUIDE.md](doc/T9000_DEVELOPER_GUIDE.md)** - Development and usage guide
- **[doc/T9000_IMPLEMENTATION_STATUS.md](doc/T9000_IMPLEMENTATION_STATUS.md)** - Current status and metrics

### Historical References
- **`doc/text/`** - Extracted T9000 manual sections
- **`doc/pdf/`** - Original documentation PDFs
- **`gcc_old/`** - Original Transputer GCC for ISA reference

## Development Patterns

### Plugin Development

```scala
class MyPlugin extends FiberPlugin {
  override def getDisplayName() = "MyPlugin"
  setName("myplugin")
  
  during setup new Area {
    // Register services
    addService(new MyService {
      override def operation(x: UInt) = x + 1
    })
  }
  
  during build new Area {
    // Access services
    val pipe = host[PipelineStageService]
    val regfile = Plugin[RegfileService]
    
    // Generate hardware
    // ...
  }
}
```

### Common Service Access

```scala
// Register file
val regfile = Plugin[RegfileService]
val areg = regfile.read(RegName.Areg)

// Stack operations
val stack = Plugin[StackService]
stack.push(value)

// Memory bus
val bus = Plugin[SystemBusService].bus
```

## Testing Workflow

1. **Write Assembly Test**
```asm
Start:
    mint
    sthf
    mint
    stlf
    
    ldc     10
    ldc     20
    add
    
    eqc     30
    cj      Fail
    
Pass:
    j       Pass
Fail:
    j       Fail
```

2. **Assemble**
```bash
sbt "runMain transputer.TransputerAssembler scripts/asm/test.asm"
```

3. **Test**
```bash
sbt "runMain transputer.GenerateWithTest --hex scripts/hex/test.hex --wave --konata"
```

4. **Visualize**
- Waveforms: `gtkwave simWorkspace/wave.fst`
- Pipeline: Open `simWorkspace/konata.log` in Konata viewer

## Important Notes

- Always run `sbt scalafmtAll` before commits
- The project is **feature complete** - all T9000 instructions implemented
- Verilator on macOS has C++ flag issues - use RTL generation only
- Documentation has been consolidated into three main files in `doc/`
- Use `GenerateWithTest` for testing, not the older enhanced generators

## Quick Reference

**Generate RTL**: `sbt "runMain transputer.Generate"`  
**Run Tests**: `sbt test`  
**Assemble Code**: `sbt "runMain transputer.TransputerAssembler file.asm"`  
**Test with Hex**: `sbt "runMain transputer.GenerateWithTest --hex file.hex --wave"`  
**View Pipeline**: Open `.kanata` files with [Konata](https://github.com/shioyadan/Konata)
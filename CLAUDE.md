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

# T9000 Verilog generation
sbt "runMain transputer.T9000Generate --word-width 32 --link-count 4 --fpu true"
sbt "runMain transputer.T9000BootRomDesign"  # Boot ROM standalone
sbt bareBones                            # Minimal configuration

# Specific test execution
sbt "testOnly transputer.T9000*"         # Run T9000-specific tests
sbt "testOnly transputer.T9000StackSpec" # Stack operations test
sbt bareBonesTest                        # Minimal config tests

# FPGA synthesis
sbt synth                               # Generate bitstream for ECP5
sbt report                              # Show utilization reports

# Simulation
sbt "test:runMain transputer.T9000CoreSim"      # Interactive simulation
sbt "test:runMain transputer.BootRomFetchSim"   # Boot ROM simulation
```

### Build Configuration Details
- **Dual build modes**: Source-based (SpinalHDL submodule) vs Published JAR dependencies
- **Generator variants**: `T9000Generate` (full) vs `BareBones` (minimal)
- **Fork JVM**: Enabled for memory management during compilation
- **Test isolation**: Separate test suites per configuration

### Environment Variables
```bash
SPINALHDL_FROM_SOURCE=1                 # Use git submodule instead of published jars
SPINALSIM_WORKSPACE=/tmp                # Redirect simulation workspace
```

## Architecture Deep Dive

### FiberPlugin Architecture

The entire T9000 is constructed using SpinalHDL's **FiberPlugin** system, which provides:

**Plugin Lifecycle:**
1. **Setup Phase**: Plugins register services and declare dependencies
2. **Build Phase**: Hardware generation with fiber-based scheduling  
3. **Database Integration**: Typed configuration through global `Database` context

**Core Plugin Categories:**
- **Infrastructure**: `TransputerPlugin`, `PipelinePlugin`, `PipelineBuilderPlugin`
- **Execution**: `FetchPlugin`, `StackPlugin`, `FpuPlugin`, `SecondaryInstrPlugin`
- **Memory**: `MainCachePlugin`, `WorkspaceCachePlugin`, `MemoryManagementPlugin`
- **System Services**: `SchedulerPlugin`, `TimerPlugin`, `VcpPlugin`, `PmiPlugin`
- **Communication**: DS-Link support, configuration registers

### Service Architecture

Plugins communicate exclusively through **service interfaces**, never direct signal access:

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

**Key Service Types:**
- `PipelineStageService` - Pipeline stage handles and control
- `StackService` - Three-register stack operations (A, B, C registers)
- `RegfileService` - 35+ register file access (including FPU registers)
- `FpuService` - IEEE 754-compliant floating-point operations
- `DataBusService` - Memory bus access and caching
- `SchedulerService` - Process management and context switching
- `VcpService` - Virtual channel processor for communication
- `SystemBusService` - 128-bit BMB system bus

### Pipeline DSL Integration

5-stage CPU pipeline: **Fetch → Decode → Execute → Memory → Writeback**

**Pipeline Construction:**
- `PipelinePlugin` creates stage structure using `StageCtrlPipeline`
- `PipelineBuilderPlugin` connects stages with collected `Link` objects
- Automatic stall/flush handling via `haltWhen()`, `throwIt()`, `flushIt()`
- Global payload propagation (e.g., `Global.OPCODE`, `Global.IPTR`, `Global.MEM_ADDR`)

**Pipeline DSL Key Classes:**
- `Node`, `StageLink`, `CtrlLink` - Basic pipeline building blocks
- `S2MLink`, `ForkLink`, `JoinLink` - Advanced pipeline patterns
- `DirectLink` vs `StageLink` - Combinational vs registered connections

### Database Configuration System

Typed configuration management using SpinalHDL's `Database` pattern:

```scala
object Global {
  val WORD_BITS = Database.blocking[Int]
  val LINK_COUNT = Database.blocking[Int]
  val FPU_PRECISION = Database.blocking[Int]
  // ... 20+ more configuration parameters
}

// Set during TransputerPlugin setup
db(Global.WORD_BITS) = param.wordWidth

// Read from any plugin
val wordBits = Global.WORD_BITS.get
```

## Instruction Set Architecture

### Transputer ISA Implementation

Based on T9000 specification with insights from `gcc_old/` reference compiler:

**Primary Instructions (4-bit opcode + 4-bit operand):**
```
0x0: j <addr>      # Direct jump
0x1: ldlp <n>      # Load local pointer  
0x2: pfix <n>      # Prefix positive
0x3: ldnl <n>      # Load non-local
0x4: ldc <n>       # Load constant
0x5: ldnlp <n>     # Load non-local pointer
0x6: nfix <n>      # Prefix negative  
0x7: ldl <n>       # Load local
0x8: adc <n>       # Add constant
0x9: call <addr>   # Function call
0xA: cj <addr>     # Conditional jump  
0xB: ajw <n>       # Adjust workspace
0xC: eqc <n>       # Equal constant
0xD: stl <n>       # Store local
0xE: stnl <n>      # Store non-local
0xF: opr <n>       # Operate (secondary instructions)
```

**Secondary Instructions (0xF + operand):**
- **M-1 ALU**: `rev`, `add`, `sub`, `and`, `xor`, `or`, `not`, `shl`, `shr`
- **Stack Ops**: `dup`, `pop`, stack manipulation
- **Process Mgmt**: `startp`, `endp`, `runp`, `stopp`
- **FP Operations**: `fpadd`, `fpmul`, `fpdiv` (T9000 direct, no `fpentry`)
- **Memory**: `move`, `move2d`, block operations

### Register Architecture (from GCC analysis)

**Three-Register Evaluation Stack:**
```
Areg (R_AREG=0)    # Top of stack
Breg (R_BREG=1)    # Second value  
Creg (R_CREG=2)    # Third value
```

**Floating-Point Stack:**
```
FAreg (R_FAREG=3)  # FP top of stack
FBreg (R_FBREG=4)  # FP second value
FCreg (R_FCREG=5)  # FP third value
```

**System Registers:**
```
Wreg (R_WREG=6)    # Workspace pointer (fixed, non-allocatable)
IptrReg            # Instruction pointer
WdescReg           # Workspace descriptor
```

## Current Implementation Status

⚠️ **Major Pipeline Redesign in Progress** - The T9000 is currently undergoing a comprehensive pipeline architecture redesign to implement the authentic 5-stage T9000 pipeline with SpinalHDL's Pipeline API.

### ✅ Working Components
- **Core Infrastructure**: Plugin system, database, service architecture
- **Register File**: 35+ registers including FPU shadow registers
- **Memory System**: Hierarchical cache (Main + Workspace caches)
- **Stack Management**: Three-register stack with workspace spill
- **FPU Framework**: IEEE 754 pipeline structure with proper timing
- **Bus System**: 128-bit BMB with up/downsizing bridges

### 🚧 Pipeline Redesign Components (In Progress)
- **5-Stage Architecture**: Fetch/Group → Local/Decode → Address/Cache → Execute → Writeback
- **SpinalHDL Pipeline API**: Automatic register management and timing closure
- **Multi-Lane Execution**: Parallel ALU/FPU using CtrlLaneApi
- **Four 32-bit Buses**: Moving from single 128-bit bus to original T9000 crossbar design
- **Workspace Cache Integration**: Triple-ported cache for two reads + one write per cycle

### ⚠️ Partially Implemented
- **Instruction Fetch**: `FetchPlugin` vs `DummyInstrFetchPlugin` selection
- **Instruction Grouping**: Basic grouping logic present, needs pipeline integration
- **Process Scheduler**: Framework complete, integration pending
- **Timer System**: Dual-timer structure ready
- **Communication**: VCP and PMI plugins structured but not enabled

### ❌ Critical Gaps (Under Active Development)
- **T9000 Verilog Generation**: Plugin initialization deadlock preventing generation
- **Pipeline Stage Connections**: Current plugins don't use new Pipeline API
- **ALU Stage Placement**: Currently in decode, needs to move to execute stage (stage 4)
- **Critical Path Optimization**: Manual register placement causes timing issues

### Milestone Status
- **M-1 (Basic ALU)**: 🔄 **Redesigning for proper pipeline stages**
- **M-2 (Literals)**: ⏳ Framework ready, needs pipeline integration
- **M-3 (Memory)**: ⏳ Cache system operational, needs pipeline timing  
- **M-4 (Processes)**: ⏳ Scheduler infrastructure complete
- **M-5 (Timers)**: ⏳ Timer plugins ready
- **M-6 (FPU)**: ⏳ Pipeline structure complete, needs multi-lane integration

## Key Architecture Files

### Core Framework
- **`T9000Transputer.scala`** - Main T9000 component with enhanced plugin configuration
- **`T9000Param.scala`** - Complete T9000 parameter configuration with all plugins
- **`T9000Generate.scala`** - T9000-specific Verilog generation with CLI options
- **`Global.scala`** - Database schema, 25+ configuration constants, pipeline payloads
- **`Transputer.scala`** - Base transputer component (legacy T800 compatibility)
- **`Generate.scala`** - Original generator (T800-focused)

### Plugin Structure
```
src/main/scala/transputer/plugins/
├── transputer/          # Core TransputerPlugin
├── pipeline/            # Pipeline infrastructure  
├── registers/           # Register file (35+ registers)
├── fetch/              # Instruction fetch
├── grouper/            # Instruction grouping
├── decode/             # Primary instruction decode (needs restoration)
├── execute/            # Secondary instruction execution (needs restoration)
├── stack/              # Three-register stack management
├── fpu/                # IEEE 754 floating-point unit
├── cache/              # Main + workspace caches
├── mmu/                # Memory management unit
├── schedule/           # Process scheduler
├── timers/             # Dual timer system  
├── vcp/                # Virtual channel processor
└── pmi/                # Programmable memory interface
```

### Service Interfaces
Each plugin directory contains `Service.scala` defining the contract:
- **Input/Output**: Method signatures for plugin operations
- **Signal Access**: Direct hardware signal exposure (performance-critical)
- **State Management**: Complex operations with internal state handling

## Development Patterns

### Adding New Plugins

1. **Create Plugin Structure**
```scala
package transputer.plugins.myfeature

class MyFeaturePlugin extends FiberPlugin {
  during setup new Area {
    println(s"[${this.getDisplayName()}] setup start")
    // Register services, validate dependencies
    addService(new MyFeatureService { ... })
    retain() // Hold setup phase
    println(s"[${this.getDisplayName()}] setup end")
  }

  during build new Area {
    println(s"[${this.getDisplayName()}] build start") 
    retain.await() // Wait for setup completion
    
    // Access other services
    val regfile = Plugin[RegfileService]
    val systemBus = Plugin[SystemBusService]
    
    // Generate hardware
    // ...
  }
}
```

2. **Define Service Interface**
```scala
trait MyFeatureService {
  def operation(input: UInt): UInt
  def statusSignal: Bool
}
```

3. **Update T9000Param.scala**
```scala
plugins += new transputer.plugins.myfeature.MyFeaturePlugin()
```

### Testing Strategy

**Unit Testing Pattern:**
```scala
class MyFeatureSpec extends AnyFunSuite {
  class TestDut extends Component {
    val db = T9000Transputer.configureDatabase(T9000Param())
    val testPlugins = Seq(
      new transputer.plugins.transputer.TransputerPlugin(),
      new transputer.plugins.registers.RegFilePlugin(),
      new transputer.plugins.myfeature.MyFeaturePlugin()
    )
    val core = Database(db).on(Transputer(testPlugins))
    val service = core.host[MyFeatureService]
    
    // Test interface
    val io = new Bundle {
      val input = in UInt(32 bits)
      val output = out UInt(32 bits)
    }
    
    io.output := service.operation(io.input)
  }
  
  test("MyFeature basic operation") {
    SimConfig.withWave.compile(new TestDut).doSim { dut =>
      // Test implementation
    }
  }
}
```

### Service Design Guidelines

- **Service Naming**: End with `Service` suffix, descriptive names
- **Pipeline Alignment**: Align services with pipeline stages where possible
- **Signal Ownership**: Services own signals, provide controlled access
- **Method vs Direct Access**: Methods for complex operations, direct signals for performance
- **Error Handling**: Use SpinalHDL's design rule checking, validate inputs

### Common Implementation Patterns

**BMB Bus Integration:**
```scala
val systemBus = Plugin[SystemBusService].bus
val memBmb = Bmb(BmbParameter(...))
// Connect via downsizer for smaller bus widths
```

**Register File Access:**
```scala
val regfile = Plugin[RegfileService] 
val value = regfile.read(RegName.Areg, shadowIndex = 0)
regfile.write(RegName.Breg, newValue, shadowIndex = 0, shadow = false)
```

**Stack Operations:**
```scala
val stack = Plugin[StackService]
stack.push(value)
val result = stack.pop()
stack.rev() // Reverse A <-> B
```

## Debugging and Troubleshooting

### Common Issues

1. **Service Not Found**: Ensure plugin is included in plugin list and service is registered
2. **Hierarchy Violations**: Use service interfaces, avoid direct signal cross-references  
3. **Pipeline Stalls**: Check `haltWhen()` conditions and upstream ready signals
4. **Database Access**: Ensure database context is available in plugin build phase

### Debug Tools

- **Waveform Generation**: `SimConfig.withWave` for VCD output
- **Plugin Trace**: Each plugin prints setup/build phase progress
- **Service Discovery**: Use `host[ServiceType]` pattern for dependency checking
- **Pipeline Debug**: `pipeline.stages.foreach(_.setName())` for stage identification

### Performance Considerations

- **Critical Paths**: Direct signal access for performance-critical operations
- **Bus Optimization**: Use appropriate bus widths and downsizers
- **Cache Efficiency**: Tune cache parameters based on workload
- **Pipeline Depth**: Balance throughput vs latency in pipeline design

## Historical Context

### GCC Reference Analysis
The `gcc_old/` directory contains original Transputer GCC compiler files providing:
- **Instruction Patterns**: Complete ISA implementation reference
- **Register Usage**: Three-register stack conventions and spill patterns
- **Memory Layout**: Workspace organization and addressing modes
- **ABI Details**: Function calling conventions and stack frame layout

### T800 vs T9000 Differences
- **FPU Integration**: T9000 has direct FP instructions vs T800's `fpentry` indirection
- **16-bit Operations**: T9000 adds `ls`/`ss` instructions for efficiency
- **Enhanced Comparison**: Direct `gtu` for unsigned comparisons
- **Performance**: Better instruction throughput and cache hierarchy

This historical analysis informed the current implementation architecture and instruction decode patterns.

## Current Development Focus: Pipeline Redesign 🚧

### Architecture Transition
The T9000 is transitioning from a manually-timed pipeline to an authentic 5-stage pipeline using SpinalHDL's Pipeline API:

**Current Issues:**
- ALU operations performed in decode stage (incorrect for T9000)
- Single 128-bit bus with complex up/downsizing
- Manual register management causing critical path issues
- Plugin initialization deadlocks preventing T9000 generation

**Target Architecture:**
- **Stage 1**: Fetch/Group with instruction buffering
- **Stage 2**: Local/Decode with workspace cache access (triple-ported)
- **Stage 3**: Address/Cache with main cache access (4-bank, dual-ported)  
- **Stage 4**: Execute with parallel ALU/FPU units
- **Stage 5**: Writeback with proper hazard handling

**Key Improvements:**
- SpinalHDL Pipeline API for automatic register management
- Multi-lane execution for parallel ALU/FPU (CtrlLaneApi)
- Four 32-bit buses matching original T9000 crossbar
- < 2ns per stage for 500MHz operation target

### Design Documentation
📚 **Comprehensive documentation created:**
- [`doc/T9000_PIPELINE_REDESIGN.md`](doc/T9000_PIPELINE_REDESIGN.md) - Overall architecture transition plan
- [`doc/T9000_5STAGE_PIPELINE.md`](doc/T9000_5STAGE_PIPELINE.md) - Detailed 5-stage implementation
- [`doc/T9000_SPINALHDL_PIPELINE.md`](doc/T9000_SPINALHDL_PIPELINE.md) - SpinalHDL Pipeline API usage patterns
- [`doc/T9000_TIMING_ANALYSIS.md`](doc/T9000_TIMING_ANALYSIS.md) - High-frequency design considerations

### Immediate Development Priorities

1. **Debug Plugin Initialization**: Resolve deadlock preventing T9000 Verilog generation
2. **Pipeline API Integration**: Convert plugins to use `StageCtrlPipeline`
3. **Multi-Lane Implementation**: Implement parallel ALU/FPU execution
4. **Bus Architecture Redesign**: Four 32-bit crossbar implementation
5. **Critical Path Optimization**: Achieve < 2ns timing per stage

The foundation is excellent - the redesign focuses on leveraging SpinalHDL's advanced pipeline features for a clean, high-performance T9000 implementation.

## Critical SpinalHDL Files for T9000 Development

### Pipeline API (Essential for Pipeline Redesign)
```
ext/SpinalHDL/lib/src/main/scala/spinal/lib/misc/pipeline/
├── package.scala          # Payload type definitions and utilities
├── Node.scala             # Pipeline node base classes and NodeApi
├── CtrlLink.scala         # Stage control with bypass logic
├── Builder.scala          # StageCtrlPipeline implementation
├── StageLink.scala        # Stage-to-stage registered connections
├── CtrlLaneApi.scala      # Multi-lane execution for parallel ALU/FPU
└── NodeLaneApi.scala      # Lane-based resource allocation
```

### Plugin Architecture (FiberPlugin System)
```
ext/SpinalHDL/lib/src/main/scala/spinal/lib/misc/plugin/
├── FiberPlugin.scala      # Base plugin class with setup/build phases
├── Host.scala             # Plugin host and service discovery
└── Fiber.scala            # Concurrent elaboration engine
```

### Bus System (BMB - Bus Matrix Bus)
```
ext/SpinalHDL/lib/src/main/scala/spinal/lib/bus/bmb/
├── Bmb.scala              # BMB bus definition and parameters
├── BmbArbiter.scala       # Multi-master arbitration
├── BmbUpSizerBridge.scala # Width conversion (narrow → wide)
├── BmbDownSizerBridge.scala # Width conversion (wide → narrow)
├── BmbToWishbone.scala    # Protocol conversion
└── BmbSmpInterconnect.scala # Multi-core interconnect
```

### Core SpinalHDL Infrastructure
```
ext/SpinalHDL/core/src/main/scala/spinal/core/
├── fiber/*.scala          # Fiber system for concurrent elaboration
├── AFix.scala             # Fixed-point arithmetic (FPU support)
├── Database.scala         # Typed configuration management
└── Fiber.scala            # Advanced scheduling and dependencies
```

### Advanced Features (Performance Critical)
```
ext/SpinalHDL/lib/src/main/scala/spinal/lib/
├── generator/*.scala      # Code generation utilities
├── formal/*.scala         # Formal verification support  
└── StreamUtils.scala      # Stream processing utilities
```

These files contain the essential SpinalHDL APIs that the T9000 pipeline redesign leverages for automatic register management, multi-lane execution, and high-frequency operation.
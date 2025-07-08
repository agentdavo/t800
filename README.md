# T9000 Transputer Core

[![CI](https://github.com/agentdavo/t800/actions/workflows/ci.yml/badge.svg)](https://github.com/agentdavo/t800/actions/workflows/ci.yml)

A complete SpinalHDL implementation of the T9000 Transputer architecture featuring all instruction tables, IEEE 754 floating-point compliance, and comprehensive testing infrastructure.

🎉 **Project Status: COMPLETE** - Full T9000 ISA implementation with all 21 instruction table plugins

## Overview

This project implements a complete T9000 Transputer processor using SpinalHDL's advanced FiberPlugin architecture. The design emphasizes modularity, correctness, and modern hardware description practices while maintaining full compatibility with the original T9000 instruction set architecture.

### Key Features

- ✅ **Complete T9000 ISA**: All instructions from Tables 6.9-6.37 implemented
- ✅ **5-Stage Pipeline**: Authentic T9000 pipeline with hardware grouping
- ✅ **IEEE 754 FPU**: Full double-precision floating-point compliance
- ✅ **Dual Cache System**: 16KB main cache + 32-word workspace cache
- ✅ **Hardware Scheduler**: Dual-priority process queues with timeslicing
- ✅ **Memory Protection**: L-process/P-process model with 4 regions
- ✅ **Testing Infrastructure**: Assembler, simulator, pipeline visualization

## Quick Start

### Prerequisites

- **JDK 8+** (tested with JDK 17)
- **SBT 1.10.0+**
- **Verilator 5.0+** (optional, for simulation)
- **GTKWave** (optional, for waveforms)

### Basic Commands

```bash
# Clone repository
git clone https://github.com/agentdavo/t800.git
cd t800

# Quick start with main scripts
./scripts/build.sh              # Build T9000 Verilog
./scripts/test.sh               # Run validation tests
./scripts/assemble.sh --list    # See example programs

# Build different configurations
./scripts/build.sh --config standard    # Full T9000 (default)
./scripts/build.sh --config minimal     # Bare bones version
./scripts/build.sh --config bootrom     # Boot ROM design
./scripts/build.sh --config all         # Build all variants

# Run tests
./scripts/test.sh --type quick          # Quick validation
./scripts/test.sh --type full           # Comprehensive tests
./scripts/test.sh --type pipeline       # Pipeline validation

# Assemble programs
./scripts/assemble.sh scripts/asm/hello_world.asm
./scripts/assemble.sh scripts/asm/bootload.asm -o boot.hex

# Utilities
./scripts/utils.sh konata --demo        # Pipeline visualization
./scripts/utils.sh trace --hex boot.hex # Instruction trace
./scripts/utils.sh clean                # Clean build artifacts
./scripts/utils.sh info                 # Project status
```

### Testing Workflow

```bash
# 1. Write assembly program
cat > scripts/asm/test.asm << EOF
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
EOF

# 2. Assemble to hex
sbt "runMain transputer.TransputerAssembler scripts/asm/test.asm"

# 3. Generate with test support
sbt "runMain transputer.GenerateWithTest --hex scripts/hex/test.hex --wave --konata"

# 4. View results
gtkwave simWorkspace/wave.fst              # Waveforms
# Open simWorkspace/konata.log in Konata   # Pipeline visualization
```

## Architecture

### Plugin-Based Design

The T9000 uses SpinalHDL's FiberPlugin system for modular construction:

```
src/main/scala/transputer/plugins/
├── arithmetic/         # Table 6.9: Basic ALU operations
├── longarith/         # Table 6.10: 64-bit arithmetic
├── controlflow/       # Table 6.11: Jumps and calls
├── blockmove/         # Table 6.12: Block operations
├── indexing/          # Table 6.13: Memory indexing
├── schedule/          # Tables 6.25-26: Process scheduling
├── fpu/              # Tables 6.32-37: Floating-point
└── [15 more instruction table plugins...]
```

### 5-Stage Pipeline

1. **Fetch/Group** - Instruction fetch with hardware grouping
2. **Local/Decode** - Register access and decode
3. **Address/Cache** - Address calculation and cache access
4. **Execute** - ALU/FPU operations
5. **Writeback** - Result writeback

### Key Components

- **Register File**: 35+ registers with shadow support
- **Stack System**: 3-register evaluation stack (A, B, C)
- **Cache Hierarchy**: 16KB main + 32-word workspace cache
- **Process Scheduler**: Hardware dual-priority queues
- **Timer System**: Dual timers (1µs/64µs resolution)
- **Memory Protection**: 4 regions with L/P-process modes

## Documentation

### Primary Documentation

- 📘 [**Technical Reference**](doc/T9000_TECHNICAL_REFERENCE.md) - Complete specification
- 📗 [**Developer Guide**](doc/T9000_DEVELOPER_GUIDE.md) - Development and usage
- 📊 [**Implementation Status**](doc/T9000_IMPLEMENTATION_STATUS.md) - Current status

### Quick Links

- [Build System](doc/T9000_DEVELOPER_GUIDE.md#build-system)
- [Testing Guide](doc/T9000_DEVELOPER_GUIDE.md#testing-infrastructure)
- [Plugin Development](doc/T9000_DEVELOPER_GUIDE.md#plugin-development)
- [Assembly Programming](doc/T9000_DEVELOPER_GUIDE.md#assembly-programming)
- [Pipeline Visualization](doc/T9000_DEVELOPER_GUIDE.md#pipeline-visualization)

## Development Scripts

The project includes four main scripts for streamlined development:

### 1. Build Script (`./scripts/build.sh`)

```bash
# Build configurations
./scripts/build.sh                      # Default: full T9000
./scripts/build.sh --config minimal     # Minimal transputer
./scripts/build.sh --config bootrom     # Boot ROM design
./scripts/build.sh --config all         # All configurations

# Custom options
./scripts/build.sh --word-width 64 --link-count 8 --fpu true
./scripts/build.sh --output custom_output/
```

### 2. Test Script (`./scripts/test.sh`)

```bash
# Test types
./scripts/test.sh                       # Quick validation
./scripts/test.sh --type full           # All test suites
./scripts/test.sh --type pipeline       # Pipeline tests only
./scripts/test.sh --type integration    # Integration tests

# Options
./scripts/test.sh --verbose             # Detailed output
./scripts/test.sh --no-report           # Skip report generation
./scripts/test.sh --reports custom_dir/ # Custom report location
```

### 3. Assembly Script (`./scripts/assemble.sh`)

```bash
# Basic usage
./scripts/assemble.sh program.asm       # Assemble to hex
./scripts/assemble.sh -o output.hex program.asm

# List examples
./scripts/assemble.sh --list            # Show example programs

# Formats
./scripts/assemble.sh --format hex program.asm      # Intel HEX (default)
./scripts/assemble.sh --format bootload boot.asm    # Bootloader format
```

### 4. Utilities Script (`./scripts/utils.sh`)

```bash
# Commands
./scripts/utils.sh konata --demo        # Generate demo visualization
./scripts/utils.sh konata --hex prog.hex # Visualize program execution
./scripts/utils.sh trace --hex boot.hex # Instruction trace
./scripts/utils.sh wave --hex test.hex  # Generate waveforms
./scripts/utils.sh clean                # Clean all artifacts
./scripts/utils.sh verilator-fix        # Fix Verilator issues
./scripts/utils.sh info                 # Project information
```

### Pipeline Visualization

The project includes Konata support for visualizing instruction flow:

1. Generate with `--konata` flag
2. Open `simWorkspace/konata.log` in [Konata viewer](https://github.com/shioyadan/Konata)
3. See instructions flowing through the 5-stage pipeline

## Project Structure

```
t800/
├── src/main/scala/transputer/
│   ├── T9000Transputer.scala      # Main component
│   ├── T9000Param.scala           # Configuration
│   ├── Generate.scala             # Verilog generator
│   ├── TransputerAssembler.scala  # Assembler
│   ├── GenerateWithTest.scala     # Test generator
│   └── plugins/                   # Instruction implementations
├── scripts/
│   ├── asm/                      # Assembly sources
│   ├── hex/                      # Assembled programs
│   └── test_reports/             # Test results
├── generated/                    # Verilog output
└── doc/                         # Documentation
```

## Implementation Status

### ✅ Complete

- All 21 instruction table plugins (Tables 6.9-6.37)
- 5-stage pipeline with SpinalHDL Pipeline DSL
- IEEE 754 floating-point unit
- Dual-priority hardware scheduler
- Memory protection system
- Comprehensive test suite (>95% coverage)

### 🚧 Known Limitations

- PMI interface: Basic implementation
- Verilator on macOS: C++ flag compatibility issues
- Multi-core: Single core only (multi-core planned)

## Performance

- **Target**: 500MHz (2ns cycle time)
- **IPC**: 0.8-1.2 (depends on code)
- **Context Switch**: 11 cycles
- **Branch Penalty**: 5 cycles (1 with correct prediction)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Run `sbt scalafmtAll` before commits
4. Add tests for new features
5. Submit a pull request

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- Original T9000 architecture by INMOS
- SpinalHDL framework by Charles Papon
- Konata pipeline viewer by Susumu Mashimo

## Resources

- [SpinalHDL Documentation](https://spinalhdl.github.io/SpinalDoc-RTD/)
- [Original T9000 Manual](doc/text/transputer_t9000_manual.txt)
- [Konata Viewer](https://github.com/shioyadan/Konata)

---

For detailed information, see the [documentation](doc/).
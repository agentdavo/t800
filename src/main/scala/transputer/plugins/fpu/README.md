# T9000 FPU Plugin

## Overview

The T9000 FPU plugin (`FpuPlugin`) provides a complete IEEE 754 compliant floating-point unit implementing all 48 T9000 FPU instructions from Tables 6.32-6.37 of the T9000 specification. The implementation leverages SpinalHDL's AFix type for automatic precision management and clean arithmetic operations.

## Architecture

### Main Entry Point
- **FpuPlugin** - Top-level FiberPlugin that integrates all FPU components

### Core Components
- **FpuInstructionDecoder** - Decodes 48 T9000 FPU instructions
- **FpuDispatcher** - Routes instructions to appropriate execution units
- **Register File** - FPA, FPB, FPC (64-bit) with shadow copies for interrupts
- **Status Register** - 32-bit status with rounding modes and exception flags

### Execution Units
- **FpuAdder** - Addition, subtraction, comparison (2 cycles)
- **FpuMultiplier** - Booth-3 multiplication (2-3 cycles)
- **FpuDividerRooter** - Shared Newton-Raphson division/sqrt logic engine (7-15 cycles)
- **FpuLoadStore** - Memory operations with BMB interface
- **FpuComparison** - IEEE 754 compliant comparisons
- **FpuConversion** - Type conversions (int↔float, precision)
- **FpuControl** - Status and rounding mode control
- **FpuStack** - Stack operations and special functions

## Services

### FpuService
Primary FPU interface providing:
- `FPA`, `FPB`, `FPC`: Direct access to FP registers
- `FPStatus`: Status register access
- `roundingMode`: Current rounding mode (2 bits)
- `errorFlags`: IEEE 754 exception flags (5 bits)
- `isBusy`: Multi-cycle operation in progress
- `setRoundingMode(mode)`: Update rounding mode

### FpuOpsService
Stack-based operations interface:
- `isValidFpuOpcode(opcode)`: Validate FPU instruction
- `getFpuOpType(opcode)`: Get operation type
- `getFpuOpCycles(opcode, precision)`: Get cycle count

### FpuControlService
Control and status management:
- `specialValueDetected`: NaN/Inf/denormal detection
- `trapEnable`: Exception trap generation
- `getErrorFlags`: Read exception flags
- `clearErrorFlags`: Clear exception state

## Supported Instructions

### Load/Store Operations (Table 6.32)
- `FPLDBS`, `FPLDBD` - Load single/double precision
- `FPLDNLS`, `FPLDNLD` - Load non-local single/double
- `FPSTSNL`, `FPSTDNL` - Store single/double non-local

### Arithmetic Operations (Table 6.33)
- `FPADD`, `FPSUB` - Addition/subtraction (2 cycles)
- `FPMUL` - Multiplication (2/3 cycles)
- `FPDIV` - Division (7/15 cycles)
- `FPSQRT` - Square root (7/15 cycles)
- `FPREMFIRST`, `FPREMSTEP` - Remainder operations

### Comparison Operations (Table 6.34)
- `FPEQ`, `FPGT`, `FPLT` - Equality and ordering
- `FPORDERED`, `FPUNORDERED` - NaN detection

### Conversion Operations (Table 6.35)
- `FPI32TOR32`, `FPI32TOR64` - Integer to float
- `FPR32TOI32`, `FPR64TOI32` - Float to integer
- `FPR32TOR64`, `FPR64TOR32` - Precision conversion
- `FPINT`, `FPNINT` - Round to integer

### Rounding Control (Table 6.36)
- `FPROUNDN` - Round to nearest even (00)
- `FPROUNDZ` - Round toward zero (01)
- `FPROUNDP` - Round toward +∞ (10)
- `FPROUNDM` - Round toward -∞ (11)

### Control Operations (Table 6.37)
- `FPUCHK` - Check exceptions
- `FPUCLRERR`, `FPUSETERR` - Manage error flags
- `FPUSTATUS`, `FPUSTATUSR` - Status register access
- `FPSTTEST` - Self-test functionality

### Stack and Special Operations
- `FPDUP`, `FPREV`, `FPPOP` - Stack manipulation
- `FPABS`, `FPNEG` - Sign operations
- `FPLDEXP`, `FPNORM` - Exponent manipulation
- `FPLDZERODB`, `FPLDZEROSN` - Load zero constants

## IEEE 754 Compliance

### Exception Flags (5 bits)
- Bit 0: Invalid Operation
- Bit 1: Division by Zero
- Bit 2: Overflow
- Bit 3: Underflow
- Bit 4: Inexact Result

### Special Value Handling
- Quiet NaN propagation
- Signaling NaN detection
- Infinity arithmetic
- Signed zero support (-0.0 == +0.0)
- Denormal number support

### Rounding Modes
All four IEEE 754 rounding modes are supported and controlled via the status register bits [6:5].

## Pipeline Integration

The FPU integrates with the T9000 5-stage pipeline:
- **Stage 1 (Fetch)**: Normal instruction fetch
- **Stage 2 (Decode)**: FPU opcode detection (0x2A/0x2B prefix)
- **Stage 3 (Execute)**: FPU dispatch and execution start
- **Stage 4 (Memory)**: Multi-cycle operations may stall
- **Stage 5 (Writeback)**: Results to FP registers

Shadow registers automatically switch on interrupt entry/exit.

## AFix Implementation

The FPU leverages SpinalHDL's AFix (Arbitrary Fixed-point) type for:
- Automatic precision tracking through operations
- Built-in overflow/underflow detection
- Native IEEE 754 rounding mode support
- Clean denormal number handling

Example AFix benefits:
```scala
// Traditional approach - manual bit manipulation
val aligned = mantissa >> shift.min(63)

// AFix approach - automatic precision
val aligned = mantissa >> shift
```

## Performance

Target performance at 50 MHz:
- **Single Precision**: 17 MFLOPS
- **Double Precision**: 8.5 MFLOPS

Operation latencies:
- Add/Sub/Compare: 2 cycles (40ns)
- Multiply: 2/3 cycles single/double (40/60ns)
- Divide/Sqrt: 7/15 cycles single/double (140/300ns)
- Load/Store: 2-3 cycles (cache dependent)

## Configuration

Enable in T9000Param.scala:
```scala
if (enableFpu) {
  plugins += new transputer.plugins.fpu.FpuPlugin()
}
```

## Testing

The FPU includes comprehensive test coverage:
- Unit tests for each execution unit
- IEEE 754 compliance tests
- Corner case validation (NaN, Inf, denormal)
- Performance benchmarks

## Implementation Status

✅ **Complete**: All 48 T9000 FPU instructions implemented
✅ **IEEE 754 Compliant**: Full exception and rounding support
✅ **Production Ready**: Old prototype code retired
✅ **AFix Enhanced**: Clean arithmetic implementation
✅ **Fully Integrated**: Pipeline stalls, interrupts, memory interface

The T9000 FPU is a complete, high-performance implementation ready for deployment.
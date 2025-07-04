# T9000 Transputer Technical Reference

This document provides the complete technical reference for the T9000 Transputer implementation in SpinalHDL.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Instruction Set Architecture](#instruction-set-architecture)
3. [Register Architecture](#register-architecture)
4. [Pipeline Architecture](#pipeline-architecture)
5. [Memory System](#memory-system)
6. [Process Management](#process-management)
7. [Floating Point Unit](#floating-point-unit)
8. [System Integration](#system-integration)

## Architecture Overview

The T9000 is a 32-bit transputer processor implemented using SpinalHDL's advanced FiberPlugin architecture. It provides:

- **5-stage pipeline**: Fetch/Group → Local/Decode → Address/Cache → Execute → Writeback
- **Complete T9000 ISA**: All 21 instruction tables (6.9-6.37) implemented
- **IEEE 754 FPU**: Double precision floating-point with full compliance
- **Advanced caching**: 16KB main cache + 32-word workspace cache
- **Hardware scheduling**: Dual-priority process queues with timeslicing
- **Memory protection**: L-process/P-process model with 4 memory regions

### Key Features

- 32-bit word size (configurable)
- 4 DS-Link channels for inter-processor communication
- Hardware instruction grouping for ILP
- Virtual Channel Processor (VCP) for communication
- Programmable Memory Interface (PMI) for external memory

## Instruction Set Architecture

### Primary Instructions (4-bit opcode + 4-bit operand)

| Opcode | Mnemonic | Description | Cycles |
|--------|----------|-------------|--------|
| 0x0 | j | Direct jump | 1 |
| 0x1 | ldlp | Load local pointer | 1 |
| 0x2 | pfix | Prefix positive | 1 |
| 0x3 | ldnl | Load non-local | 2-3 |
| 0x4 | ldc | Load constant | 1 |
| 0x5 | ldnlp | Load non-local pointer | 1 |
| 0x6 | nfix | Prefix negative | 1 |
| 0x7 | ldl | Load local | 1-2 |
| 0x8 | adc | Add constant | 1 |
| 0x9 | call | Function call | 2 |
| 0xA | cj | Conditional jump | 1-2 |
| 0xB | ajw | Adjust workspace | 1 |
| 0xC | eqc | Equal constant | 1 |
| 0xD | stl | Store local | 1-2 |
| 0xE | stnl | Store non-local | 2-3 |
| 0xF | opr | Operate (secondary) | 1-N |

### Secondary Instructions (Selected)

The T9000 implements all secondary instructions from Tables 6.9-6.37:

**Arithmetic (Table 6.9)**
- add, sub, mul, div, rem, and, or, xor, not, shl, shr, gt, eq

**Long Arithmetic (Table 6.10)**
- ladd, lsub, lmul, ldiv, lshl, lshr, norm

**Control Flow (Table 6.11)**
- ret, ldpi, gajw, gcall, lend

**Memory Operations (Table 6.12-6.13)**
- move, move2d, lb, sb, ls, ss, bsub, wsub

**Process Management (Table 6.25-6.26)**
- startp, endp, runp, stopp, ldpri

**Floating Point (Table 6.32-6.37)**
- fpadd, fpsub, fpmul, fpdiv, fpint, fpnan, fpinf

## Register Architecture

### State Registers (Context Switched)

**Core Registers**
- `Areg/Areg.sh` - Top of evaluation stack
- `Breg/Breg.sh` - Second stack register
- `Creg/Creg.sh` - Third stack register
- `IptrReg/IptrReg.sh` - Instruction pointer
- `WdescReg/WdescReg.sh` - Workspace descriptor (pointer + priority)
- `StatusReg/StatusReg.sh` - Process status and control

**FPU Registers**
- `FPAreg/FPAreg.sh` - Floating-point top of stack
- `FPBreg/FPBreg.sh` - FP second register
- `FPCreg/FPCreg.sh` - FP third register
- `FPstatusReg/FPstatusReg.sh` - FP status and rounding mode

**System Registers**
- `ThReg/ThReg.sh` - Trap handler pointer
- `EptrReg/EptrReg.sh` - Error pointer
- `BMreg0-2/BMreg0-2.sh` - 2D block move control
- `WlReg/WlReg.sh`, `WuReg/WuReg.sh` - Watchpoint bounds

### Non-State Machine Registers

- `FptrReg0/1` - Front pointers for priority queues
- `BptrReg0/1` - Back pointers for priority queues
- `ClockReg0/1` - System timers (1µs/64µs)
- `TptrReg0/1` - Timer queue pointers

## Pipeline Architecture

### 5-Stage Pipeline Design

The T9000 uses an authentic 5-stage pipeline matching the original specification:

1. **Fetch/Group (F)**
   - Instruction fetch from memory
   - Hardware instruction grouping
   - Branch prediction
   - 4-byte fetch width

2. **Local/Decode (D)**
   - Instruction decode
   - Register file access
   - Workspace cache access (triple-ported)
   - Operand preparation

3. **Address/Cache (A)**
   - Address calculation
   - Main cache access (4-bank, dual-ported)
   - Memory protection checks
   - TLB lookup

4. **Execute (X)**
   - ALU operations
   - FPU operations (multi-cycle)
   - Branch resolution
   - Trap detection

5. **Writeback (W)**
   - Register writeback
   - Cache update
   - Process state update
   - Exception handling

### Pipeline Features

- **Forwarding**: Full forwarding paths between stages
- **Hazard Detection**: RAW, WAR, WAW hazard handling
- **Branch Prediction**: Static prediction with BTB
- **Multi-cycle Operations**: FPU and complex instructions
- **Pipeline Interlocks**: Automatic stall generation

## Memory System

### Cache Hierarchy

**Main Cache (16KB)**
- 4-bank architecture for parallel access
- 32-byte cache lines
- 2-way set associative
- Write-through with write buffer
- Critical word first

**Workspace Cache (32 words)**
- Direct mapped
- Single cycle access
- Triple-ported for ILP
- Hardware managed
- Optimized for stack operations

### Memory Protection

**Four Memory Regions**
- Each region has base, size, and permissions
- Logical to physical translation
- Access violation detection
- Separate L-process and P-process permissions

**Protection Modes**
- L-process: Trusted, full access
- P-process: Protected, restricted access
- Trap on violation
- Hardware enforced

## Process Management

### Scheduler Architecture

**Dual Priority Queues**
- High priority: Real-time processes
- Low priority: Time-sliced processes
- Hardware queue management
- O(1) scheduling operations

**Process States**
- Running: Currently executing
- Ready: In scheduler queue
- Waiting: On timer queue
- Blocked: On channel/resource

**Context Switching**
- Hardware managed
- 11-cycle switch latency
- Automatic state save/restore
- Shadow register switching

### Timer System

**Dual Timers**
- ClockReg0: 1µs resolution
- ClockReg1: 64µs resolution
- Timer queue management
- Automatic process wakeup

**Timeslicing**
- 256µs quantum for low priority
- Preemptive scheduling
- Fair time distribution

## Floating Point Unit

### IEEE 754 Compliance

**Supported Operations**
- Single and double precision
- All basic arithmetic (+, -, ×, ÷)
- Square root, conversions
- Comparisons and classifications

**Rounding Modes**
- Round to nearest (ties to even)
- Round toward +∞
- Round toward -∞
- Round toward zero

**Exception Handling**
- Invalid operation
- Division by zero
- Overflow/Underflow
- Inexact result
- Hardware trap generation

### FPU Pipeline

- 3-7 cycle latency depending on operation
- Fully pipelined for throughput
- Early-out for special cases
- Denormal number support

## System Integration

### Bus Architecture

**BMB System Bus**
- 128-bit data width
- Pipelined transactions
- Multiple outstanding requests
- Automatic width conversion

**External Interfaces**
- PMI: External memory interface
- VCP: Virtual channel processor
- DS-Links: 4 serial communication links
- Interrupt controller

### Configuration

**Build-time Parameters**
- Word width (32/64 bit)
- Cache sizes
- Link count
- FPU precision
- Pipeline features

**Runtime Configuration**
- Memory regions
- Timer values
- Interrupt masks
- Process priorities

### Debug Support

- Hardware breakpoints
- Watchpoints
- Single-step execution
- Pipeline visibility
- Performance counters

## Performance Characteristics

**Target Metrics**
- 500MHz operation (2ns cycle)
- 1.2 IPC average
- 5-cycle branch penalty
- 3-cycle cache miss penalty

**Critical Paths**
- ALU operations: < 2ns
- Cache access: < 1.8ns
- Register file: < 1.5ns
- FPU operations: Multi-cycle

## Implementation Details

**Technology**
- SpinalHDL 1.10.2
- FiberPlugin architecture
- Service-oriented design
- ~50K lines of Scala

**Verification**
- Comprehensive test suite
- Instruction-level verification
- Pipeline validation
- Compliance testing
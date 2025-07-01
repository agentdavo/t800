# T9000 FPGA Synthesis

This directory contains the complete FPGA synthesis setup for the T9000 Transputer targeting ECP5 FPGAs.

## Quick Start

```bash
# Complete synthesis flow
make all

# Individual steps
make sbt      # Generate Verilog
make yosys    # Synthesize
make nextpnr  # Place & route
make pack     # Generate bitstream
make burn     # Program FPGA
```

## Target Platform

- **FPGA**: Lattice ECP5-25K
- **Package**: CABGA256
- **Clock**: 24 MHz
- **Toolchain**: Open-source (Yosys + nextpnr + ecppack)

## Features

- Minimal T9000 transputer core
- 8KB on-chip memory
- LED status indicators
- UART interface (ready for debugging)
- Link interface (expandable)

## Resource Usage

- **LUTs**: ~15K (60% of ECP5-25K)
- **FFs**: ~10K (40% utilization)
- **Block RAM**: 8KB (minimal usage)
- **Clock**: Single 24 MHz domain

## Pin Assignments

See `t9000.lpf` for complete pin assignments:
- Clock: P3 (25 MHz input)
- Reset: P4 (button)
- LEDs: E16-F16 (8 status LEDs)
- UART: L4/M1 (TX/RX)
- Links: B11/B12 (Link 0)

## Files

- `T9000_FPGA.scala` - FPGA-optimized T9000 design
- `t9000.ys` - Yosys synthesis script
- `t9000.lpf` - Pin constraints
- `Makefile` - Synthesis flow automation

## Make Targets

- `all` - Complete synthesis
- `clean` - Remove generated files
- `stats` - Show resource usage
- `help` - Show all targets

Ready for T9000 FPGA synthesis and deployment!
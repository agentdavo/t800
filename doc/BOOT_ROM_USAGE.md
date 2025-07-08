# T9000 Boot ROM Usage Guide

This guide explains how to use the boot ROM feature to initialize the T9000 with programs at startup.

## Overview

The T9000 can be configured to boot from ROM at address 0x80000000. This allows the processor to start executing code immediately after reset without external loading.

## Quick Start

### 1. Using the INMOS Bootloader

The easiest way to boot the T9000 is with the official INMOS bootloader:

```bash
# Generate T9000 with INMOS bootloader in ROM
sbt "runMain transputer.Generate --boot-inmos"

# Or use the dedicated boot ROM generator
sbt "runMain transputer.GenerateWithBootRom --inmos"
```

### 2. Using Custom Programs

To boot with your own program:

```bash
# First, write your assembly program
cat > scripts/asm/myboot.asm << EOF
Start:
    mint            ; Initialize processor
    sthf
    mint
    stlf
    
    ; Your boot code here
    ldc     42      ; Example: load constant
    
    ; Must loop forever or halt properly
Loop:
    j       Loop
EOF

# Assemble to hex
sbt "runMain transputer.TransputerAssembler scripts/asm/myboot.asm"

# Generate with your boot program
sbt "runMain transputer.Generate --boot-rom-hex scripts/hex/myboot.hex"
```

## Command Line Options

### For `transputer.Generate`:

- `--boot-inmos` - Boot with INMOS bootloader
- `--enable-boot-rom` - Enable boot ROM
- `--boot-rom-hex <file>` - Specify hex file to load
- `--boot-rom-address <addr>` - ROM start address (default: 0x80000000)
- `--boot-rom-size <bytes>` - ROM size (default: 4096)

### For `transputer.GenerateWithBootRom`:

```bash
# Boot with INMOS loader
sbt "runMain transputer.GenerateWithBootRom --inmos"

# Boot with custom hex file
sbt "runMain transputer.GenerateWithBootRom scripts/hex/custom.hex"
```

## How It Works

1. **Reset Vector**: The T9000 reset vector is set to 0x80000000
2. **ROM Content**: The hex file is loaded into ROM starting at this address
3. **Execution**: On reset, the processor fetches instructions from ROM
4. **Memory Files**: The generator creates initialization files for simulation

## Generated Files

When using boot ROM, these files are created:

```
generated/
├── T9000Transputer.v          # Main design (with --boot-inmos)
├── T9000TransputerWithBoot.v  # Design with boot ROM
├── boot_rom.hex               # Verilog memory init format
└── boot_rom.bin               # Binary format
```

## Simulation Usage

### With Verilog Simulators

The generated Verilog can use `$readmemh` to initialize ROM:

```verilog
// In your testbench
initial begin
  $readmemh("boot_rom.hex", dut.rom_memory);
end
```

### With Verilator

```cpp
// In your C++ testbench
Verilated::commandArgs(argc, argv);
top->eval();
// ROM is automatically initialized from boot_rom.hex
```

## Boot Program Requirements

Your boot program must:

1. **Initialize the processor** - Set up stack and queues
2. **Configure hardware** - Timer, links, etc.
3. **Handle reset properly** - Either loop forever or jump to main code
4. **Fit in ROM** - Default size is 4KB

### Minimal Boot Template

```asm
; Minimal boot program template
VAL STACK_SIZE IS 256

Start:
    ; Essential initialization
    mint                    ; NotProcess.p
    sthf                    ; High priority queue
    mint
    stlf                    ; Low priority queue
    
    ; Initialize timer
    ldc     0
    sttimer
    
    ; Clear error flags
    testerr
    clrhalterr
    
    ; Set up workspace
    ajw     STACK_SIZE
    
    ; Jump to main program
    ldc     MainProgram
    gcall
    
    ; Should never reach here
Halt:
    j       Halt

MainProgram:
    ; Your application code
    ret
```

## INMOS Bootloader

The INMOS bootloader (`scripts/hex/bootload.hex`) provides:

- Two-stage bootstrap process
- Link initialization
- Memory setup
- Process initialization
- Ready for external program loading

It's the recommended starting point for most applications.

## Troubleshooting

### ROM Not Loading

1. Check hex file path is correct
2. Verify hex file format (Intel HEX)
3. Ensure addresses match ROM location

### Processor Not Starting

1. Verify reset vector is 0x80000000
2. Check first instruction is valid
3. Ensure initialization sequence is correct

### Simulation Issues

1. Place memory init files in simulation directory
2. Check file permissions
3. Verify simulator supports `$readmemh`

## Example: Hello World Boot

```bash
# Create hello world boot program
cat > scripts/asm/hello_boot.asm << EOF
Start:
    mint
    sthf
    mint
    stlf
    
    ; Print message (simplified)
    ldc     Message
    call    PrintString
    
Done:
    j       Done

Message:
    db      "Hello from T9000!", 0

PrintString:
    ; Implementation depends on output device
    ret
EOF

# Build and generate
sbt "runMain transputer.TransputerAssembler scripts/asm/hello_boot.asm"
sbt "runMain transputer.Generate --boot-rom-hex scripts/hex/hello_boot.hex"
```

## Next Steps

1. Study the INMOS bootloader source: `scripts/asm/bootload.asm`
2. Create custom boot sequences for your application
3. Integrate with FPGA block RAM for hardware implementation
4. Use boot ROM for diagnostic and test programs
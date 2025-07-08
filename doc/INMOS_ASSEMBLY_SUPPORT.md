# INMOS Assembly Support

The T9000 TransputerAssembler now supports official INMOS transputer assembly syntax, enabling assembly of official transputer code examples.

## Supported INMOS Directives

- `.TRANSPUTER` - Indicates transputer assembly mode
- `TITLE` - Program title (informational)
- `PAGE` - Page formatting (ignored)
- `EQU` - Define constants (INMOS style)
- `ORG` - Set origin address
- `DB` - Define bytes (data)
- `END` - End of assembly

## Assembling INMOS Code

### Basic Usage

```bash
# Assemble any INMOS format file
sbt "runMain transputer.TransputerAssembler scripts/asm/myfile.asm"

# Output goes to scripts/hex/myfile.hex
```

### Official Examples

Two official INMOS hello world examples are provided:

1. **hello-iserver.asm** - Primary bootstrap version
   - Fits in 255 bytes (bootstrap limit)
   - Uses IServer protocol for console output
   - Assembled to: `scripts/hex/hello-iserver.hex`

2. **hello-iserver-rom.asm** - ROM boot version
   - Designed for ROM at 0x7FFFFFFE
   - Full initialization sequence
   - Assembled to: `scripts/hex/hello-iserver-rom.hex`

### Simplified Versions

Due to forward reference limitations in our assembler, simplified versions were created:

- `hello-iserver-modified.asm` - Resolved forward references
- `hello-iserver-rom-simple.asm` - Simplified ROM layout

### Assembly Script

Use the provided script to assemble INMOS files:

```bash
./scripts/assemble_inmos.sh
```

This handles the forward reference issue in hello-iserver.asm where:
```asm
MemLength EQU MemStop - MemStart  ; MemStop defined later
```

## IServer Protocol

The hello world examples demonstrate the IServer protocol:

### Frame Structure
```
2 bytes - Frame length
1 byte  - Request type (0x0F = REQ_PUTS)
4 bytes - Stream ID (0x01 = STDOUT)
2 bytes - Data length
N bytes - String data
```

### Key Instructions
- `outbyte` - Output single byte to link
- `outword` - Output 32-bit word to link
- `out` - Output block of data

### Example Code
```asm
; Send "hello world" to console
ldc     LINK0_OUTPUT
ldc     REQ_PUTS
outbyte                 ; Send request type

ldc     LINK0_OUTPUT
ldc     STDOUT_STREAMID
outword                 ; Send stream ID
```

## Limitations

1. **Forward References**: EQU expressions cannot reference labels defined later
2. **Complex ORG**: Expressions in ORG directives must be resolvable in first pass
3. **Macro Support**: No macro expansion support

## Boot ROM Usage

To boot from the assembled code:

```bash
# Use INMOS bootloader
sbt "runMain transputer.Generate --boot-inmos"

# Use custom ROM
sbt "runMain transputer.Generate --boot-rom-hex scripts/hex/hello-iserver-rom.hex"
```

## Testing

Test files demonstrating INMOS syntax:
- `scripts/asm/test_simple.asm` - Basic directive test
- `scripts/asm/hello-iserver-simple.asm` - Simplified IServer example

All assembled files are placed in `scripts/hex/` in Intel HEX format.
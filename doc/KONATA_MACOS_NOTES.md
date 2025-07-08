# Konata Pipeline Visualization on macOS

## Current Status

The T9000 supports Konata pipeline visualization for cycle-accurate performance analysis. However, there are compilation issues with Verilator on macOS that prevent full simulation.

## The Issue

When running simulation with Konata on macOS:
```bash
sbt "runMain transputer.GenerateWithTest --hex program.hex --konata --wave"
```

The compilation fails due to C++ compiler flag incompatibilities:
- `-fcf-protection=none` is not supported on macOS/Darwin
- Clang-specific flags conflict with Verilator expectations

## Workarounds

### 1. Generate RTL Only
Generate Verilog with boot ROM but without simulation:
```bash
sbt "runMain transputer.Generate --enable-boot-rom --boot-rom-hex scripts/hex/hello-iserver.hex"
```

### 2. Use Linux Environment
- Docker container with Linux
- Virtual machine (VMware, VirtualBox)
- Remote Linux server
- GitHub Codespaces

### 3. Alternative Visualization
Use waveform viewers (GTKWave) for basic pipeline analysis:
```bash
# On Linux or in container
sbt "runMain transputer.GenerateWithTest --hex program.hex --wave"
gtkwave simWorkspace/wave.fst
```

## Konata Features (When Working)

The T9000 Konata integration provides:
- 5-stage pipeline visualization (Fetch→Decode→Address→Execute→Writeback)
- Instruction flow tracking
- Stall and flush visualization
- Dependency arrows
- Multi-lane execution views
- IPC (Instructions Per Cycle) analysis

## Boot ROM Testing Status

Successfully assembled INMOS examples:
- ✅ `hello-iserver.hex` - Primary bootstrap (161 bytes)
- ✅ `hello-iserver-rom.hex` - ROM boot version (171 bytes)  
- ✅ `test_simple.hex` - Basic test (33 bytes)

These can be loaded into boot ROM:
```bash
# Generate with specific boot ROM
sbt "runMain transputer.Generate --enable-boot-rom --boot-rom-hex scripts/hex/hello-iserver.hex"

# Or use INMOS bootloader
sbt "runMain transputer.Generate --boot-inmos"
```

## Future Solutions

1. **Fix Verilator Flags**: Modify SpinalHDL's Verilator backend for macOS compatibility
2. **Use Icarus Verilog**: Alternative simulator without C++ compilation
3. **Native macOS Support**: Wait for SpinalHDL updates

## Linux Setup for Full Testing

```bash
# In Docker or Linux VM
git clone <repo>
cd t800

# Install dependencies
sudo apt-get update
sudo apt-get install -y default-jdk sbt verilator gtkwave

# Run with Konata
sbt "runMain transputer.GenerateWithTest --hex scripts/hex/hello-iserver.hex --konata"

# View pipeline trace
# Download Konata viewer from: https://github.com/shioyadan/Konata
# Open simWorkspace/konata.log
```

The Konata visualization would show the T9000 executing the IServer protocol hello world program, with each instruction flowing through the pipeline stages.
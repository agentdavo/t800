# ECP5 FPGA Toolchain Installation

## Required Tools for T9000 FPGA Synthesis

The T9000 FPGA synthesis flow requires the following open-source ECP5 toolchain:

1. **yosys** - Logic synthesis (✓ already installed)
2. **nextpnr-ecp5** - Place and route (❌ missing) 
3. **ecppack** - Bitstream generation (✓ already installed)
4. **openFPGALoader** - Programming tool (optional)

## Installation Options

### Option 1: OSS CAD Suite (Recommended)
Complete toolchain bundle with all ECP5 tools:

```bash
# Download latest OSS CAD Suite for macOS
curl -L https://github.com/YosysHQ/oss-cad-suite-build/releases/latest/download/oss-cad-suite-darwin-x64.tgz -o oss-cad-suite.tgz

# Extract and install
tar -xzf oss-cad-suite.tgz
sudo mv oss-cad-suite /opt/

# Add to PATH
echo 'export PATH="/opt/oss-cad-suite/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Option 2: Homebrew (Individual packages)
```bash
# Install Project Trellis (includes nextpnr-ecp5)
brew install prjtrellis

# Verify installation
which nextpnr-ecp5
which ecppack
```

### Option 3: Manual Build
If automated installation fails, build from source:

```bash
# Install dependencies
brew install cmake boost python3 eigen

# Clone and build nextpnr
git clone --recursive https://github.com/YosysHQ/nextpnr.git
cd nextpnr
cmake -DARCH=ecp5 -DBUILD_TESTS=OFF .
make -j$(nproc)
sudo make install
```

## Verification

After installation, verify tools are available:

```bash
yosys --version
nextpnr-ecp5 --version  
ecppack --help
```

## Current Status

- **yosys**: ✓ Available at /usr/local/bin/yosys
- **ecppack**: ✓ Available at /usr/local/bin/ecppack  
- **nextpnr-ecp5**: ❌ Not found in PATH

## Synthesis Flow Status

Current synthesis progress:
- ✅ Verilog generation (T9000_FPGA.v)
- ✅ Yosys synthesis (t9000.json) 
- ❌ Place & route (missing nextpnr-ecp5)
- ❌ Bitstream generation (requires P&R output)

Once nextpnr-ecp5 is installed, run:
```bash
cd fpga/
make nextpnr pack
```

## Target FPGA

- **Device**: ECP5-25K 
- **Package**: CABGA256
- **Speed Grade**: 6
- **Clock**: 24 MHz

## Pin Configuration (t9000.lpf)

```
LOCATE COMP "io_clk" SITE "P3";
LOCATE COMP "io_rst" SITE "P4"; 
LOCATE COMP "io_led[0]" SITE "E16";
LOCATE COMP "io_led[1]" SITE "F16";
# ... (additional pins defined)
```
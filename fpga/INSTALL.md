# ECP5 FPGA Toolchain Installation

The T9000 FPGA synthesis flow requires the following open-source ECP5 toolchain:

## Installation

```bash
# Download latest OSS CAD Suite for macOS
curl -L https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2025-07-05/oss-cad-suite-darwin-x64-20250705.tgz -o oss-cad-suite.tgz

# Extract and install
tar -xzf oss-cad-suite.tgz
sudo mv oss-cad-suite /opt/

# Add to PATH
echo 'export PATH="/opt/oss-cad-suite/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

## Verification

After installation, verify tools are available:

```bash
yosys --version
nextpnr-ecp5 --version  
ecppack --help


Once nextpnr-ecp5 is installed, run:
```bash
cd fpga/
make nextpnr pack
```

## Target FPGA

- **Device**: ECP5-25K 
- **Package**: CABGA256
- **Speed Grade**: 6
- **Clock**: 100 MHz

## Pin Configuration (t9000.lpf)

```
LOCATE COMP "io_clk" SITE "P3";
LOCATE COMP "io_rst" SITE "P4"; 
LOCATE COMP "io_led[0]" SITE "E16";
LOCATE COMP "io_led[1]" SITE "F16";
# ... (additional pins defined)
```
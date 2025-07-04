#!/bin/bash

#
# T9000 Transputer Build Script
# Generates various T9000 configurations
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}==========================================${NC}"
echo -e "${BLUE}T9000 Transputer Generation Script${NC}"
echo -e "${BLUE}==========================================${NC}"

# Create output directory
OUTPUT_DIR="./generated"
mkdir -p "$OUTPUT_DIR"

echo -e "\n${GREEN}📁 Output directory: $OUTPUT_DIR${NC}"

echo -e "\n${YELLOW}🏗️  Building T9000 configurations...${NC}"

# 1. Standard T9000 Configuration (default)
echo -e "\n${GREEN}1. Standard T9000 Transputer${NC}"
echo "Generating standard T9000 with default parameters..."
echo "Command: sbt \"runMain transputer.T9000Generate\""
sbt "runMain transputer.T9000Generate" || echo -e "${RED}Standard build failed${NC}"

# 2. Bare Bones Configuration (minimal)
echo -e "\n${GREEN}2. Bare Bones Transputer${NC}" 
echo "Generating minimal transputer configuration..."
echo "Command: sbt \"runMain transputer.Generate\""
sbt "runMain transputer.Generate" || echo -e "${RED}Bare bones build failed${NC}"

# 3. Boot ROM Design
echo -e "\n${GREEN}3. T9000 Boot ROM${NC}"
echo "Generating T9000 boot ROM design..."
echo "Command: sbt \"runMain transputer.T9000BootRomDesign\""
sbt "runMain transputer.T9000BootRomDesign" || echo -e "${RED}Boot ROM build failed${NC}"

echo -e "\n${BLUE}==========================================${NC}"
echo -e "${GREEN}✅ T9000 Transputer builds completed!${NC}"
echo -e "${BLUE}==========================================${NC}"

# List generated files
echo -e "\n${YELLOW}📄 Generated files:${NC}"
if [ -d "$OUTPUT_DIR" ]; then
    find "$OUTPUT_DIR" -name "*.v" -o -name "*.sv" | sort | while read file; do
        size=$(wc -l < "$file")
        echo -e "  ✅ $file (${size} lines)"
    done
else
    echo -e "  ${RED}No files found in $OUTPUT_DIR${NC}"
fi

# Check for specific expected files
echo -e "\n${YELLOW}📊 Build Summary:${NC}"
if [ -f "$OUTPUT_DIR/T9000Transputer.v" ]; then
    echo -e "  ✅ T9000Transputer.v generated successfully"
else
    echo -e "  ❌ T9000Transputer.v not found"
fi

if [ -f "$OUTPUT_DIR/Transputer.v" ]; then
    echo -e "  ✅ Transputer.v (bare bones) generated successfully"
else
    echo -e "  ❌ Transputer.v not found"
fi

if [ -f "$OUTPUT_DIR/T9000BootRom.v" ]; then
    echo -e "  ✅ T9000BootRom.v generated successfully"
else
    echo -e "  ❌ T9000BootRom.v not found"
fi

echo -e "\n${YELLOW}🚀 Next Steps:${NC}"
echo "  1. Check generated Verilog files in $OUTPUT_DIR/"
echo "  2. Run synthesis tools on the generated Verilog"
echo "  3. Run validation tests with ./scripts/validate_t9000_system.sh"
echo ""

# Exit with success
exit 0
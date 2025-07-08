#!/bin/bash

#
# T9000 Transputer Build Script
# Generates various T9000 configurations and Verilog files
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
CONFIG="standard"
WORD_WIDTH=32
LINK_COUNT=4
ENABLE_FPU="true"
OUTPUT_DIR="./generated"

# Function to display usage
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Build T9000 Transputer Verilog files"
    echo ""
    echo "Options:"
    echo "  -c, --config <type>    Configuration type: standard|minimal|bootrom|all (default: standard)"
    echo "  -w, --word-width <n>   Word width in bits (default: 32)"
    echo "  -l, --link-count <n>   Number of transputer links (default: 4)"
    echo "  -f, --fpu <bool>       Enable FPU (true/false, default: true)"
    echo "  -o, --output <dir>     Output directory (default: ./generated)"
    echo "  -h, --help             Display this help message"
    echo ""
    echo "Configurations:"
    echo "  standard   - Full T9000 with all 21 instruction table plugins"
    echo "  minimal    - Bare bones transputer for basic functionality"
    echo "  bootrom    - Boot ROM design with hello world"
    echo "  all        - Build all configurations"
    echo ""
    echo "Examples:"
    echo "  $0                                    # Build standard T9000"
    echo "  $0 --config minimal                   # Build minimal transputer"
    echo "  $0 --config all                       # Build all configurations"
    echo "  $0 --word-width 64 --link-count 8    # Custom T9000 configuration"
    exit 0
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--config)
            CONFIG="$2"
            shift 2
            ;;
        -w|--word-width)
            WORD_WIDTH="$2"
            shift 2
            ;;
        -l|--link-count)
            LINK_COUNT="$2"
            shift 2
            ;;
        -f|--fpu)
            ENABLE_FPU="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

echo -e "${BLUE}==========================================${NC}"
echo -e "${BLUE}T9000 Transputer Build Script${NC}"
echo -e "${BLUE}==========================================${NC}"

# Create output directory
mkdir -p "$OUTPUT_DIR"
echo -e "\n${GREEN}📁 Output directory: $OUTPUT_DIR${NC}"

# Function to build a configuration
build_config() {
    local config_name="$1"
    local command="$2"
    local expected_file="$3"
    
    echo -e "\n${GREEN}Building: $config_name${NC}"
    echo "Command: $command"
    
    if eval "$command"; then
        if [ -f "$expected_file" ]; then
            local size=$(wc -l < "$expected_file")
            echo -e "  ${GREEN}✅ Success: $expected_file (${size} lines)${NC}"
            return 0
        else
            echo -e "  ${RED}❌ Failed: $expected_file not found${NC}"
            return 1
        fi
    else
        echo -e "  ${RED}❌ Build failed${NC}"
        return 1
    fi
}

# Build based on configuration
case $CONFIG in
    standard)
        echo -e "\n${YELLOW}🏗️  Building Standard T9000 Configuration${NC}"
        build_config "Standard T9000 Transputer" \
            "sbt \"runMain transputer.T9000Generate --word-width $WORD_WIDTH --link-count $LINK_COUNT --enable-fpu $ENABLE_FPU\"" \
            "$OUTPUT_DIR/T9000Transputer.v"
        ;;
    
    minimal)
        echo -e "\n${YELLOW}🏗️  Building Minimal Configuration${NC}"
        build_config "Bare Bones Transputer" \
            "sbt \"runMain transputer.Generate\"" \
            "$OUTPUT_DIR/Transputer.v"
        ;;
    
    bootrom)
        echo -e "\n${YELLOW}🏗️  Building Boot ROM Configuration${NC}"
        build_config "T9000 Boot ROM" \
            "sbt \"runMain transputer.T9000BootRomDesign\"" \
            "$OUTPUT_DIR/T9000BootRomDesign.v"
        ;;
    
    all)
        echo -e "\n${YELLOW}🏗️  Building All Configurations${NC}"
        
        # Build all three configurations
        build_config "Standard T9000 Transputer" \
            "sbt \"runMain transputer.T9000Generate --word-width $WORD_WIDTH --link-count $LINK_COUNT --enable-fpu $ENABLE_FPU\"" \
            "$OUTPUT_DIR/T9000Transputer.v"
        
        build_config "Bare Bones Transputer" \
            "sbt \"runMain transputer.Generate\"" \
            "$OUTPUT_DIR/Transputer.v"
        
        build_config "T9000 Boot ROM" \
            "sbt \"runMain transputer.T9000BootRomDesign\"" \
            "$OUTPUT_DIR/T9000BootRomDesign.v"
        ;;
    
    *)
        echo -e "${RED}Unknown configuration: $CONFIG${NC}"
        echo "Valid configurations: standard, minimal, bootrom, all"
        exit 1
        ;;
esac

echo -e "\n${BLUE}==========================================${NC}"
echo -e "${GREEN}✅ Build Complete!${NC}"
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

echo -e "\n${YELLOW}🚀 Next Steps:${NC}"
echo "  1. Synthesize Verilog files for FPGA implementation"
echo "  2. Run tests with: ./scripts/test.sh"
echo "  3. Assemble programs with: ./scripts/assemble.sh"
echo ""

exit 0
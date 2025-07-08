#!/bin/bash

#
# T9000 Transputer Utilities Script
# Various utility functions for development and debugging
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Function to display usage
usage() {
    echo "Usage: $0 <command> [options]"
    echo ""
    echo "T9000 Transputer development utilities"
    echo ""
    echo "Commands:"
    echo "  konata [options]      Generate or view Konata pipeline visualization"
    echo "  trace [options]       Run instruction trace simulation"
    echo "  wave [options]        Generate waveform simulation"
    echo "  clean                 Clean build artifacts and simulation files"
    echo "  verilator-fix         Fix Verilator PCH compilation issues"
    echo "  info                  Display project information and status"
    echo ""
    echo "Use '$0 <command> --help' for command-specific options"
    exit 0
}

# Konata visualization command
cmd_konata() {
    local HEX_FILE=""
    local OUTPUT_FILE="simWorkspace/Transputer/konata.log"
    local DEMO=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --hex)
                HEX_FILE="$2"
                shift 2
                ;;
            --output|-o)
                OUTPUT_FILE="$2"
                shift 2
                ;;
            --demo)
                DEMO=true
                shift
                ;;
            --help|-h)
                echo "Usage: $0 konata [options]"
                echo ""
                echo "Generate Konata pipeline visualization"
                echo ""
                echo "Options:"
                echo "  --hex <file>      Hex file to simulate"
                echo "  --output <file>   Output Konata log file"
                echo "  --demo            Generate demo visualization"
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                exit 1
                ;;
        esac
    done
    
    echo -e "${BLUE}Generating Konata Pipeline Visualization${NC}"
    echo "========================================="
    
    if [ "$DEMO" = true ]; then
        # Generate demo Konata log
        mkdir -p $(dirname "$OUTPUT_FILE")
        cat > "$OUTPUT_FILE" << 'EOF'
Kanata	0004
C=	0
I	0	0	0
L	0	0	80000000: mint
S	0	0	fetch
S	0	1	decode
S	0	2	execute
S	0	3	writeback
R	0	0	4
I	1	4	0
L	1	0	80000001: sthf
S	1	0	fetch
S	1	1	decode
S	1	2	execute
S	1	3	writeback
R	1	0	8
I	2	8	0
L	2	0	80000002: ldc #8
S	2	0	fetch
S	2	1	decode
S	2	2	execute
S	2	3	writeback
R	2	0	12
C=	15
EOF
        echo -e "${GREEN}✓ Demo Konata log generated: $OUTPUT_FILE${NC}"
    elif [ -n "$HEX_FILE" ]; then
        # Run simulation with Konata output
        echo "Running simulation with hex file: $HEX_FILE"
        sbt "runMain transputer.GenerateWithTest --hex $HEX_FILE --konata"
    else
        echo -e "${RED}Error: Specify --hex <file> or --demo${NC}"
        exit 1
    fi
    
    echo ""
    echo "To visualize the pipeline:"
    echo "1. Download Konata from: https://github.com/shioyadan/Konata"
    echo "2. Open: $OUTPUT_FILE"
}

# Instruction trace command
cmd_trace() {
    local HEX_FILE="scripts/hex/bootload.hex"
    local OUTPUT_FILE="scripts/test_reports/instruction_trace.txt"
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --hex)
                HEX_FILE="$2"
                shift 2
                ;;
            --output|-o)
                OUTPUT_FILE="$2"
                shift 2
                ;;
            --help|-h)
                echo "Usage: $0 trace [options]"
                echo ""
                echo "Run instruction trace simulation"
                echo ""
                echo "Options:"
                echo "  --hex <file>      Hex file to trace (default: bootload.hex)"
                echo "  --output <file>   Output trace file"
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                exit 1
                ;;
        esac
    done
    
    echo -e "${BLUE}Running Instruction Trace${NC}"
    echo "========================="
    echo "Input:  $HEX_FILE"
    echo "Output: $OUTPUT_FILE"
    echo ""
    
    # Ensure output directory exists
    mkdir -p $(dirname "$OUTPUT_FILE")
    
    # Run trace simulation
    if [ -f "src/test/scala/transputer/BootloadTraceSim.scala" ]; then
        sbt "test:runMain transputer.BootloadTraceSim $HEX_FILE $OUTPUT_FILE"
    else
        echo -e "${YELLOW}Creating trace simulator...${NC}"
        ./scripts/test_bootload_trace.sh
    fi
    
    echo -e "\n${GREEN}✓ Trace complete${NC}"
    echo "View trace: cat $OUTPUT_FILE"
}

# Waveform generation command
cmd_wave() {
    local HEX_FILE=""
    local CYCLES=1000
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --hex)
                HEX_FILE="$2"
                shift 2
                ;;
            --cycles)
                CYCLES="$2"
                shift 2
                ;;
            --help|-h)
                echo "Usage: $0 wave [options]"
                echo ""
                echo "Generate waveform simulation"
                echo ""
                echo "Options:"
                echo "  --hex <file>      Hex file to simulate"
                echo "  --cycles <n>      Number of cycles (default: 1000)"
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                exit 1
                ;;
        esac
    done
    
    if [ -z "$HEX_FILE" ]; then
        echo -e "${RED}Error: --hex <file> required${NC}"
        exit 1
    fi
    
    echo -e "${BLUE}Generating Waveform Simulation${NC}"
    echo "=============================="
    echo "Hex file: $HEX_FILE"
    echo "Cycles:   $CYCLES"
    echo ""
    
    # Disable PCH to avoid compilation issues
    export VERILATOR_OPT_FAST="-O2"
    export VERILATOR_OPT_SLOW="-O0"
    
    # Run simulation with waveform
    sbt "runMain transputer.GenerateWithTest --hex $HEX_FILE --wave --cycles $CYCLES"
    
    echo -e "\n${GREEN}✓ Simulation complete${NC}"
    echo "View waveform: gtkwave simWorkspace/Transputer/*.vcd"
}

# Clean command
cmd_clean() {
    echo -e "${BLUE}Cleaning Build Artifacts${NC}"
    echo "========================"
    
    echo "Cleaning simulation workspace..."
    rm -rf simWorkspace/
    
    echo "Cleaning SBT target directories..."
    rm -rf target/
    rm -rf project/target/
    
    echo "Cleaning generated Verilog..."
    rm -f generated/*.v
    
    echo "Cleaning test reports..."
    rm -f scripts/test_reports/*.txt
    rm -f scripts/test_reports/*.md
    
    echo -e "${GREEN}✓ Clean complete${NC}"
}

# Verilator fix command
cmd_verilator_fix() {
    echo -e "${BLUE}Fixing Verilator PCH Issues${NC}"
    echo "==========================="
    
    # Set environment variables
    export VERILATOR_OPT_FAST="-O2"
    export VERILATOR_OPT_SLOW="-O0"
    export OBJCACHE=""
    
    echo "Environment variables set:"
    echo "  VERILATOR_OPT_FAST=-O2"
    echo "  VERILATOR_OPT_SLOW=-O0"
    echo "  OBJCACHE=(disabled)"
    
    # Check if makefile exists to patch
    if [ -f "simWorkspace/Transputer/verilator/VTransputer.mk" ]; then
        echo -e "\n${YELLOW}Patching existing Verilator makefile...${NC}"
        ./scripts/patch_verilator_makefile.sh
    fi
    
    echo -e "\n${GREEN}✓ Verilator fixes applied${NC}"
    echo "You can now run simulations without PCH errors"
}

# Info command
cmd_info() {
    echo -e "${BLUE}T9000 Transputer Project Information${NC}"
    echo "===================================="
    
    # Check for generated Verilog
    echo -e "\n${CYAN}Generated Verilog Files:${NC}"
    if [ -d "generated" ]; then
        for file in generated/*.v; do
            if [ -f "$file" ]; then
                size=$(wc -l < "$file")
                echo -e "  ✅ $(basename $file) (${size} lines)"
            fi
        done
    else
        echo "  No Verilog files generated yet"
    fi
    
    # Check for test reports
    echo -e "\n${CYAN}Recent Test Reports:${NC}"
    if [ -d "scripts/test_reports" ]; then
        recent_reports=$(find scripts/test_reports -name "*.txt" -o -name "*.md" | head -5)
        if [ -n "$recent_reports" ]; then
            echo "$recent_reports" | while read report; do
                echo "  📄 $(basename $report)"
            done
        else
            echo "  No test reports found"
        fi
    fi
    
    # Check for assembly files
    echo -e "\n${CYAN}Example Assembly Files:${NC}"
    if [ -d "scripts/asm" ]; then
        count=$(ls -1 scripts/asm/*.asm 2>/dev/null | wc -l)
        echo "  Found $count assembly files in scripts/asm/"
    fi
    
    # Plugin count
    echo -e "\n${CYAN}T9000 Configuration:${NC}"
    if [ -f "src/main/scala/transputer/T9000Param.scala" ]; then
        plugin_count=$(grep -c "Plugin()" src/main/scala/transputer/T9000Param.scala || echo "0")
        echo "  Instruction table plugins: $plugin_count"
    fi
    
    echo -e "\n${CYAN}Quick Commands:${NC}"
    echo "  Build T9000:     ./scripts/build.sh"
    echo "  Run tests:       ./scripts/test.sh"
    echo "  Assemble code:   ./scripts/assemble.sh <file.asm>"
    echo "  View pipeline:   ./scripts/utils.sh konata --demo"
}

# Main command dispatcher
if [ $# -eq 0 ]; then
    usage
fi

COMMAND=$1
shift

case $COMMAND in
    konata)
        cmd_konata "$@"
        ;;
    trace)
        cmd_trace "$@"
        ;;
    wave)
        cmd_wave "$@"
        ;;
    clean)
        cmd_clean "$@"
        ;;
    verilator-fix)
        cmd_verilator_fix "$@"
        ;;
    info)
        cmd_info "$@"
        ;;
    --help|-h)
        usage
        ;;
    *)
        echo -e "${RED}Unknown command: $COMMAND${NC}"
        usage
        ;;
esac

exit 0
#!/bin/bash

#
# T9000 Transputer Assembly Script
# Assembles transputer assembly files and generates hex files
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
INPUT_FILE=""
OUTPUT_FILE=""
FORMAT="hex"
LIST_EXAMPLES=false
ASSEMBLER="transputer"

# Function to display usage
usage() {
    echo "Usage: $0 [OPTIONS] <input_file>"
    echo ""
    echo "Assemble transputer assembly files"
    echo ""
    echo "Options:"
    echo "  -o, --output <file>    Output file name (default: auto-generated)"
    echo "  -f, --format <fmt>     Output format: hex|binary|bootload (default: hex)"
    echo "  -a, --assembler <asm>  Assembler: transputer|inmos (default: transputer)"
    echo "  -l, --list             List example assembly files"
    echo "  -h, --help             Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0 scripts/asm/hello_world.asm           # Assemble to hex"
    echo "  $0 -o boot.hex scripts/asm/bootload.asm  # Custom output name"
    echo "  $0 --format binary program.asm           # Binary output"
    echo "  $0 --list                                # List available examples"
    exit 0
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -o|--output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        -f|--format)
            FORMAT="$2"
            shift 2
            ;;
        -a|--assembler)
            ASSEMBLER="$2"
            shift 2
            ;;
        -l|--list)
            LIST_EXAMPLES=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        -*)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
        *)
            INPUT_FILE="$1"
            shift
            ;;
    esac
done

echo -e "${BLUE}==========================================${NC}"
echo -e "${BLUE}T9000 Transputer Assembler${NC}"
echo -e "${BLUE}==========================================${NC}"

# Handle list examples
if [ "$LIST_EXAMPLES" = true ]; then
    echo -e "\n${YELLOW}Available example assembly files:${NC}"
    echo ""
    
    if [ -d "scripts/asm" ]; then
        for file in scripts/asm/*.asm; do
            if [ -f "$file" ]; then
                basename=$(basename "$file")
                # Extract description from file if available
                desc=$(grep -m1 "^;\s*Description:" "$file" 2>/dev/null | sed 's/^.*Description:\s*//' || echo "")
                if [ -z "$desc" ]; then
                    # Try to get title or first comment
                    desc=$(grep -m1 "^;\s*[A-Z]" "$file" 2>/dev/null | sed 's/^;\s*//' || echo "No description")
                fi
                printf "  %-30s %s\n" "$basename" "$desc"
            fi
        done
    else
        echo -e "  ${RED}No example files found in scripts/asm/${NC}"
    fi
    
    echo ""
    echo "To assemble an example:"
    echo "  $0 scripts/asm/hello_world.asm"
    exit 0
fi

# Check if input file provided
if [ -z "$INPUT_FILE" ]; then
    echo -e "${RED}Error: No input file specified${NC}"
    echo ""
    usage
fi

# Check if input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo -e "${RED}Error: Input file not found: $INPUT_FILE${NC}"
    exit 1
fi

# Generate output filename if not specified
if [ -z "$OUTPUT_FILE" ]; then
    BASE_NAME=$(basename "$INPUT_FILE" .asm)
    case $FORMAT in
        hex)
            OUTPUT_FILE="${BASE_NAME}.hex"
            ;;
        binary)
            OUTPUT_FILE="${BASE_NAME}.bin"
            ;;
        bootload)
            OUTPUT_FILE="${BASE_NAME}_boot.hex"
            ;;
    esac
fi

# Create output directories if needed
OUTPUT_DIR=$(dirname "$OUTPUT_FILE")
if [ "$OUTPUT_DIR" != "." ] && [ ! -d "$OUTPUT_DIR" ]; then
    mkdir -p "$OUTPUT_DIR"
fi

echo "Input file:  $INPUT_FILE"
echo "Output file: $OUTPUT_FILE"
echo "Format:      $FORMAT"
echo "Assembler:   $ASSEMBLER"
echo ""

# Function to assemble with TransputerAssembler
assemble_transputer() {
    local input="$1"
    local output="$2"
    local format="$3"
    
    echo -e "${YELLOW}Assembling with TransputerAssembler...${NC}"
    
    case $format in
        hex)
            sbt "runMain transputer.TransputerAssembler $input $output"
            ;;
        bootload)
            sbt "runMain transputer.TransputerAssembler --bootload $input $output"
            ;;
        binary)
            # First generate hex, then convert to binary
            local temp_hex="${output}.tmp"
            sbt "runMain transputer.TransputerAssembler $input $temp_hex"
            if [ -f "$temp_hex" ]; then
                # Convert hex to binary (simple implementation)
                echo "Converting to binary format..."
                # This would need a proper hex2bin converter
                echo -e "${YELLOW}Note: Binary output not yet implemented${NC}"
                mv "$temp_hex" "$output"
            fi
            ;;
    esac
}

# Function to handle special INMOS assembly files
assemble_inmos() {
    local input="$1"
    local output="$2"
    
    echo -e "${YELLOW}Processing INMOS-style assembly...${NC}"
    
    # Check if it's hello-iserver.asm which needs special handling
    if [[ "$input" == *"hello-iserver.asm"* ]]; then
        echo "Note: hello-iserver.asm requires forward reference resolution"
        # Could implement special handling here
    fi
    
    # For now, use the standard assembler
    assemble_transputer "$input" "$output" "hex"
}

# Main assembly process
case $ASSEMBLER in
    transputer)
        assemble_transputer "$INPUT_FILE" "$OUTPUT_FILE" "$FORMAT"
        ;;
    inmos)
        assemble_inmos "$INPUT_FILE" "$OUTPUT_FILE"
        ;;
    *)
        echo -e "${RED}Unknown assembler: $ASSEMBLER${NC}"
        exit 1
        ;;
esac

# Check if output was created
if [ -f "$OUTPUT_FILE" ]; then
    echo -e "\n${GREEN}✅ Assembly successful!${NC}"
    echo -e "Output file: ${GREEN}$OUTPUT_FILE${NC}"
    
    # Display file info
    FILE_SIZE=$(wc -c < "$OUTPUT_FILE")
    echo "File size: $FILE_SIZE bytes"
    
    # For hex files, show first few lines
    if [ "$FORMAT" = "hex" ]; then
        echo -e "\nFirst 5 lines of output:"
        head -5 "$OUTPUT_FILE" | sed 's/^/  /'
    fi
else
    echo -e "\n${RED}❌ Assembly failed!${NC}"
    echo "Output file was not created"
    exit 1
fi

echo -e "\n${YELLOW}Next steps:${NC}"
echo "  1. Use the hex file with simulation: sbt \"runMain transputer.GenerateWithTest --hex $OUTPUT_FILE\""
echo "  2. Generate Konata visualization: ./scripts/utils.sh konata --hex $OUTPUT_FILE"
echo "  3. View waveforms if simulation was run with --wave flag"
echo ""

exit 0
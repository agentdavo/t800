#!/bin/bash

#
# T9000 Transputer Build Script
# Generates various T9000 configurations with all enhanced plugins
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
OUTPUT_DIR="./generated/t9000"
mkdir -p "$OUTPUT_DIR"

echo -e "\n${GREEN}📁 Output directory: $OUTPUT_DIR${NC}"

# Base parameters
BASE_PARAMS="--output-dir=$OUTPUT_DIR"

echo -e "\n${YELLOW}🏗️  Building T9000 configurations...${NC}"

# 1. Standard T9000 Configuration
echo -e "\n${GREEN}1. Standard T9000 Transputer${NC}"
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --report-model \
  --report-compliance \
  || echo -e "${RED}Standard build failed${NC}"

# 2. High-Performance Configuration
echo -e "\n${GREEN}2. High-Performance T9000${NC}" 
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --main-cache-kb=32 \
  --workspace-cache-words=64 \
  --scheduler-queue-depth=32 \
  --enable-profiling=true \
  --report-performance \
  || echo -e "${RED}Performance build failed${NC}"

# 3. Minimal Configuration (for testing)
echo -e "\n${GREEN}3. Minimal T9000 (Testing)${NC}"
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --main-cache-kb=4 \
  --workspace-cache-words=16 \
  --enable-vcp=false \
  --enable-pmi=false \
  --enable-profiling=false \
  || echo -e "${RED}Minimal build failed${NC}"

# 4. Debug Configuration
echo -e "\n${GREEN}4. Debug T9000${NC}"
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --enable-debug=true \
  --enable-profiling=true \
  --enable-test-framework=true \
  --report-model \
  || echo -e "${RED}Debug build failed${NC}"

# 5. Communication-focused Configuration
echo -e "\n${GREEN}5. Communication-focused T9000${NC}"
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --link-count=8 \
  --pmi-channels=8 \
  --pmi-devices-per-channel=16 \
  --spi-ddr-links=true \
  --scheduler-queue-depth=64 \
  || echo -e "${RED}Communication build failed${NC}"

# 6. Single-precision FPU Configuration
echo -e "\n${GREEN}6. Single-precision FPU T9000${NC}"
scala-cli run src/main/scala/transputer/T9000Generate.scala -- \
  $BASE_PARAMS \
  --fpu-precision=32 \
  --ieee754-compliance=true \
  || echo -e "${RED}Single-precision build failed${NC}"

echo -e "\n${BLUE}==========================================${NC}"
echo -e "${GREEN}✅ T9000 Transputer builds completed!${NC}"
echo -e "${BLUE}==========================================${NC}"

# List generated files
echo -e "\n${YELLOW}📄 Generated files:${NC}"
find "$OUTPUT_DIR" -name "*.v" -o -name "*.sv" | sort

# Generate configuration summary
echo -e "\n${YELLOW}📊 Configuration Summary:${NC}"
cat > "$OUTPUT_DIR/README.md" << EOF
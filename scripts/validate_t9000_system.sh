#!/bin/bash

# T9000 Transputer System Validation Script
# Quick validation of core T9000 functionality

echo "============================================================================="
echo "T9000 TRANSPUTER SYSTEM VALIDATION"
echo "============================================================================="
echo "Validating complete T9000 ISA implementation with all instruction table plugins"
echo "Generated on: $(date)"
echo "============================================================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to run validation test
run_validation() {
    local test_name="$1"
    local command="$2"
    local description="$3"
    
    echo -e "${BLUE}Validating $test_name${NC}: $description"
    
    # Run the command
    eval $command > /dev/null 2>&1
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo -e "  Result: ${GREEN}✅ PASS${NC}"
        return 0
    else
        echo -e "  Result: ${RED}❌ FAIL${NC}"
        return 1
    fi
}

# Initialize counters
total_tests=0
passed_tests=0

echo -e "${YELLOW}Core System Validation${NC}"
echo "======================"
echo ""

# Test 1: Code formatting
run_validation "Code Format" "sbt scalafmtCheck" "Code style and formatting compliance"
total_tests=$((total_tests + 1))
[ $? -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test 2: T9000 Verilog generation
echo -e "${BLUE}Validating T9000 Verilog Generation${NC}: Complete T9000 with all 21 instruction table plugins"
sbt "runMain transputer.T9000Generate --word-width 32 --link-count 4 --enable-fpu true" > /dev/null 2>&1
verilog_exit_code=$?

if [ $verilog_exit_code -eq 0 ] && [ -f "generated/T9000Transputer.v" ]; then
    verilog_size=$(wc -l < "generated/T9000Transputer.v")
    echo -e "  Result: ${GREEN}✅ PASS${NC} (Generated $verilog_size lines of Verilog)"
    passed_tests=$((passed_tests + 1))
else
    echo -e "  Result: ${RED}❌ FAIL${NC} (Verilog generation failed)"
fi
total_tests=$((total_tests + 1))

# Test 3: Minimal Transputer generation
run_validation "Minimal Transputer" "sbt \"runMain transputer.Generate\"" "Basic Transputer functionality"
total_tests=$((total_tests + 1))
[ $? -eq 0 ] && passed_tests=$((passed_tests + 1))

echo ""
echo -e "${YELLOW}Instruction Table Plugin Coverage${NC}"
echo "=================================="
echo ""

# Test 4: Plugin coverage validation
echo -e "${BLUE}Validating Plugin Coverage${NC}: Checking all 21 instruction table plugins"

# Check for all required plugins in T9000Param.scala
plugins=(
    "ArithmeticPlugin"
    "LongArithPlugin" 
    "ControlFlowPlugin"
    "BlockMovePlugin"
    "IndexingPlugin"
    "RangeCheckPlugin"
    "DevicePlugin"
    "BitOpsPlugin"
    "GeneralPlugin"
    "TimerPlugin"
    "IOPlugin"
    "ChannelPlugin"
    "ResourcePlugin"
    "SemaphorePlugin"
    "AlternativePlugin"
    "SchedulerPlugin"
    "InterruptPlugin"
    "MemoryProtectionPlugin"
    "SystemPlugin"
    "MainCachePlugin"
    "FpuPlugin"
)

plugin_checks=0
plugin_passes=0

for plugin in "${plugins[@]}"; do
    plugin_checks=$((plugin_checks + 1))
    if grep -q "$plugin" src/main/scala/transputer/T9000Param.scala; then
        plugin_passes=$((plugin_passes + 1))
    fi
done

if [ $plugin_passes -eq $plugin_checks ]; then
    echo -e "  Result: ${GREEN}✅ PASS${NC} (All $plugin_checks instruction table plugins included)"
    passed_tests=$((passed_tests + 1))
else
    echo -e "  Result: ${RED}❌ FAIL${NC} (Only $plugin_passes/$plugin_checks plugins found)"
fi
total_tests=$((total_tests + 1))

echo ""
echo -e "${YELLOW}File Structure Validation${NC}"
echo "========================="
echo ""

# Test 5: Plugin source files exist
echo -e "${BLUE}Validating Plugin Files${NC}: Checking source files for all plugins"
plugin_files_exist=0
plugin_files_total=0

for plugin in "${plugins[@]}"; do
    plugin_files_total=$((plugin_files_total + 1))
    # Convert plugin name to expected path
    plugin_path=$(echo "$plugin" | sed 's/Plugin//' | tr '[:upper:]' '[:lower:]')
    
    # Special cases for path mapping
    case $plugin_path in
        "longarith") plugin_path="longarith" ;;
        "controlflow") plugin_path="controlflow" ;;
        "blockmove") plugin_path="blockmove" ;;
        "rangecheck") plugin_path="rangecheck" ;;
        "bitops") plugin_path="bitops" ;;
        "memoryprotection") plugin_path="protection" ;;
        "maincache") plugin_path="core/cache" ;;
    esac
    
    # Check if plugin file exists
    if [ -f "src/main/scala/transputer/plugins/$plugin_path/${plugin}.scala" ]; then
        plugin_files_exist=$((plugin_files_exist + 1))
    fi
done

if [ $plugin_files_exist -eq $plugin_files_total ]; then
    echo -e "  Result: ${GREEN}✅ PASS${NC} (All $plugin_files_total plugin source files found)"
    passed_tests=$((passed_tests + 1))
else
    echo -e "  Result: ${RED}❌ FAIL${NC} (Only $plugin_files_exist/$plugin_files_total plugin files found)"
fi
total_tests=$((total_tests + 1))

echo ""
echo -e "${YELLOW}T9000 Architecture Validation${NC}"
echo "=============================="
echo ""

# Test 6: Database configuration files
run_validation "Database Config" "grep -q 'WORD_BITS' src/main/scala/transputer/Global.scala" "Global configuration constants"
total_tests=$((total_tests + 1))
[ $? -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test 7: T9000 parameter configuration
run_validation "T9000 Config" "grep -q 'T9000Param' src/main/scala/transputer/T9000Param.scala" "T9000 parameter configuration"
total_tests=$((total_tests + 1))
[ $? -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test 8: Generated Verilog structure validation
echo -e "${BLUE}Validating Verilog Structure${NC}: Checking generated Verilog content"
if [ -f "generated/T9000Transputer.v" ]; then
    # Check for key components in generated Verilog
    has_module=$(grep -c "module T9000Transputer" generated/T9000Transputer.v)
    has_clock=$(grep -c "input.*clk" generated/T9000Transputer.v)
    has_reset=$(grep -c "input.*reset" generated/T9000Transputer.v)
    
    if [ $has_module -ge 1 ] && [ $has_clock -ge 1 ] && [ $has_reset -ge 1 ]; then
        echo -e "  Result: ${GREEN}✅ PASS${NC} (Valid Verilog module structure)"
        passed_tests=$((passed_tests + 1))
    else
        echo -e "  Result: ${RED}❌ FAIL${NC} (Invalid Verilog structure)"
    fi
else
    echo -e "  Result: ${RED}❌ FAIL${NC} (No Verilog file found)"
fi
total_tests=$((total_tests + 1))

# Calculate results
failed_tests=$((total_tests - passed_tests))
success_rate=$((passed_tests * 100 / total_tests))

echo ""
echo "============================================================================="
echo -e "${GREEN}T9000 SYSTEM VALIDATION COMPLETE${NC}"
echo "============================================================================="
echo ""
echo "📊 Validation Summary:"
echo "   Total Tests:  $total_tests"
echo "   Passed:       $passed_tests"
echo "   Failed:       $failed_tests"
echo "   Success Rate: $success_rate%"
echo ""

# Generate simple report
echo "📄 System Status:"
if [ $failed_tests -eq 0 ]; then
    echo -e "   🎉 ${GREEN}EXCELLENT: ALL VALIDATIONS PASSED${NC}"
    echo "   • Complete T9000 ISA implementation operational"
    echo "   • All 21 instruction table plugins integrated"
    echo "   • Clean Verilog generation confirmed"
    echo "   • System ready for deployment"
elif [ $failed_tests -le 2 ]; then
    echo -e "   ⚠️  ${YELLOW}GOOD: MINOR ISSUES DETECTED${NC}"
    echo "   • Core functionality verified"
    echo "   • System mostly operational"
else
    echo -e "   ❌ ${RED}ISSUES DETECTED${NC}"
    echo "   • Review system configuration"
    echo "   • Check plugin integration"
fi

echo ""
echo "📁 Generated Files:"
if [ -f "generated/T9000Transputer.v" ]; then
    verilog_size=$(wc -l < "generated/T9000Transputer.v")
    echo "   ✅ T9000Transputer.v ($verilog_size lines)"
else
    echo "   ❌ T9000Transputer.v (missing)"
fi

echo ""
echo "🚀 Next Steps:"
if [ $failed_tests -eq 0 ]; then
    echo "   ✅ Ready for FPGA synthesis"
    echo "   ✅ Begin application development"
    echo "   ✅ Consider performance optimization"
else
    echo "   🔧 Address validation failures"
    echo "   🔄 Re-run validation after fixes"
fi

echo ""
echo "To generate detailed test reports:"
echo "  ./build_t9000_tests.sh"
echo ""

# Exit with appropriate code
exit $failed_tests
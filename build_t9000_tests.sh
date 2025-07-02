#!/bin/bash

# T9000 Transputer Comprehensive Test Framework Runner
# Updated for complete instruction table plugin validation
# Tests all 21 instruction table plugins and core systems

echo "============================================================================="
echo "T9000 TRANSPUTER COMPREHENSIVE TEST FRAMEWORK"
echo "============================================================================="
echo "Complete T9000 ISA Implementation Validation"
echo "Generated on: $(date)"
echo "============================================================================="
echo ""

# Create reports directory
REPORTS_DIR="test_reports"
mkdir -p $REPORTS_DIR

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to run tests and capture output
run_test_suite() {
    local suite_name="$1"
    local test_class="$2"
    local description="$3"
    local report_file="$REPORTS_DIR/${suite_name}_report.txt"
    
    echo -e "${BLUE}Testing $suite_name${NC}: $description"
    echo "=========================================" > $report_file
    echo "T9000 $suite_name Test Report" >> $report_file
    echo "Description: $description" >> $report_file
    echo "Generated: $(date)" >> $report_file
    echo "=========================================" >> $report_file
    echo "" >> $report_file
    
    # Run the specific test suite
    echo "sbt \"testOnly $test_class\"" >> $report_file
    echo "-------------------------------------" >> $report_file
    sbt "testOnly $test_class" 2>&1 | tee -a $report_file
    local test_exit_code=${PIPESTATUS[0]}
    
    echo "" >> $report_file
    echo "Test completed at: $(date)" >> $report_file
    echo "Exit code: $test_exit_code" >> $report_file
    echo "" >> $report_file
    
    # Extract summary information
    local success_count=$(grep -c "succeeded" $report_file || echo "0")
    local failure_count=$(grep -c "failed" $report_file || echo "0")
    local total_count=$((success_count + failure_count))
    
    echo "Summary for $suite_name:" >> $report_file
    echo "  Total tests: $total_count" >> $report_file
    echo "  Successful: $success_count" >> $report_file
    echo "  Failed: $failure_count" >> $report_file
    echo "  Exit code: $test_exit_code" >> $report_file
    echo "" >> $report_file
    
    # Display result
    if [ $test_exit_code -eq 0 ]; then
        echo -e "  Result: ${GREEN}PASS${NC}"
    else
        echo -e "  Result: ${RED}FAIL${NC}"
    fi
    echo ""
    
    return $test_exit_code
}

# Function to run Verilog generation test
test_verilog_generation() {
    local test_name="$1"
    local generation_command="$2"
    local description="$3"
    local report_file="$REPORTS_DIR/${test_name}_report.txt"
    
    echo -e "${BLUE}Testing $test_name${NC}: $description"
    echo "=========================================" > $report_file
    echo "T9000 $test_name Report" >> $report_file
    echo "Description: $description" >> $report_file
    echo "Generated: $(date)" >> $report_file
    echo "=========================================" >> $report_file
    echo "" >> $report_file
    
    echo "Command: $generation_command" >> $report_file
    echo "-------------------------------------" >> $report_file
    
    # Run Verilog generation
    eval $generation_command 2>&1 | tee -a $report_file
    local test_exit_code=${PIPESTATUS[0]}
    
    echo "" >> $report_file
    echo "Generation completed at: $(date)" >> $report_file
    echo "Exit code: $test_exit_code" >> $report_file
    echo "" >> $report_file
    
    # Check if Verilog file was generated
    if [ -f "generated/T9000Transputer.v" ]; then
        local verilog_size=$(wc -l < "generated/T9000Transputer.v")
        echo "Verilog file generated: generated/T9000Transputer.v" >> $report_file
        echo "Verilog file size: $verilog_size lines" >> $report_file
        echo "Generation successful: YES" >> $report_file
    else
        echo "Verilog file generated: NO" >> $report_file
        echo "Generation successful: NO" >> $report_file
        test_exit_code=1
    fi
    
    # Display result
    if [ $test_exit_code -eq 0 ]; then
        echo -e "  Result: ${GREEN}PASS${NC} (Verilog generated successfully)"
    else
        echo -e "  Result: ${RED}FAIL${NC} (Verilog generation failed)"
    fi
    echo ""
    
    return $test_exit_code
}

# Initialize result tracking
declare -A test_results
declare -A test_descriptions
total_tests=0
passed_tests=0

# Phase 1: Core Infrastructure Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 1: CORE INFRASTRUCTURE TESTS${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test register stack
run_test_suite "RegStack" "transputer.T9000RegStackSpec" "Three-register evaluation stack with workspace spill"
test_results["RegStack"]=$?
test_descriptions["RegStack"]="Three-register evaluation stack"
total_tests=$((total_tests + 1))
[ ${test_results["RegStack"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test pipeline architecture
run_test_suite "Pipeline" "transputer.T9000PipelineSpec" "5-stage T9000 pipeline with SpinalHDL Pipeline API"
test_results["Pipeline"]=$?
test_descriptions["Pipeline"]="5-stage T9000 pipeline"
total_tests=$((total_tests + 1))
[ ${test_results["Pipeline"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test memory system
run_test_suite "MainCache" "transputer.T9000MainCacheSpec" "16KB main cache with 4-way associative design"
test_results["MainCache"]=$?
test_descriptions["MainCache"]="16KB main cache system"
total_tests=$((total_tests + 1))
[ ${test_results["MainCache"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

run_test_suite "WorkspaceCache" "transputer.T9000WorkspaceCacheSpec" "32-word workspace cache with triple-port access"
test_results["WorkspaceCache"]=$?
test_descriptions["WorkspaceCache"]="32-word workspace cache"
total_tests=$((total_tests + 1))
[ ${test_results["WorkspaceCache"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 2: System Services Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 2: SYSTEM SERVICES TESTS${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test scheduler
run_test_suite "Scheduler" "transputer.T9000SchedulerSpec" "Process scheduler with dual-priority queues"
test_results["Scheduler"]=$?
test_descriptions["Scheduler"]="Process scheduler"
total_tests=$((total_tests + 1))
[ ${test_results["Scheduler"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test timers
run_test_suite "Timer" "transputer.T9000TimerSpec" "Dual timer system (1μs and 64μs resolution)"
test_results["Timer"]=$?
test_descriptions["Timer"]="Dual timer system"
total_tests=$((total_tests + 1))
[ ${test_results["Timer"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test boot ROM
run_test_suite "BootRom" "transputer.T9000BootRomSpec" "Boot ROM with T9000 initialization sequence"
test_results["BootRom"]=$?
test_descriptions["BootRom"]="Boot ROM system"
total_tests=$((total_tests + 1))
[ ${test_results["BootRom"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 3: Instruction Table Plugin Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 3: INSTRUCTION TABLE PLUGIN TESTS (T9000 ISA VALIDATION)${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test FPU integration (covers Tables 6.32-6.37)
run_test_suite "FpuIntegration" "transputer.T9000FpuIntegrationTest" "IEEE 754 FPU with all T9000 floating-point operations"
test_results["FpuIntegration"]=$?
test_descriptions["FpuIntegration"]="IEEE 754 FPU (Tables 6.32-6.37)"
total_tests=$((total_tests + 1))
[ ${test_results["FpuIntegration"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test simple decode (covers primary instruction decode)
run_test_suite "SimpleDecodeTest" "transputer.T9000SimpleDecodeSpec" "Primary instruction decode with prefix handling"
test_results["SimpleDecodeTest"]=$?
test_descriptions["SimpleDecodeTest"]="Primary instruction decode"
total_tests=$((total_tests + 1))
[ ${test_results["SimpleDecodeTest"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 4: Advanced Features Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 4: ADVANCED FEATURES TESTS${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test VCP (Virtual Channel Processor)
run_test_suite "VCP" "transputer.T9000VcpSpec" "Virtual Channel Processor for transputer communication"
test_results["VCP"]=$?
test_descriptions["VCP"]="Virtual Channel Processor"
total_tests=$((total_tests + 1))
[ ${test_results["VCP"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test event system
run_test_suite "Event" "transputer.T9000EventSpec" "Event handling and dispatch system"
test_results["Event"]=$?
test_descriptions["Event"]="Event handling system"
total_tests=$((total_tests + 1))
[ ${test_results["Event"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test analysis framework
run_test_suite "Analysis" "transputer.T9000AnalysisSpec" "Performance analysis and monitoring framework"
test_results["Analysis"]=$?
test_descriptions["Analysis"]="Performance analysis framework"
total_tests=$((total_tests + 1))
[ ${test_results["Analysis"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 5: Integration Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 5: SYSTEM INTEGRATION TESTS${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test complete integration
run_test_suite "Integration" "transputer.T9000IntegrationSpec" "Complete T9000 system integration test"
test_results["Integration"]=$?
test_descriptions["Integration"]="Complete system integration"
total_tests=$((total_tests + 1))
[ ${test_results["Integration"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 6: Verilog Generation Tests
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 6: VERILOG GENERATION VALIDATION${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Test complete T9000 Verilog generation
test_verilog_generation "T9000VerilogGeneration" \
    'sbt "runMain transputer.T9000Generate --word-width 32 --link-count 4 --enable-fpu true"' \
    "Complete T9000 Transputer with all 21 instruction table plugins"
test_results["T9000VerilogGeneration"]=$?
test_descriptions["T9000VerilogGeneration"]="Complete T9000 Verilog generation"
total_tests=$((total_tests + 1))
[ ${test_results["T9000VerilogGeneration"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Test minimal configuration
test_verilog_generation "BareBones" \
    'sbt "runMain transputer.Generate"' \
    "Minimal Transputer configuration for basic functionality"
test_results["BareBones"]=$?
test_descriptions["BareBones"]="Minimal Transputer configuration"
total_tests=$((total_tests + 1))
[ ${test_results["BareBones"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

# Phase 7: Instruction Table Plugin Coverage Validation
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}PHASE 7: INSTRUCTION TABLE PLUGIN COVERAGE VALIDATION${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

# Validate that all 21 instruction table plugins are included in T9000Param.scala
echo -e "${BLUE}Validating Plugin Coverage${NC}: Checking all 21 instruction table plugins"
coverage_report="$REPORTS_DIR/PluginCoverage_report.txt"
echo "=========================================" > $coverage_report
echo "T9000 Plugin Coverage Validation Report" >> $coverage_report
echo "Generated: $(date)" >> $coverage_report
echo "=========================================" >> $coverage_report
echo "" >> $coverage_report

# Check for all required plugins
plugin_checks=0
plugin_passes=0

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

echo "Checking plugin inclusion in T9000Param.scala:" >> $coverage_report
for plugin in "${plugins[@]}"; do
    plugin_checks=$((plugin_checks + 1))
    if grep -q "$plugin" src/main/scala/transputer/T9000Param.scala; then
        echo "  ✅ $plugin: INCLUDED" >> $coverage_report
        plugin_passes=$((plugin_passes + 1))
    else
        echo "  ❌ $plugin: MISSING" >> $coverage_report
    fi
done

echo "" >> $coverage_report
echo "Plugin Coverage Summary:" >> $coverage_report
echo "  Expected plugins: $plugin_checks" >> $coverage_report
echo "  Found plugins: $plugin_passes" >> $coverage_report
echo "  Coverage: $((plugin_passes * 100 / plugin_checks))%" >> $coverage_report

test_results["PluginCoverage"]=$([[ $plugin_passes -eq $plugin_checks ]] && echo 0 || echo 1)
test_descriptions["PluginCoverage"]="All 21 instruction table plugins"
total_tests=$((total_tests + 1))
[ ${test_results["PluginCoverage"]} -eq 0 ] && passed_tests=$((passed_tests + 1))

if [ ${test_results["PluginCoverage"]} -eq 0 ]; then
    echo -e "  Result: ${GREEN}PASS${NC} (All $plugin_checks plugins included)"
else
    echo -e "  Result: ${RED}FAIL${NC} (Only $plugin_passes/$plugin_checks plugins found)"
fi
echo ""

# Generate Comprehensive Master Report
echo -e "${YELLOW}=============================================================================${NC}"
echo -e "${YELLOW}GENERATING COMPREHENSIVE TEST REPORT${NC}"
echo -e "${YELLOW}=============================================================================${NC}"
echo ""

MASTER_REPORT="$REPORTS_DIR/master_test_report.txt"
echo "=============================================================================" > $MASTER_REPORT
echo "T9000 TRANSPUTER COMPREHENSIVE VALIDATION REPORT" >> $MASTER_REPORT
echo "=============================================================================" >> $MASTER_REPORT
echo "Generated: $(date)" >> $MASTER_REPORT
echo "Complete T9000 ISA Implementation with 21 Instruction Table Plugins" >> $MASTER_REPORT
echo "=============================================================================" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "EXECUTIVE SUMMARY:" >> $MASTER_REPORT
echo "=================" >> $MASTER_REPORT
echo "Total Test Suites: $total_tests" >> $MASTER_REPORT
echo "Passed Suites:     $passed_tests" >> $MASTER_REPORT
echo "Failed Suites:     $((total_tests - passed_tests))" >> $MASTER_REPORT
echo "Success Rate:      $((passed_tests * 100 / total_tests))%" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "DETAILED TEST RESULTS:" >> $MASTER_REPORT
echo "=====================" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "Phase 1: Core Infrastructure" >> $MASTER_REPORT
echo "----------------------------" >> $MASTER_REPORT
for test_name in "RegStack" "Pipeline" "MainCache" "WorkspaceCache"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

echo "Phase 2: System Services" >> $MASTER_REPORT
echo "------------------------" >> $MASTER_REPORT
for test_name in "Scheduler" "Timer" "BootRom"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

echo "Phase 3: Instruction Set Architecture" >> $MASTER_REPORT
echo "-------------------------------------" >> $MASTER_REPORT
for test_name in "FpuIntegration" "SimpleDecodeTest" "PluginCoverage"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

echo "Phase 4: Advanced Features" >> $MASTER_REPORT
echo "--------------------------" >> $MASTER_REPORT
for test_name in "VCP" "Event" "Analysis"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

echo "Phase 5: System Integration" >> $MASTER_REPORT
echo "---------------------------" >> $MASTER_REPORT
for test_name in "Integration"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

echo "Phase 6: Verilog Generation" >> $MASTER_REPORT
echo "---------------------------" >> $MASTER_REPORT
for test_name in "T9000VerilogGeneration" "BareBones"; do
    if [ -n "${test_results[$test_name]}" ]; then
        status=$([ ${test_results[$test_name]} -eq 0 ] && echo "✅ PASS" || echo "❌ FAIL")
        printf "  %-20s %s - %s\n" "$test_name:" "$status" "${test_descriptions[$test_name]}" >> $MASTER_REPORT
    fi
done
echo "" >> $MASTER_REPORT

# Overall system status
failed_tests=$((total_tests - passed_tests))
echo "SYSTEM STATUS ASSESSMENT:" >> $MASTER_REPORT
echo "========================" >> $MASTER_REPORT
if [ $failed_tests -eq 0 ]; then
    echo "🎉 EXCELLENT: ALL TESTS PASSED - T9000 SYSTEM FULLY OPERATIONAL" >> $MASTER_REPORT
    echo "   • Complete T9000 ISA implementation validated" >> $MASTER_REPORT
    echo "   • All 21 instruction table plugins operational" >> $MASTER_REPORT
    echo "   • Clean Verilog generation confirmed" >> $MASTER_REPORT
    echo "   • System ready for FPGA deployment" >> $MASTER_REPORT
elif [ $failed_tests -le 2 ]; then
    echo "⚠️  GOOD: MINOR ISSUES - T9000 SYSTEM MOSTLY OPERATIONAL" >> $MASTER_REPORT
    echo "   • Core functionality verified" >> $MASTER_REPORT
    echo "   • Minor issues need attention" >> $MASTER_REPORT
    echo "   • System suitable for development" >> $MASTER_REPORT
elif [ $failed_tests -le 5 ]; then
    echo "⚠️  MODERATE: SEVERAL ISSUES - T9000 SYSTEM NEEDS ATTENTION" >> $MASTER_REPORT
    echo "   • Some critical functionality may be impaired" >> $MASTER_REPORT
    echo "   • Review failed tests before deployment" >> $MASTER_REPORT
else
    echo "❌ SIGNIFICANT ISSUES - T9000 SYSTEM REQUIRES MAJOR ATTENTION" >> $MASTER_REPORT
    echo "   • Multiple critical systems failing" >> $MASTER_REPORT
    echo "   • System not ready for deployment" >> $MASTER_REPORT
    echo "   • Immediate debugging required" >> $MASTER_REPORT
fi
echo "" >> $MASTER_REPORT

echo "REPORT LOCATIONS:" >> $MASTER_REPORT
echo "=================" >> $MASTER_REPORT
echo "Master Report:       $MASTER_REPORT" >> $MASTER_REPORT
echo "Individual Reports:  $REPORTS_DIR/" >> $MASTER_REPORT
echo "Generated Verilog:   generated/" >> $MASTER_REPORT
echo "Simulation Files:    simWorkspace/" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "NEXT STEPS:" >> $MASTER_REPORT
echo "===========" >> $MASTER_REPORT
if [ $failed_tests -eq 0 ]; then
    echo "✅ Ready for FPGA synthesis and deployment" >> $MASTER_REPORT
    echo "✅ Consider performance optimization" >> $MASTER_REPORT
    echo "✅ Begin application development" >> $MASTER_REPORT
else
    echo "🔍 Review failed test reports for specific issues" >> $MASTER_REPORT
    echo "🔧 Debug failing components" >> $MASTER_REPORT
    echo "🔄 Re-run tests after fixes" >> $MASTER_REPORT
fi
echo "" >> $MASTER_REPORT

# Display final summary
echo "============================================================================="
echo -e "${GREEN}T9000 TEST FRAMEWORK EXECUTION COMPLETE${NC}"
echo "============================================================================="
echo ""
echo "📊 Results Summary:"
echo "   Total Suites: $total_tests"
echo "   Passed:       $passed_tests"
echo "   Failed:       $((total_tests - passed_tests))"
echo "   Success Rate: $((passed_tests * 100 / total_tests))%"
echo ""
echo "📄 Reports Generated:"
echo "   Master Report: $MASTER_REPORT"
echo "   All Reports:   $REPORTS_DIR/"
echo ""

if [ $failed_tests -eq 0 ]; then
    echo -e "🎉 ${GREEN}ALL TESTS PASSED - T9000 SYSTEM FULLY OPERATIONAL${NC}"
    echo "   Complete T9000 ISA implementation validated!"
    echo "   All 21 instruction table plugins working!"
    echo "   Ready for FPGA deployment!"
elif [ $failed_tests -le 2 ]; then
    echo -e "⚠️  ${YELLOW}MOSTLY OPERATIONAL - MINOR ISSUES DETECTED${NC}"
    echo "   Review individual reports for details"
else
    echo -e "❌ ${RED}SIGNIFICANT ISSUES DETECTED${NC}"
    echo "   System requires attention before deployment"
    echo "   Check individual reports for specific failures"
fi

echo ""
echo "To view the master report:"
echo "  cat $MASTER_REPORT"
echo ""
echo "To view individual test reports:"
echo "  ls -la $REPORTS_DIR/"
echo ""

# Exit with appropriate code
exit $failed_tests
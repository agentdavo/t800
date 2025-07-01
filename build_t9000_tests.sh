#!/bin/bash

# T9000 Transputer Test Framework Runner with Comprehensive Reporting
# This script runs all tests and generates detailed reports

echo "==========================================="
echo "T9000 Transputer Test Framework Report"
echo "==========================================="
echo "Generated on: $(date)"
echo ""

# Create reports directory
REPORTS_DIR="test_reports"
mkdir -p $REPORTS_DIR

# Function to run tests and capture output
run_test_suite() {
    local suite_name="$1"
    local test_class="$2"
    local report_file="$REPORTS_DIR/${suite_name}_report.txt"
    
    echo "Running $suite_name tests..."
    echo "=========================================" > $report_file
    echo "T9000 $suite_name Test Report" >> $report_file
    echo "Generated: $(date)" >> $report_file
    echo "=========================================" >> $report_file
    echo "" >> $report_file
    
    # Run the specific test suite
    sbt "testOnly $test_class" 2>&1 | tee -a $report_file
    
    echo "" >> $report_file
    echo "Test completed at: $(date)" >> $report_file
    echo "" >> $report_file
    
    # Extract summary information
    local success_count=$(grep -c "succeeded" $report_file || echo "0")
    local failure_count=$(grep -c "failed" $report_file || echo "0")
    local total_count=$((success_count + failure_count))
    
    echo "Summary for $suite_name:" >> $report_file
    echo "  Total tests: $total_count" >> $report_file
    echo "  Successful: $success_count" >> $report_file
    echo "  Failed: $failure_count" >> $report_file
    echo "" >> $report_file
    
    return $failure_count
}

# Run individual test suites
echo "1. Running Working Legacy Tests..."
echo "================================="

# Run the working legacy tests first
run_test_suite "Stack" "transputer.T9000StackSpec"
STACK_RESULT=$?

run_test_suite "Timer" "transputer.T9000TimerSpec"
TIMER_RESULT=$?

run_test_suite "Scheduler" "transputer.T9000SchedulerSpec"
SCHEDULER_RESULT=$?

run_test_suite "MainCache" "transputer.T9000MainCacheSpec"
CACHE_RESULT=$?

run_test_suite "WorkspaceCache" "transputer.T9000WorkspaceCacheSpec"
WORKSPACE_RESULT=$?

run_test_suite "PMI" "transputer.T9000PmiSpec"
PMI_RESULT=$?

run_test_suite "BootRomFetch" "transputer.BootRomFetchSpec"
BOOTROM_RESULT=$?

echo ""
echo "2. Running New T9000 Plugin Tests..."
echo "===================================="

# Run new plugin tests
run_test_suite "VCP" "transputer.T9000VcpSpec"
VCP_RESULT=$?

run_test_suite "Event" "transputer.T9000EventSpec"
EVENT_RESULT=$?

run_test_suite "Analysis" "transputer.T9000AnalysisSpec"
ANALYSIS_RESULT=$?

echo ""
echo "3. Running Integration Tests..."
echo "==============================="

run_test_suite "Integration" "transputer.T9000IntegrationSpec"
INTEGRATION_RESULT=$?

# Generate master summary report
MASTER_REPORT="$REPORTS_DIR/master_test_report.txt"
echo "==========================================" > $MASTER_REPORT
echo "T9000 TRANSPUTER MASTER TEST REPORT" >> $MASTER_REPORT
echo "==========================================" >> $MASTER_REPORT
echo "Generated: $(date)" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "TEST SUITE RESULTS:" >> $MASTER_REPORT
echo "==================" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

# Summary of all test results
echo "Core T9000 Components:" >> $MASTER_REPORT
echo "  Stack Plugin:        $([ $STACK_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Timer Plugin:        $([ $TIMER_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Scheduler Plugin:    $([ $SCHEDULER_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Main Cache Plugin:   $([ $CACHE_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Workspace Cache:     $([ $WORKSPACE_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "Communication & Memory:" >> $MASTER_REPORT
echo "  VCP Plugin:          $([ $VCP_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  PMI Plugin:          $([ $PMI_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Boot ROM Fetch:      $([ $BOOTROM_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "Analysis & Debugging:" >> $MASTER_REPORT
echo "  Event Plugin:        $([ $EVENT_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "  Analysis Plugin:     $([ $ANALYSIS_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "System Integration:" >> $MASTER_REPORT
echo "  Integration Tests:   $([ $INTEGRATION_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

# Calculate overall statistics
TOTAL_FAILURES=$((STACK_RESULT + TIMER_RESULT + SCHEDULER_RESULT + CACHE_RESULT + WORKSPACE_RESULT + PMI_RESULT + BOOTROM_RESULT + VCP_RESULT + EVENT_RESULT + ANALYSIS_RESULT + INTEGRATION_RESULT))
TOTAL_SUITES=11
PASSED_SUITES=$((TOTAL_SUITES - TOTAL_FAILURES))

echo "OVERALL STATISTICS:" >> $MASTER_REPORT
echo "==================" >> $MASTER_REPORT
echo "Total Test Suites: $TOTAL_SUITES" >> $MASTER_REPORT
echo "Passed Suites:     $PASSED_SUITES" >> $MASTER_REPORT
echo "Failed Suites:     $TOTAL_FAILURES" >> $MASTER_REPORT
echo "Success Rate:      $(( (PASSED_SUITES * 100) / TOTAL_SUITES ))%" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

echo "SYSTEM STATUS:" >> $MASTER_REPORT
echo "==============" >> $MASTER_REPORT
if [ $TOTAL_FAILURES -eq 0 ]; then
    echo "✅ ALL TESTS PASSED - T9000 SYSTEM FULLY OPERATIONAL" >> $MASTER_REPORT
elif [ $TOTAL_FAILURES -le 2 ]; then
    echo "⚠️  MINOR ISSUES - T9000 SYSTEM MOSTLY OPERATIONAL" >> $MASTER_REPORT
else
    echo "❌ SIGNIFICANT ISSUES - T9000 SYSTEM NEEDS ATTENTION" >> $MASTER_REPORT
fi
echo "" >> $MASTER_REPORT

echo "REPORT LOCATIONS:" >> $MASTER_REPORT
echo "=================" >> $MASTER_REPORT
echo "Master Report:       $MASTER_REPORT" >> $MASTER_REPORT
echo "Individual Reports:  $REPORTS_DIR/" >> $MASTER_REPORT
echo "Waveform Files:      gen/ (if simulation with waves enabled)" >> $MASTER_REPORT
echo "" >> $MASTER_REPORT

# Generate an HTML report as well
HTML_REPORT="$REPORTS_DIR/t9000_test_report.html"
cat > $HTML_REPORT << EOF
<!DOCTYPE html>
<html>
<head>
    <title>T9000 Transputer Test Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background-color: #2c3e50; color: white; padding: 20px; text-align: center; }
        .summary { background-color: #ecf0f1; padding: 15px; margin: 20px 0; }
        .pass { color: #27ae60; font-weight: bold; }
        .fail { color: #e74c3c; font-weight: bold; }
        .section { margin: 20px 0; }
        table { width: 100%; border-collapse: collapse; }
        th, td { border: 1px solid #bdc3c7; padding: 8px; text-align: left; }
        th { background-color: #34495e; color: white; }
    </style>
</head>
<body>
    <div class="header">
        <h1>T9000 Transputer Test Report</h1>
        <p>Generated: $(date)</p>
    </div>
    
    <div class="summary">
        <h2>Test Summary</h2>
        <p><strong>Total Suites:</strong> $TOTAL_SUITES</p>
        <p><strong>Passed:</strong> <span class="pass">$PASSED_SUITES</span></p>
        <p><strong>Failed:</strong> <span class="fail">$TOTAL_FAILURES</span></p>
        <p><strong>Success Rate:</strong> $(( (PASSED_SUITES * 100) / TOTAL_SUITES ))%</p>
    </div>
    
    <div class="section">
        <h2>Test Results by Component</h2>
        <table>
            <tr><th>Component</th><th>Result</th><th>Report</th></tr>
            <tr><td>Stack Plugin</td><td class="$([ $STACK_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $STACK_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Stack_report.txt">View Report</a></td></tr>
            <tr><td>Timer Plugin</td><td class="$([ $TIMER_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $TIMER_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Timer_report.txt">View Report</a></td></tr>
            <tr><td>Scheduler Plugin</td><td class="$([ $SCHEDULER_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $SCHEDULER_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Scheduler_report.txt">View Report</a></td></tr>
            <tr><td>Main Cache</td><td class="$([ $CACHE_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $CACHE_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="MainCache_report.txt">View Report</a></td></tr>
            <tr><td>Workspace Cache</td><td class="$([ $WORKSPACE_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $WORKSPACE_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="WorkspaceCache_report.txt">View Report</a></td></tr>
            <tr><td>VCP Plugin</td><td class="$([ $VCP_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $VCP_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="VCP_report.txt">View Report</a></td></tr>
            <tr><td>PMI Plugin</td><td class="$([ $PMI_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $PMI_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="PMI_report.txt">View Report</a></td></tr>
            <tr><td>Event Plugin</td><td class="$([ $EVENT_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $EVENT_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Event_report.txt">View Report</a></td></tr>
            <tr><td>Analysis Plugin</td><td class="$([ $ANALYSIS_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $ANALYSIS_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Analysis_report.txt">View Report</a></td></tr>
            <tr><td>Integration Tests</td><td class="$([ $INTEGRATION_RESULT -eq 0 ] && echo "pass" || echo "fail")">$([ $INTEGRATION_RESULT -eq 0 ] && echo "PASS" || echo "FAIL")</td><td><a href="Integration_report.txt">View Report</a></td></tr>
        </table>
    </div>
</body>
</html>
EOF

# Display final summary
echo ""
echo "=========================================="
echo "T9000 TEST FRAMEWORK EXECUTION COMPLETE"
echo "=========================================="
echo ""
echo "Results Summary:"
echo "  Total Suites: $TOTAL_SUITES"
echo "  Passed:       $PASSED_SUITES"
echo "  Failed:       $TOTAL_FAILURES"
echo "  Success Rate: $(( (PASSED_SUITES * 100) / TOTAL_SUITES ))%"
echo ""
echo "Reports Generated:"
echo "  📄 Master Report: $MASTER_REPORT"
echo "  🌐 HTML Report:   $HTML_REPORT"
echo "  📁 All Reports:   $REPORTS_DIR/"
echo ""

if [ $TOTAL_FAILURES -eq 0 ]; then
    echo "✅ ALL TESTS PASSED - T9000 SYSTEM FULLY OPERATIONAL"
else
    echo "⚠️  Some tests failed - Check individual reports for details"
fi

echo ""
echo "To view the master report:"
echo "  cat $MASTER_REPORT"
echo ""
echo "To view the HTML report:"
echo "  open $HTML_REPORT"
echo ""
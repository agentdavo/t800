#!/usr/bin/env tclsh
# Enhanced ECP5 synthesis script for Yosys + nextpnr-ecp5
# Usage: tclsh synth.tcl <constraints.lpf> <device>
# Example device: LFE5U-45F
# Notes: Assumes Transputer.v is in gen/src/verilog

# Retrieve command-line arguments
set lpf [lindex $argv 0]
set device [lindex $argv 1]

# Define fixed top file location
set top "gen/src/verilog/Transputer.v"
set basename "Transputer"
set json "$basename.json"
set asc "$basename.asc"
set bit "$basename.bit"
set rpt "$basename.rpt"
set topmod $basename

# Ensure top file exists
if {![file exists $top]} {
  puts "[info level0] Error: Top file $top does not exist!"
  exit 1
}

# Run Yosys synthesis with optimization
puts "[info level0] Running Yosys..."
if {[catch {exec yosys -p "read_verilog $top; synth_ecp5 -top $topmod -json $json -abc9 -no-rw-check"} result]} {
  puts "[info level0] Yosys failed: $result"
  exit 1
}

# Run nextpnr-ecp5 with placement and timing optimization
puts "[info level0] Running nextpnr-ecp5..."
if {[catch {exec nextpnr-ecp5 --json $json --lpf $lpf --textcfg $asc --device $device --report $rpt --timing-allow-fail --freq 250 --placer heap} result]} {
  puts "[info level0] nextpnr-ecp5 failed: $result"
  exit 1
}

# Run ecppack to generate bitstream
puts "[info level0] Running ecppack..."
if {[catch {exec ecppack $asc $bit} result]} {
  puts "[info level0] ecppack failed: $result"
  exit 1
}
puts "[info level0] Bitstream generated: $bit"
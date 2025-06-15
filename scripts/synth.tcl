#!/usr/bin/env tclsh
# Basic ECP5 synthesis script for Yosys + nextpnr-ecp5
# Usage: tclsh synth.tcl <top.v> <constraints.lpf> <device>
# Example device: LFE5U-45F

set top [lindex $argv 0]
set lpf [lindex $argv 1]
set device [lindex $argv 2]

set basename [file rootname $top]
set json "$basename.json"
set asc "$basename.asc"
set bit "$basename.bit"
set rpt "$basename.rpt"
set topmod $basename

puts "[info level0] Running Yosys..."
yosys -p "read_verilog $top; synth_ecp5 -top $topmod -json $json"

puts "[info level0] Running nextpnr-ecp5..."
nextpnr-ecp5 --json $json --lpf $lpf --textcfg $asc --device $device --report $rpt

puts "[info level0] Running ecppack..."
ecppack $asc $bit
puts "[info level0] Bitstream generated: $bit"

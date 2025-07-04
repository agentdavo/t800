#!/bin/bash

# T9000 Bootload Instruction Trace Test Script
# Simulates execution of bootload.hex with per-cycle instruction trace

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
HEX_FILE="$SCRIPT_DIR/hex/bootload.hex"
TRACE_REPORT="$SCRIPT_DIR/test_reports/bootload_trace.txt"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}T9000 Bootload Instruction Trace Test${NC}"
echo -e "${BLUE}========================================${NC}"

# Create test reports directory if it doesn't exist
mkdir -p "$SCRIPT_DIR/test_reports"

# First, ensure bootload.hex exists
if [ ! -f "$HEX_FILE" ]; then
    echo -e "${YELLOW}Bootload.hex not found. Assembling bootload.asm...${NC}"
    cd "$PROJECT_ROOT"
    sbt "runMain transputer.TransputerAssembler --bootload" || {
        echo -e "${RED}Failed to assemble bootload.asm${NC}"
        exit 1
    }
fi

echo -e "${GREEN}✓ Found bootload.hex${NC}"

# Check if simulation file exists, if not create it
if [ ! -f "$PROJECT_ROOT/src/test/scala/transputer/BootloadTraceSim.scala" ]; then
    echo -e "${YELLOW}Creating BootloadTraceSim.scala...${NC}"
    cat > "$PROJECT_ROOT/src/test/scala/transputer/BootloadTraceSim.scala" << 'EOF'
package transputer

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb._
import scala.collection.mutable
import scala.io.Source
import java.io.{File, PrintWriter}

object BootloadTraceSim {
  
  // T9000 instruction trace format
  case class TraceEntry(
    iptr: Long,
    code: List[Int],
    mnemonic: String,
    operand: String,
    halt: String = "--",
    areg: Long = 0,
    breg: Long = 0,
    creg: Long = 0,
    wptr: Long = 0x80000230,
    wptrData: Long = 0
  ) {
    def format: String = {
      val codeStr = code.map(b => f"$b%02X").mkString(" ")
      val codeField = f"$codeStr%-30s"
      val mnemonicField = f"$mnemonic%-8s $operand%-12s"
      f" ${iptr}%08X: $codeField$mnemonicField$halt%-4s ${areg}%08X ${breg}%08X ${creg}%08X   ${wptr}%08X ${wptrData}%08X"
    }
  }
  
  // Instruction decoder
  def decodeInstruction(bytes: List[Int], iptr: Long): (String, String, Int) = {
    if (bytes.isEmpty) return ("???", "", 1)
    
    val opcode = (bytes.head >> 4) & 0xF
    val operand = bytes.head & 0xF
    
    val (mnemonic, operandStr, length) = opcode match {
      case 0x0 => ("J", s"#${operand.toHexString}", 1)
      case 0x1 => ("LDLP", s"#$operand", 1)
      case 0x2 => ("PFIX", s"#$operand", 1)
      case 0x3 => ("LDNL", s"#$operand", 1)
      case 0x4 => ("LDC", s"#$operand", 1)
      case 0x5 => ("LDNLP", s"#$operand", 1)
      case 0x6 => ("NFIX", s"#$operand", 1)
      case 0x7 => ("LDL", s"#$operand", 1)
      case 0x8 => ("ADC", s"#$operand", 1)
      case 0x9 => ("CALL", s"#${operand.toHexString}", 1)
      case 0xA => ("CJ", s"#${operand.toHexString}", 1)
      case 0xB => ("AJW", s"#$operand", 1)
      case 0xC => ("EQC", s"#$operand", 1)
      case 0xD => ("STL", s"#$operand", 1)
      case 0xE => ("STNL", s"#$operand", 1)
      case 0xF => 
        // Secondary operations
        val fullOp = if (bytes.length > 1 && (bytes.head & 0xF) == 0x2) {
          // Extended secondary op
          ((operand << 4) | (bytes(1) & 0xF))
        } else {
          operand
        }
        
        val (secMnemonic, secLength) = fullOp match {
          case 0x00 => ("REV", 1)
          case 0x05 => ("ADD", 1)
          case 0x06 => ("DUP", 1)
          case 0x07 => ("GCALL", 1)
          case 0x08 => ("IN", 1)
          case 0x0B => ("WSUB", 1)
          case 0x0C => ("OUT", 1)
          case 0x0D => ("SUB", 1)
          case 0x18 => ("STHF", 1)
          case 0x19 => ("STLF", 1)
          case 0x1B => ("LDPI", 2)  // Uses prefix
          case 0x20 => ("RET", 1)
          case 0x21 => ("LEND", 2)
          case 0x29 => ("TESTERR", 1)
          case 0x2F => ("TERMINATE", 1)
          case 0x33 => ("XOR", 1)
          case 0x3C => ("GAJW", 1)
          case 0x42 => ("MINT", 1)
          case 0x54 => ("STTIMER", 1)
          case 0x57 => ("CLRHALTERR", 1)
          case 0x58 => ("SETHALTERR", 1)
          case 0x9C => ("FPTESTERR", 1)
          case _ => (f"OPR#${fullOp}%02X", 1)
        }
        (secMnemonic, "", if (fullOp > 15) 2 else secLength)
      case _ => ("???", "", 1)
    }
    
    (mnemonic, operandStr, length)
  }
  
  // Load hex file into memory
  def loadHexFile(filename: String): Map[Long, Int] = {
    val memory = mutable.Map[Long, Int]()
    var extendedAddr = 0L
    
    val source = Source.fromFile(filename)
    try {
      for (line <- source.getLines() if line.startsWith(":")) {
        val recordType = line.substring(7, 9)
        recordType match {
          case "04" => // Extended Linear Address
            extendedAddr = Integer.parseInt(line.substring(9, 13), 16).toLong << 16
          case "00" => // Data
            val byteCount = Integer.parseInt(line.substring(1, 3), 16)
            val address = Integer.parseInt(line.substring(3, 7), 16).toLong + extendedAddr
            for (i <- 0 until byteCount) {
              val byte = Integer.parseInt(line.substring(9 + i * 2, 11 + i * 2), 16)
              memory(address + i) = byte
            }
          case _ => // Ignore other record types
        }
      }
    } finally {
      source.close()
    }
    
    memory.toMap
  }
  
  def main(args: Array[String]): Unit = {
    val hexFile = if (args.nonEmpty) args(0) else "scripts/hex/bootload.hex"
    val traceFile = if (args.length > 1) args(1) else "scripts/test_reports/bootload_trace.txt"
    
    println(s"Loading hex file: $hexFile")
    val memory = loadHexFile(hexFile)
    println(s"Loaded ${memory.size} bytes")
    
    // Initialize processor state
    var iptr = 0x80000000L
    var areg = 0L
    var breg = 0L
    var creg = 0L
    var wptr = 0x80000230L
    var halted = false
    val traces = mutable.ListBuffer[TraceEntry]()
    
    // Simulation parameters
    val maxCycles = 1000
    var cycle = 0
    
    println("\n" + "-" * 110)
    println("-IPtr------Code-----------------------Mnemonic------------HE---AReg-----BReg-----CReg-------WPtr-----WPtr[0]-")
    println("-" * 110)
    
    val writer = new PrintWriter(new File(traceFile))
    writer.println("-IPtr------Code-----------------------Mnemonic------------HE---AReg-----BReg-----CReg-------WPtr-----WPtr[0]-")
    writer.println("-" * 110)
    
    try {
      while (!halted && cycle < maxCycles && memory.contains(iptr)) {
        // Fetch instruction bytes
        val bytes = (0 to 3).map(i => memory.getOrElse(iptr + i, 0)).toList
        val (mnemonic, operand, length) = decodeInstruction(bytes, iptr)
        val instrBytes = bytes.take(length)
        
        // Create trace entry
        val trace = TraceEntry(
          iptr = iptr,
          code = instrBytes,
          mnemonic = mnemonic,
          operand = operand,
          areg = areg,
          breg = breg,
          creg = creg,
          wptr = wptr,
          wptrData = memory.getOrElse(wptr, 0)
        )
        
        traces += trace
        println(trace.format)
        writer.println(trace.format)
        
        // Simple instruction execution simulation
        val opcode = (bytes.head >> 4) & 0xF
        val op = bytes.head & 0xF
        
        opcode match {
          case 0x4 => // LDC
            creg = breg
            breg = areg
            areg = op
          case 0x7 => // LDL
            creg = breg
            breg = areg
            areg = memory.getOrElse(wptr + op * 4, 0).toLong & 0xFFFFFFFFL
          case 0xB => // AJW
            wptr = wptr + op * 4
          case 0xD => // STL
            memory(wptr + op * 4) = areg.toInt
          case 0xF if op == 0x42 => // MINT
            creg = breg
            breg = areg
            areg = 0x80000000L
          case 0xF if op == 0x2F => // TERMINATE
            halted = true
            println("\n-I-EMUDBG: Processor terminated")
            writer.println("\n-I-EMUDBG: Processor terminated")
          case _ => // Other instructions - just advance
        }
        
        iptr += length
        cycle += 1
        
        // Add some debug messages for specific operations
        mnemonic match {
          case "IN" =>
            println(s"-I-EMUDBG: In(1): Channel=#${areg.toHexString}, to memory at #${breg.toHexString}, length #${creg}")
            writer.println(s"-I-EMUDBG: In(1): Channel=#${areg.toHexString}, to memory at #${breg.toHexString}, length #${creg}")
          case "GCALL" =>
            println(s"-I-EMUDBG: Calling function at #${areg.toHexString}")
            writer.println(s"-I-EMUDBG: Calling function at #${areg.toHexString}")
          case _ =>
        }
      }
      
      println("-" * 110)
      writer.println("-" * 110)
      println(s"\nSimulation complete after $cycle cycles")
      writer.println(s"\nSimulation complete after $cycle cycles")
      
      if (cycle >= maxCycles) {
        println("Warning: Maximum cycles reached")
        writer.println("Warning: Maximum cycles reached")
      }
      
    } finally {
      writer.close()
    }
    
    println(s"\nTrace written to: $traceFile")
  }
}
EOF
    echo -e "${GREEN}✓ Created BootloadTraceSim.scala${NC}"
else
    echo -e "${GREEN}✓ Using existing BootloadTraceSim.scala${NC}"
fi

# Run the simulation
echo -e "\n${YELLOW}Running bootload trace simulation...${NC}\n"
cd "$PROJECT_ROOT"

sbt "test:runMain transputer.BootloadTraceSim $HEX_FILE $TRACE_REPORT" || {
    echo -e "${RED}Simulation failed${NC}"
    exit 1
}

echo -e "\n${GREEN}✓ Simulation complete${NC}"
echo -e "${GREEN}✓ Trace output saved to: $TRACE_REPORT${NC}"

# Display first 50 lines of trace
echo -e "\n${BLUE}First 50 lines of instruction trace:${NC}"
echo -e "${YELLOW}$(head -52 "$TRACE_REPORT")${NC}"

echo -e "\n${BLUE}========================================${NC}"
echo -e "${GREEN}Bootload trace test complete!${NC}"
echo -e "${BLUE}========================================${NC}"
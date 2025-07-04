#!/bin/bash

# T9000 Konata Pipeline Visualization Workflow Demo
# This script demonstrates the complete workflow from assembly to pipeline visualization

echo "=== T9000 Konata Pipeline Visualization Demo ==="
echo ""

# Step 1: Create a test program
echo "Step 1: Creating test assembly program..."
cat > scripts/asm/konata_demo.asm << 'EOF'
; Konata Pipeline Demo
; Shows various pipeline behaviors

Start:
    ; Initialize processor
    mint            ; Load NotProcess.p
    sthf            ; Set high priority queue
    mint
    stlf            ; Set low priority queue
    
    ; Simple arithmetic sequence
    ldc     10      ; Instruction 0: Load 10
    ldc     20      ; Instruction 1: Load 20 (creates dependency)
    add             ; Instruction 2: Add (depends on both loads)
    
    ; Memory operation (potential cache miss)
    stl     0       ; Store result
    ldl     0       ; Load it back (may stall)
    
    ; Branch operation (may cause flush)
    eqc     30      ; Check if result is 30
    cj      Pass    ; Jump if equal
    
    ; Not taken path
    ldc     99      ; This might be flushed
    j       Fail
    
Pass:
    ldc     1       ; Success marker
    j       Done
    
Fail:
    ldc     0       ; Failure marker
    
Done:
    terminate       ; End program
EOF

echo "Created: scripts/asm/konata_demo.asm"
echo ""

# Step 2: Assemble the program
echo "Step 2: Assembling to hex format..."
sbt -error "runMain transputer.TransputerAssembler scripts/asm/konata_demo.asm" 2>&1 | grep -E "(Generated|Size|Error)"
echo ""

# Step 3: Generate RTL
echo "Step 3: Generating T9000 RTL..."
sbt -error "runMain transputer.GenerateWithTest" 2>&1 | grep "Generated"
echo ""

# Step 4: Show how to generate with Konata (simulation would follow)
echo "Step 4: Command to generate with Konata visualization:"
echo "  sbt \"runMain transputer.GenerateWithTest --hex scripts/hex/konata_demo.hex --konata --wave\""
echo ""
echo "Note: Full simulation requires Verilator. The Konata log would be saved to:"
echo "  simWorkspace/konata.log"
echo ""

# Step 5: Generate a static Konata example
echo "Step 5: Generating example Konata log..."
sbt -error "testOnly transputer.KonataTest -- -z \"Generate example\"" 2>&1 | grep -E "(Generated|View)"

echo ""
echo "=== Workflow Complete ==="
echo ""
echo "Files created:"
echo "  - scripts/asm/konata_demo.asm   : Source assembly"
echo "  - scripts/hex/konata_demo.hex   : Assembled program"  
echo "  - generated/T9000Transputer.v   : RTL output"
echo "  - generated/example.kanata      : Example Konata log"
echo ""
echo "To view the pipeline visualization:"
echo "  1. Download Konata from: https://github.com/shioyadan/Konata/releases"
echo "  2. Open generated/example.kanata in Konata"
echo ""
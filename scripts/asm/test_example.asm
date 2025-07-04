; T9000 Test Example Assembly Program
; This demonstrates using the assembler with enhanced testing features
;
; Assemble with:
;   sbt "runMain transputer.TransputerAssembler scripts/asm/test_example.asm"
;
; Test with:
;   sbt "runMain transputer.T9000GenerateEnhanced \
;       --load-hex scripts/hex/test_example.hex \
;       --pass-symbol 0x80001000 \
;       --fail-symbol 0x80002000 \
;       --with-wave --with-konata"

; Constants
VAL PASS_ADDR IS 0x80001000
VAL FAIL_ADDR IS 0x80002000
VAL STACK_SIZE IS 32

; Start of program (0x80000000)
Start:
    ; Initialize processor state
    mint                    ; Load NotProcess.p (0x80000000)
    sthf                    ; Initialize high priority queue
    mint
    stlf                    ; Initialize low priority queue
    
    ldc     0
    sttimer                 ; Start timer
    
    testerr                 ; Clear error flag
    clrhalterr              ; Clear halt on error
    
    ; Adjust workspace for our program
    ajw     STACK_SIZE      ; Create stack space
    
    ; Run tests
    call    TestArithmetic
    call    TestStack
    call    TestMemory
    call    TestControl
    
    ; All tests passed - jump to pass symbol
    j       PassPoint

; Test 1: Basic Arithmetic
TestArithmetic:
    ; Test addition
    ldc     10
    ldc     20
    add                     ; Should be 30
    eqc     30
    cj      FailPoint       ; Fail if not 30
    
    ; Test subtraction
    ldc     50
    ldc     20
    sub                     ; Should be 30
    eqc     30
    cj      FailPoint
    
    ; Test multiplication (if available)
    ldc     5
    ldc     6
    prod                    ; Should be 30
    eqc     30
    cj      FailPoint
    
    ret

; Test 2: Stack Operations
TestStack:
    ; Test dup
    ldc     42
    dup                     ; Stack: 42, 42
    eqc     42
    cj      FailPoint
    eqc     42
    cj      FailPoint
    
    ; Test rev
    ldc     1
    ldc     2
    rev                     ; Stack: 2, 1
    eqc     1
    cj      FailPoint
    eqc     2
    cj      FailPoint
    
    ret

; Test 3: Memory Operations
TestMemory:
    ; Store and load local
    ldc     123
    stl     0               ; Store 123 in local 0
    ldc     456
    stl     1               ; Store 456 in local 1
    
    ldl     0               ; Load local 0
    eqc     123
    cj      FailPoint
    
    ldl     1               ; Load local 1
    eqc     456
    cj      FailPoint
    
    ; Test workspace-relative addressing
    ldlp    0               ; Load pointer to local 0
    ldnl    0               ; Load via pointer
    eqc     123
    cj      FailPoint
    
    ret

; Test 4: Control Flow
TestControl:
    ; Test conditional jump
    ldc     1
    eqc     1
    cj      ControlFail     ; Should not jump (condition false)
    j       ControlPass
    
ControlFail:
    j       FailPoint
    
ControlPass:
    ; Test loop
    ldc     5
    stl     2               ; Loop counter
    
LoopStart:
    ldl     2
    adc     -1              ; Decrement
    dup
    stl     2               ; Store back
    cj      LoopEnd         ; Exit if zero
    j       LoopStart
    
LoopEnd:
    ; Counter should be 0
    ldl     2
    eqc     0
    cj      FailPoint
    
    ret

; Pass point - aligned to specific address
    align
PassData:
    db      "PASS", 0x00
    align
    
; Calculate padding to reach pass address
    ; This would need manual adjustment based on actual code size
    ; For demonstration, we'll use a jump
PassPoint:
    ldc     PASS_ADDR
    gcall                   ; Jump to pass address
    
; Pass infinite loop at specific address
; In real code, this would be at 0x80001000
PassLoop:
    j       PassLoop

; Fail point
    align
FailData:
    db      "FAIL", 0x00
    align
    
FailPoint:
    ldc     FAIL_ADDR
    gcall                   ; Jump to fail address
    
; Fail infinite loop at specific address
; In real code, this would be at 0x80002000
FailLoop:
    j       FailLoop

; End of program
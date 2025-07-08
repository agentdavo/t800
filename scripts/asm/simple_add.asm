; Simple Addition Test
; Tests basic ALU operation (addition)

; Initialize processor
Start:
    mint            ; Load NotProcess.p (0x80000000)
    sthf            ; Initialize high priority queue
    mint
    stlf            ; Initialize low priority queue
    
    ; Perform addition: 10 + 20 = 30
    ldc     10      ; Load constant 10 to A register
    ldc     20      ; Load constant 20 to A register (A=20, B=10)
    add             ; Add: A = A + B (A=30)
    
    ; Check result
    eqc     30      ; Is A == 30?
    cj      Fail    ; Jump to Fail if not equal
    
    ; Success - infinite loop
Pass:
    j       Pass
    
    ; Failure - infinite loop
Fail:
    j       Fail
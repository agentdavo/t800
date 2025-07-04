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

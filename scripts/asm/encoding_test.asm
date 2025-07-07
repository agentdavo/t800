; Test special encoding cases
ORG 0x80000000

; Test standard secondary op (fits in 4 bits)
    add         ; 0xF5 - single byte

; Test extended secondary op (needs prefix)
    mint        ; 0x42 - needs pfix #4, opr #2

; Test negative operation
    ldshadow    ; pfix #2, pfix #6, opr #3

; Test FPU extended operation  
    fpusqrtfirst ; pfix #2, pfix #0, pfix #0, pfix #0, pfix #0, opr #0

; Test terminate (special multi-byte encoding 0x2FF)
    terminate   ; needs multiple prefix bytes

END
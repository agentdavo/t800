; FPU instruction test
ORG 0x80000000

; Standard FPU operations (should be 2 bytes each)
    fpadd      ; 0x87
    fpmul      ; 0x8b
    
; FPU extended operation (should be 6 bytes)
    fpusqrtfirst  ; 0x20000 encoding
    
END
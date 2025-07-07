; Debug test for FPU extended operations
ORG 0x80000000

; Just test fpusqrtfirst
    fpusqrtfirst ; Should encode as: pfix 0, pfix 0, pfix 0, pfix 0, pfix 2, opr 0

END
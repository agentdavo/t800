; Simple test for INMOS directive support
TITLE Simple Test
PAGE 60,132

    .TRANSPUTER
    MaxINT      EQU 0x7FFFFFFF
    TptrLoc0    EQU 0x80000024
    TptrLoc1    EQU 0x80000028

    ORG         0x80000070

Start:
    mint                        ; load NotProcess.p
    sthf                        ; init high priority queue
    mint
    stlf                        ; init low priority queue
    
    ldc     TptrLoc0           ; test EQU constant
    ldc     MaxINT             ; test another constant
    
    j       Done

Message:    
    DB      "Hello", 0x00
    
Done:
    terminate
    
    END
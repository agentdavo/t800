# Hello World ROM Program

This file contains the full assembly listing used in the
`HelloWorldSpec` test. The code runs from ROM and prints "hello world"
over link 0 using the IServer protocol.

```asm
TITLE Transputer Hello World using the IServer protocol, runs from ROM.

PAGE 60,132

    .TRANSPUTER
    MaxINT      EQU 0x7FFFFFFF
    ResetCode   EQU 0x7FFFFFFE
    TptrLoc0    EQU 0x80000024
    TptrLoc1    EQU 0x80000028

    start_time  EQU 0

    CodeLen     EQU 0x000003FF
    ORG         (ResetCode - 8) - CodeLen

CodeStart:
    mint
    sthf
    mint
    stlf
    mint
    ldc     TPtrLoc0
    stnl    0
    mint
    ldc     TPtrLoc1
    stnl    0
    ldc     start_time
    sttimer
    testerr
    clrhalterr
    fptesterr
    mint
    ldc     0x80000020
    stnl    0
    mint
    ldc     0x8000001C
    stnl    0
    mint
    ldc     0x80000018
    stnl    0
    mint
    ldc     0x80000014
    stnl    0
    mint
    ldc     0x80000010
    stnl    0
    mint
    ldc     0x8000000C
    stnl    0
    mint
    ldc     0x80000008
    stnl    0
    mint
    ldc     0x80000004
    stnl    0
    mint
    ldc     0x80000000
    stnl    0

    j       MAIN

    REQ_PUTS        EQU 0x0f
    STDOUT_STREAMID EQU 0x01
    LINK0_OUTPUT    EQU 0x80000000
    LINK0_INPUT     EQU 0x80000010

HWSTR:
    DB      "hello world", 0x00

MAIN:
    ajw     0x100
    ldc     HWSTR - _M1
    ldpi
_M1:
    call    putConsolePString
    terminate

; -------------------------------------------------------------
; putConsolePString routine
; -------------------------------------------------------------

putConsolePString:
    PCPS_WLEN       EQU 2

    PPS_STRINGADDR  EQU 0
    PPS_STRINGLEN   EQU 1

    ajw     -PCPS_WLEN
    ldl     WS_STR_ADDR
    dup
    stl     PPS_STRINGADDR
    call    strlen
    dup
    stl     PPS_STRINGLEN
    adc     7
    call    outshort0
    ldc     LINK0_OUTPUT
    ldc     REQ_PUTS
    outbyte
    ldc     LINK0_OUTPUT
    ldc     STDOUT_STREAMID
    outword
    ldl     PPS_STRINGLEN
    call    outshort0
    ldl     PPS_STRINGADDR
    ldc     LINK0_OUTPUT
    ldl     PPS_STRINGLEN
    out
    ajw     PCPS_WLEN
    ret

outshort0:
    WSOS0_WORD EQU 0x01
    ldc     LINK0_OUTPUT
    ldlp    WSOS0_WORD
    rev
    ldc     2
    out
    ret

strlen:
    ldl     0x01
    ldc     0
_sl_loop:
    rev
    dup
    lb
    cj      _sl_end
    adc     1
    rev
    adc     1
    j       _sl_loop
_sl_end:
    pop
    pop
    ret

CodeEnd:

_ActualCodeLen  EQU CodeEnd - CodeStart

    ORG     ResetCode - 8
JumpToCodeStart:
    j       CodeStart
    ORG     ResetCode
    j       JumpToCodeStart

_XtraPadding   EQU JumpToCodeStart - CodeEnd

    END
```

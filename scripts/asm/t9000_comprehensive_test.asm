; Comprehensive T9000 Instruction Set Test
; Tests all major instruction categories

ORG 0x80000000

Start:
    ; === Primary Instructions ===
    j       Start           ; 0x0 - Jump
    ldlp    10              ; 0x1 - Load local pointer
    pfix    0x12            ; 0x2 - Prefix
    ldnl    4               ; 0x3 - Load non-local
    ldc     100             ; 0x4 - Load constant
    ldnlp   8               ; 0x5 - Load non-local pointer
    nfix    -5              ; 0x6 - Negative prefix
    ldl     2               ; 0x7 - Load local
    adc     50              ; 0x8 - Add constant
    call    TestFunc        ; 0x9 - Call
    cj      SkipLabel       ; 0xA - Conditional jump
    ajw     16              ; 0xB - Adjust workspace
    eqc     0               ; 0xC - Equals constant
    stl     3               ; 0xD - Store local
    stnl    5               ; 0xE - Store non-local
    ; opr is 0xF - used for secondary operations

    ; === Basic Secondary Operations (no prefix) ===
    rev                     ; Reverse stack
    add                     ; Add
    sub                     ; Subtract
    prod                    ; Product (multiply)
    gt                      ; Greater than
    diff                    ; Difference
    
    ; === Long Arithmetic ===
    ladd                    ; Long add
    lsub                    ; Long subtract
    lmul                    ; Long multiply
    ldiv                    ; Long divide
    lshr                    ; Long shift right
    lshl                    ; Long shift left
    
    ; === Bitwise Operations ===
    and                     ; Bitwise AND
    or                      ; Bitwise OR
    xor                     ; Bitwise XOR
    not                     ; Bitwise NOT
    shl                     ; Shift left
    shr                     ; Shift right
    
    ; === T9000 Unsigned Operations ===
    gtu                     ; Greater than unsigned
    cbu                     ; Check byte unsigned
    csu                     ; Check sixteen unsigned
    ciru                    ; Check in range unsigned
    
    ; === Process Management ===
    startp                  ; Start process
    endp                    ; End process
    runp                    ; Run process
    stopp                   ; Stop process
    
    ; === Timer Operations ===
    ldtimer                 ; Load timer
    sttimer                 ; Set timer
    tin                     ; Timer input
    talt                    ; Timer alt start
    taltwt                  ; Timer alt wait
    
    ; === ALT Constructs ===
    alt                     ; Alt start
    altwt                   ; Alt wait
    altend                  ; Alt end
    enbc                    ; Enable channel
    disc                    ; Disable channel
    
    ; === Memory Operations ===
    lb                      ; Load byte
    sb                      ; Store byte
    ls                      ; Load sixteen
    ss                      ; Store sixteen
    lbx                     ; Load byte and sign extend
    lsx                     ; Load sixteen and sign extend
    xbword                  ; Sign extend byte to word
    xsword                  ; Sign extend sixteen to word
    
    ; === Device Operations ===
    devlb   0x1000          ; Device load byte
    devsb   0x1004          ; Device store byte
    devls   0x1008          ; Device load sixteen
    devss   0x100C          ; Device store sixteen
    devlw   0x1010          ; Device load word
    devsw   0x1014          ; Device store word
    
    ; === FPU Operations ===
    fpadd                   ; FP add
    fpsub                   ; FP subtract
    fpmul                   ; FP multiply
    fpdiv                   ; FP divide
    fpgt                    ; FP greater than
    fpeq                    ; FP equal
    fprem                   ; FP remainder
    fpsqrt                  ; FP square root
    fpabs                   ; FP absolute value
    fpdup                   ; FP duplicate
    fprev                   ; FP reverse
    
    ; === FPU Conversion Operations ===
    fpi32tor32              ; INT32 to REAL32
    fpi32tor64              ; INT32 to REAL64
    fpr32tor64              ; REAL32 to REAL64
    fpr64tor32              ; REAL64 to REAL32
    fprtoi32                ; REAL to INT32
    
    ; === FPU Rounding Modes ===
    fprn                    ; Round to nearest
    fprz                    ; Round to zero
    fprp                    ; Round positive
    fprm                    ; Round minus
    
    ; === CRC and Bit Operations ===
    crcword                 ; CRC on word
    crcbyte                 ; CRC on byte
    bitcnt                  ; Count bits
    bitrevword              ; Reverse bits in word
    bitrevnbits             ; Reverse n bits
    
    ; === 2D Block Move ===
    move2dinit              ; Initialize 2D move
    move2dall               ; 2D move all
    move2dnonzero           ; 2D move non-zero
    move2dzero              ; 2D move zero
    
    ; === Configuration ===
    ldconf                  ; Load configuration
    stconf                  ; Store configuration
    ldflags                 ; Load flags
    stflags                 ; Store flags
    
    ; === Interrupts and Protection ===
    intdis                  ; Interrupt disable
    intenb                  ; Interrupt enable
    ldth                    ; Load trap handler
    
    ; === Negative Prefix Operations ===
    ldshadow                ; Load shadow registers
    stshadow                ; Store shadow registers
    tret                    ; Trap return
    goprot                  ; Go protected
    syscall                 ; System call
    signal                  ; Signal semaphore
    wait                    ; Wait on semaphore
    grant                   ; Grant resource
    mkrc                    ; Mark resource channel
    unmkrc                  ; Unmark resource channel
    nop                     ; No operation
    
    ; === Cache Operations ===
    ica                     ; Invalidate cache all
    fdca                    ; Flush/disable cache all
    icl                     ; Invalidate cache line
    fdcl                    ; Flush/disable cache line
    
    ; === Special Operations ===
    mint                    ; Minimum integer
    pop                     ; Pop stack
    dup                     ; Duplicate
    terminate               ; Terminate
    
SkipLabel:
    ; Target for conditional jump
    nop
    
TestFunc:
    ; Test function
    ret                     ; Return
    
DataArea:
    db      0x12, 0x34, 0x56, 0x78
    
END
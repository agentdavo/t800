; T9000 Instruction Set Test
; Tests various T9000-specific instructions

ORG 0x80000000

Start:
    ; Test basic arithmetic
    ldc     10          ; load constant 10
    ldc     20          ; load constant 20
    add                 ; basic add (secondary op)
    
    ; Test long arithmetic
    ldc     0x12345678  ; load 32-bit constant
    ldc     0x87654321  ; load another 32-bit constant
    ladd                ; long add
    
    ; Test unsigned comparison (T9000 specific)
    ldc     100
    ldc     200
    gtu                 ; greater than unsigned
    
    ; Test floating point operations
    fpadd               ; FP add
    fpsub               ; FP subtract
    fpmul               ; FP multiply
    fpdiv               ; FP divide
    
    ; Test 16-bit operations (T9000 specific)
    ldc     TestData
    ls                  ; load sixteen
    ldc     TestData
    ss                  ; store sixteen
    
    ; Test device access (T9000 specific)
    ldc     0x80001000  ; device address
    devlw               ; device load word
    ldc     0x80001004  ; device address
    devsw               ; device store word
    
    ; Test protection operations
    ldth                ; load trap handler
    intdis              ; interrupt disable
    intenb              ; interrupt enable
    
    ; Test negative operations
    fpstall             ; stall FPU
    ldshadow            ; load shadow registers
    stshadow            ; store shadow registers
    tret                ; trap return
    
    ; Test extended FPU operations
    fpusqrtfirst        ; FPU square root first
    fpusqrtstep         ; FPU square root step
    fpusqrtlast         ; FPU square root last
    
    ; Test CRC and bit operations (T9000 specific)
    ldc     0xABCDEF01
    crcword             ; CRC on word
    bitcnt              ; count bits
    bitrevword          ; reverse bits in word
    
    ; Test ALT constructs
    alt                 ; start alternative
    enbc                ; enable channel
    altwt               ; alt wait
    altend              ; alt end
    
    ; Test resource operations
    grant               ; grant resource
    mkrc                ; mark resource channel
    unmkrc              ; unmark resource channel
    
    ; Test semaphore operations
    signal              ; signal semaphore
    wait                ; wait on semaphore
    
    ; Test timer operations
    ldtimer             ; load timer
    sttimer             ; set timer
    settimeslice        ; set timeslicing
    
    ; Test cache operations
    ica                 ; invalidate cache all
    fdca                ; flush and disable cache all
    
    ; Test system operations
    ldconf              ; load configuration
    stconf              ; store configuration
    ldflags             ; load status flags
    stflags             ; store status flags
    
    ; Test special operations
    restart             ; restart processor
    reboot              ; reboot system
    
    terminate           ; end test

TestData:
    db      0x12, 0x34, 0x56, 0x78
    
END
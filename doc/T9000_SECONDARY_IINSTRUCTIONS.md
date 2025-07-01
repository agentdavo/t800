Below are the tables from 6.9 to 6.37 extracted from the document.

### **Table 6.9 IMS T9000 arithmetic and logical instructions**

| Memory Code | Mnemonic | Processor cycles | Name | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 24F6 | and | 1 | and | |
| 24FB | or | 1 | or | |
| 23F3 | xor | 1 | exclusive or | |
| 23F2 | not | 1 | bitwise not | |
| 24F1 | shl | 1 | shift left | |
| 24F0 | shr | 1 | shift right | |
| F5 | add | 1 | add | O |
| FC | sub | 1 | subtract | O |
| 25F3 | mul | 2-5 | multiply | O |
| 27F2 | fmul | 3-6 | fractional multiply | O |
| 22FC | div | 5-12 | divide | O |
| 21FF | rem | 6-13 | remainder | O |
| F9 | gt | 1 | greater than | |
| 25F5 | gtu | 1 | greater than unsigned | |
| F4 | diff | 1 | difference | |
| 25F2 | sum | 1 | sum | |
| F8 | prod | 2-5 | product | |

### **Table 6.10 IMS T9000 long arithmetic instructions**

| Memory Code | Mnemonic | Processor cycles | Name | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 21F6 | ladd | 1 | long add | O |
| 23F8 | lsub | 1 | long subtract | O |
| 23F7 | lsum | 1 | long sum | |
| 24FF | ldiff | 1 | long diff | |
| 23F1 | lmul | 3-6 | long multiply | |
| 21FA | ldiv | 15 | long divide | O |
| 23F6 | lshl | 2 | long shift left | |
| 23F5 | lshr | 2 | long shift right | |
| 21F9 | norm | 2-3 | normalize | |

### **Table 6.11 IMS T9000 jump and call instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 22F0 | ret | return | M |
| 21FB | ldpi | load pointer to instruction | |
| 23FC | gajw | general adjust workspace | M,U |
| F6 | gcall | general call | |
| 22F1 | lend | loop end | M,T,U,D |

### **Table 6.12 IMS T9000 block move instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 24FA | move | move message | M,I |
| 25FB | move2dinit | initialize data for 2D block move | |
| 25FC | move2dall | 2D block copy | M,I |
| 25FD | move2dnonzero | 2D block copy non-zero bytes | M,I |
| 25FE | move2dzero | 2D block copy zero bytes | M,I |

### **Table 6.13 IMS T9000 indexing/array instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| F2 | bsub | byte subscript | |
| FA | wsub | word subscript | |
| 28F1 | wsubdb | form double word subscript | |
| 2CF1 | ssub | sixteen subscript | |
| 23F4 | bcnt | byte count | |
| 23FF | wcnt | word count | |
| F1 | lb | load byte | M |
| 23FB | sb | store byte | M |
| 2CFA | ls | load sixteen | MU |
| 2CF8 | ss | store sixteen | MU |
| 2BF9 | lbx | load byte and sign extend | M |
| 2FF9 | lsx | load sixteen and sign extend | MU |

### **Table 6.14 IMS T9000 range checking and conversion instructions**

| Memory Code | Mnemonic | Processor cycles | Name | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 2CF7 | cir | 2 | check in range | E |
| 2CFC | ciru | 2 | check in range unsigned | E |
| 2BFA | cb | - | check byte | E |
| 2BFB | cbu | - | check byte unsigned | E |
| 2FFA | cs | - | check sixteen | E |
| 2FFB | csu | - | check sixteen unsigned | E |
| 25F6 | cword | - | check word | E |
| 24FC | csngl | - | check single | E |
| 21F3 | csub0 | - | check subscript from 0 | E |
| 24FD | ccnt1 | - | check count from 1 | E |
| 2FF8 | xsword | - | sign extend sixteen to word | |
| 2BF8 | xbword | - | sign extend byte to word | |
| 23FA | xword | - | extend to word | |
| 21FD | xdble | - | extend to double | |

### **Table 6.15 IMS T9000 device access instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2FF0 | devlb | device load byte | M |
| 2FF2 | devls | device load sixteen | M,U |
| 2FF4 | devlw | device load word | M,U |
| 62F4 | devmove | device move | M,I |
| 2FF1 | devsb | device store byte | M |
| 2FF3 | devss | device store sixteen | M,U |
| 2FF5 | devsw | device store word | M,U |

### **Table 6.16 IMS T9000 CRC and bit instructions**

| Memory Code | Mnemonic | Processor cycles | Name | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 27F4 | crcword | 16 | calculate CRC on word | |
| 27F5 | crcbyte | 4 | calculate CRC on byte | |
| 27F6 | bitcnt | 8 | count bits set in word | |
| 27F7 | bitrevword | 1 | reverse bits in word | |
| 27F8 | bitrevnbits | 1 | reverse bottom n bits in word | |

### **Table 6.17 IMS T9000 general instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| F0 | rev | reverse | |
| 25FA | dup | duplicate top of stack | |
| 27F9 | pop | pop processor stack | |
| 63F0 | nop | no operation | |
| 24F2 | mint | minimum integer | |

### **Table 6.18 IMS T9000 timer handling instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 22F2 | ldtimer | load timer | |
| 25F4 | sttimer | store timer | P |
| 22FB | tin | timer input | P,D,I |
| 24FE | talt | timer alt start | P |
| 25F1 | taltwt | timer alt wait | P,D,I |
| 24F7 | enbt | enable timer | P |
| 22FE | dist | disable timer | P,I |

### **Table 6.19 IMS T9000 input/output instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| F7 | in | input message | P,D,I,E,U |
| FB | out | output message | P,D,I,E,U |
| FF | outword | output word | P,D,I,E,U |
| FE | outbyte | output byte | P,D,I,E,U |

### **Table 6.20 IMS T9000 variable length input/output instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2CF0 | ldcent | load message byte count | P,E |
| 61FC | vin | variable-length input message | P,I,E,U,D |
| 61FD | vout | variable-length output message | P,I,E,U,D |

### **Table 6.21 IMS T9000 channel and virtual link instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2CF9 | chantype | channel type | P,E,U |
| 61F6 | initvlcb | initialize VLCB | P,E,U |
| 2CF3 | ldchstatus | load channel status | P,E,U |
| 21F2 | resetch | reset channel | P,E,U |
| 61F7 | setchmode | set channel mode | P,E,U |
| 61FE | stopch | stop virtual channel | P,E,U,D |
| 61F8 | sethdr | set virtual channel header | P,E,U |
| 61F5 | writehdr | write virtual channel header | P,E,U,I |
| 61F4 | readhdr | read virtual channel header | P,E,U,I |
| 2BFC | insphdr | inspect virtual channel header | P,E,U |
| 61F9 | swapbfr | swap buffer pointer in VLCB | P,E,U |
| 2BFD | readbfr | read buffer pointer from VLCB | P,E,U |

### **Table 6.22 IMS T9000 resource channel instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 61F1 | grant | grant resource | P,U,D |
| 61F2 | enbg | enable grant | P,U |
| 61F3 | disg | disable grant | P,U |
| 62F8 | ldresptr | load resource queue pointer | P,E,U |
| 62F9 | stresptr | store resource queue pointer | P,E,U |
| 62FA | erdsq | empty resource data structure queue | P,U |
| 62FB | irdsq | insert at front of RDS queue | P,U |
| 62FC | mkrc | mark resource channel | P,E,U |
| 62FD | unmkrc | unmark resource channel | P,E,U |

### **Table 6.23 IMS T9000 semaphore instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 60F5 | wait | wait | P,U,O,D |
| 60F4 | signal | signal | P,U,O |

### **Table 6.24 IMS T9000 alternative instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 24F3 | alt | alt start | P |
| 24F4 | altwt | alt wait | P,D |
| 24F5 | altend | alt end | P |
| 24F9 | enbs | enable skip | P |
| 23F0 | diss | disable skip | P |
| 24F8 | enbc | enable channel | P,E,U |
| 22FF | disc | disable channel | P,E,U |
| 24FE | talt | timer alt start | P |
| 25F1 | taltwt | timer alt wait | P,D,I |
| 24F7 | enbt | enable timer | P |
| 22FE | dist | disable timer | P,I |
| 61F2 | enbg | enable grant | P,U |
| 61F3 | disg | disable grant | P,U |

### **Table 6.25 IMS T9000 scheduling instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| FD | startp | start process | P,U |
| F3 | endp | end process | P,D,U |
| 23F9 | runp | run process | P |
| 21F5 | stopp | stop process | P,D |
| 21FE | ldpri | load current priority | |

### **Table 6.26 IMS T9000 process queue manipulation and timeslicing instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 60F0 | swapqueue | swap scheduler queue | P |
| 60F1 | swaptimer | swap timer queue | P |
| 60F2 | insertqueue | insert at front of scheduler queue | P |
| 2BF0 | settimeslice | set timeslicing status | P |
| 60F3 | timeslice | timeslice | T,D |

### **Table 6.27 IMS T9000 interrupt instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2CF4 | intdis | interrupt disable | P |
| 2CF5 | intenb | interrupt enable | P |
| 60FE | fpldall | floating point load all | M,U |
| 60FF | fpstall | floating point store all | M,U |
| 61F0 | stmove2dinit | store move2dinit data | M,U |
| 60FC | ldshadow | load shadow registers | P,U |
| 60FD | stshadow | store shadow registers | P,U |

### **Table 6.28 IMS T9000 trap handler instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2CF2 | ldth | load trap handler | P |
| 60F9 | selth | select trap handler | P,D,U |
| 2BF6 | ldflags | load error flags | |
| 2BF7 | stflags | store error flags | |
| 60FA | goprot | go protected | P,U |
| 62FE | restart | restart | P,U |
| 60FB | tret | trap return | P |
| 60F8 | syscall | system call | |
| 62FF | causeerror | cause error | |

### **Table 6.29 IMS T9000 processor initialization instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 22FA | testpranal | test processor analysing | P |
| 25F4 | sttimer | store timer | P |
| 2127FC | lddevid | load device identity | |
| 27FE | ldmemstartval | load value of memstart address | P |
| 6aFC | ldprodid | load product identity | |

### **Table 6.30 IMS T9000 configuration instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2BFE | ldconf | load from configuration register | P,E |
| 2BFF | stconf | store to configuration register | P,E |

### **Table 6.31 IMS T9000 cache instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 62F0 | fdca | flush dirty cache address | M |
| 62F2 | fdcl | flush dirty cache line | P |
| 62F1 | ica | invalidate cache address | M |
| 62F3 | icl | invalidate cache line | P |

### **Table 6.32 IMS T9000 floating point arithmetic instructions**

| Memory Code | Mnemonic | Processor cycles REAL32 | Processor cycles REAL64 | Name | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- |
| 28F7 | fpadd | 2 | 2 | floating point add | i,o,u,x,t,d |
| 28F9 | fpsub | 2 | 2 | floating point subtract | i,o,u,x,t,d |
| 28FB | fpmul | 2 | 3 | floating point multiply | i,o,u,x,t,d |
| 28FC | fpdiv | 8 | 15 | floating point divide | i,z,o,u,x,t,d |
| 2DFB | fpabs | 1 | 1 | floating point absolute value | i,t |
| 2DFA | fpexpinc32 | 2 | 2 | floating point multiply by 232 | i,o,u,x,t,d |
| 2DF9 | fpexpdec32 | 2 | 2 | floating point divide by 232 | i,u,x,t,d |
| 2DF2 | fpmulby2 | 2 | 2 | floating point multiply by 2 | i,o,u,x,t,d |
| 2DF1 | fpdivby2 | 2 | 2 | floating point divide by 2 | i,u,x,t,d |
| 2CFF | fprem | 5-74 | 5-529 | floating point remainder | I,i,u,t,d |
| 2DF3 | fpsqrt | 8 | 15 | floating point square root | i,x,t,d |
| 28FD | fprange | 5-11 | 5-18 | floating point range reduce | i,u,t,d |
| 2DFD | fpadddbsn | 2 | 2 | floating point add double producing single | i,x,o,u,t,d |

### **Table 6.33 IMS T9000 floating point load and store instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 28FE | fpldnlsn | floating point load non-local single | M,U |
| 28FA | fpldnldb | floating point load non-local double | M,U |
| 28F6 | fpldnlsni | floating point load non-local indexed single | M,U |
| 28F2 | fpldnldbi | floating point load non-local indexed double | M,U |
| 29FF | fpldzerosn | load zero single | |
| 2AF0 | fpldzerodb | load zero double | |
| 2AFA | fpldnladdsn | floating point load non-local and add single | M,U,i,o,u,x,t,d |
| 2AF6 | fpldnladddb | floating point load non-local and add double | M,U,i,o,u,x,t,d |
| 2AFC | fpldnlmulsn | floating point load non-local and multiply single | M,U,i,o,u,x,t,d |
| 2AF8 | fpldnlmuldb | floating point load non-local and multiply double | M,U,i,o,u,x,t,d |
| 28F8 | fpstnlsn | floating point store non-local single | M,U |
| 28F4 | fpstnldb | floating point store non-local double | M,U |
| 29FE | fpstnli32 | floating point store non-local int32 | M,U |

### **Table 6.34 IMS T9000 floating point comparison instructions**

| Memory Code | Mnemonic | Processor cycles REAL32 | Processor cycles REAL64 | Name | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- |
| 29F4 | fpgt | 2 | 2 | floating point greater than | i,t,d |
| 29F5 | fpeq | 2 | 2 | floating point equality | i,t,d |
| 29F7 | fpge | 2 | 2 | floating point greater than or equal | i,t,d |
| 29FB | fplg | 2 | 2 | floating point less than or greater than | i,t,d |
| 29F2 | fpordered | 1 | 1 | floating point orderability | i,t |
| 29F1 | fpnan | 1 | 1 | floating point NaN | |
| 29F3 | fpnotfinite | 1 | 1 | floating point not finite | |
| 2DFE | fpchki32 | 2 | 2 | check in range of int32 | i,t |
| 2DFF | fpchki64 | 2 | 2 | check in range of int64 | i,t |

### **Table 6.35 IMS T9000 floating point conversion instructions**

| Memory Code | Mnemonic | Processor cycles | Name | Notes |
| :--- | :--- | :--- | :--- | :--- |
| 2DF7 | fpr32tor64 | 2 | floating point real32 to real64 | i,t,d |
| 2DF8 | fpr64tor32 | 2 | floating point real64 to real32 | i,o,u,x,t,d |
| 29FD | fprtoi32 | 2-4 | real to int32 | i,x,t |
| 29F6 | fpi32tor32 | 2-4 | int32 to real32 | M,U,x |
| 29F8 | fpi32tor64 | 2 | int32 to real64 | M,U |
| 29FA | fpb32tor64 | 2 | bit32 to real64 | M,U |
| 2AF1 | fpint | 2 | round to floating integer | i,x,t |

### **Table 6.36 IMS T9000 floating point general instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2AF4 | fprev | floating point reverse | |
| 2AF3 | fpdup | floating point duplicate | |

### **Table 6.37 IMS T9000 floating point rounding instructions**

| Memory Code | Mnemonic | Name | Notes |
| :--- | :--- | :--- | :--- |
| 2DF0 | fprn | set rounding mode to round nearest | |
| 2DF6 | fprz | set rounding mode to round zero | |
| 2DF4 | fprp | set rounding mode to round plus | |
| 2DF5 | fprm | set rounding mode to round minus | |
# Technical Report: IMS T9000 Registers, Status, and Control Bits

**To**: Hardware Logic Design Lead  
**From**: Systems Architecture Group  
**Date**: July 1, 2025  
**Subject**: Detailed Report on the T9000 Programmer-Visible Register Set and Control Bits

## 1.0 Introduction

This report provides a comprehensive overview of the programmer-visible registers and associated status and control bits of the IMS T9000 transputer, as detailed in Chapter 5 of the T9000 Instruction Set Manual. Understanding this architecture is essential for hardware logic designers implementing mechanisms for context switching, interrupt handling, process protection, and status management. The report covers state registers defining a process's context, shadow registers for interrupts, and non-state system registers for scheduling and timing.

## 2.0 Machine Registers

The T9000's registers are divided into state registers, which define a process's context, and other machine registers, which manage system-wide resources.

### 2.1 State Registers

State registers collectively define the context of the executing process, which must be saved and restored during context switches. The T9000 supports two process modes:

- **L-process (Trusted Mode)**: Standard execution mode.
- **P-process (Protected Mode)**: Restricted mode with memory protection and address translation.

#### 2.1.1 L-Process State Registers

The state of an L-process is defined by the following registers, each with a corresponding shadow register for high-priority interrupts. During an interrupt, hardware atomically copies state register contents to their shadow counterparts.

| **Register** | **Shadow Register** | **Description** |
|--------------|---------------------|-----------------|
| StatusReg | StatusReg.sh | Status Register: Contains process status flags (e.g., error flags) and control bits (e.g., trap enables). |
| WdescReg | WdescReg.sh | Workspace Descriptor Register: 32-bit value combining the workspace pointer and priority bit. |
| IptrReg | IptrReg.sh | Instruction Pointer Register: Points to the next instruction. |
| Areg | Areg.sh | Integer Stack Register A: Top of the integer evaluation stack. |
| Breg | Breg.sh | Integer Stack Register B: Middle of the integer evaluation stack. |
| Creg | Creg.sh | Integer Stack Register C: Bottom of the integer evaluation stack. |
| ThReg | ThReg.sh | Trap-handler Register: Pointer to the Trap-Handler Data Structure (THDS). |
| FPstatusReg | FPstatusReg.sh | Floating-Point Status Register: Contains precision tags for FP stack and rounding mode. |
| FPAreg | FPAreg.sh | Floating-Point Stack Register A: Top of the FP evaluation stack. |
| FPBreg | FPBreg.sh | Floating-Point Stack Register B: Middle of the FP evaluation stack. |
| FPCreg | FPCreg.sh | Floating-Point Stack Register C: Bottom of the FP evaluation stack. |
| BMreg0 | BMreg0.sh | 2D Block Move Control Register 0: Stores row length for 2D block move. |
| BMreg1 | BMreg1.sh | 2D Block Move Control Register 1: Stores destination stride for 2D block move. |
| BMreg2 | BMreg2.sh | 2D Block Move Control Register 2: Stores source stride for 2D block move. |
| WlReg | WlReg.sh | Watchpoint Lower Bound Register: Start address of the watchpoint region. |
| WuReg | WuReg.sh | Watchpoint Upper Bound Register: End address of the watchpoint region. |
| EptrReg | EptrReg.sh | Error Pointer Register: Address of the instruction causing a trap. |
| Ereg, Xreg | Ereg.sh, Xreg.sh | Internal Registers: Temporary storage during interruptible instructions, preserved across interrupts. |

#### 2.1.2 P-Process Additional State Registers

P-processes include additional registers for memory management, also shadowed during interrupts.

| **Register** | **Shadow Register** | **Description** |
|--------------|---------------------|-----------------|
| RegionReg0-3 | RegionReg0.sh-3.sh | Region Descriptor Registers: Define size, position, physical mapping, and access permissions for four memory protection regions. |
| PstateReg | PstateReg.sh | Protected State Register: Pointer to the P-state Data Structure (PDS) for saved P-process state. |
| WdescStubReg | WdescStubReg.sh | Workspace Descriptor Stub Register: Contains the descriptor of the supervisor L-process controlling the P-process. |

### 2.2 Other Machine Registers

These registers manage system resources and are not part of a process’s context, thus not saved during context switches or shadowed.

| **Register** | **Description** |
|--------------|-----------------|
| FptrReg0 | High Priority Front Pointer Register: Points to the first process on the high-priority scheduling list. |
| BptrReg0 | High Priority Back Pointer Register: Points to the last process on the high-priority scheduling list. |
| FptrReg1 | Low Priority Front Pointer Register: Points to the first process on the low-priority scheduling list. |
| BptrReg1 | Low Priority Back Pointer Register: Points to the last process on the low-priority scheduling list. |
| ClockReg0 | High Priority Clock Register: System timer for high-priority processes, increments every 1µs. |
| ClockReg1 | Low Priority Clock Register: System timer for low-priority processes, increments every 64µs. |
| TptrReg0 | High Priority Timer List Pointer Register: Points to the head of the high-priority timer queue. |
| TptrReg1 | Low Priority Timer List Pointer Register: Points to the head of the low-priority timer queue. |
| TnextReg0 | High Priority Alarm Register: Caches the time of the next event on the high-priority timer queue. |
| TnextReg1 | Low Priority Alarm Register: Caches the time of the next event on the low-priority timer queue. |

## 3.0 Process Status and Control Bits (StatusReg)

The 32-bit StatusReg is a critical component of process state, containing status flags (reflecting past operations) and control bits (determining future behavior). It is loaded from a process’s THDS or PDS upon execution and saved back during traps. Hardware must implement logic for setting, clearing, and acting on these bits.

| **Bit** | **Name** | **Type** | **Description** |
|---------|----------|----------|-----------------|
| 2 | sb.FPErrorFlag | Flag | Set on general floating-point error (T805 compatibility). |
| 3 | sb.FPErrorTeBit | Control | If set, a trap is taken when FPError is signalled. |
| 6 | sb.IntOvFlag | Flag | Set on integer overflow or divide-by-zero. |
| 7 | sb.IntOvTeBit | Control | If set, a trap is taken when IntegerOverflow is signalled. |
| 8 | sb.FPInOpFlag | Flag | Set on IEEE 'invalid operation'. |
| 9 | sb.FPInOpTeBit | Control | If set, a trap is taken when FPInvalidOp is signalled. |
| 10 | sb.FPDivByZeroFlag | Flag | Set on IEEE 'divide by zero'. |
| 11 | sb.FPDivByZeroTeBit | Control | If set, a trap is taken when FPDivideByZero is signalled. |
| 12 | sb.FPOvFlag | Flag | Set on IEEE 'overflow'. |
| 13 | sb.FPOvTeBit | Control | If set, a trap is taken when FPOverflow is signalled. |
| 14 | sb.FPUndFlag | Flag | Set on IEEE 'underflow'. |
| 15 | sb.FPUndTeBit | Control | If set, a trap is taken when FPUnderflow is signalled. |
| 16 | sb.FPInexFlag | Flag | Set on IEEE 'inexact result'. |
| 17 | sb.FPInexTeBit | Control | If set, a trap is taken when FPInexact is signalled. |
| 19 | sb.UnalignTeBit | Control | If set, a trap is taken on misaligned memory access; otherwise, the address is corrected. |
| 25 | sb.TimesliceDisabledBit | Control | If set, the current low-priority process is not timesliced. |
| 26 | sb.StepBit | Control | If set, a single-step trap is taken after the current instruction. |
| 27 | sb.IsPprocessBit | Status | Set by hardware to indicate protected mode. |
| 29 | sb.WtchPntEnbl | Control | If set, watchpointing is enabled for the region defined by WlReg and WuReg. |
| 30 | sb.WtchPntPend | Flag | Set by hardware on watchpoint write, with a trap taken at instruction end. |

### 3.1 Default Control Word

For an L-process with a null trap-handler, StatusReg is loaded with a default control word where all bits are 0 except `sb.IntOvTeBit` (bit 7), set to 1, making integer overflow a trapping condition by default.

## 4.0 The Process Descriptor (WdescReg)

The 32-bit WdescReg uniquely identifies the executing process, containing:

- **Workspace Pointer (Wptr)**: Bits 31–2 form a 30-bit word-aligned address of the process’s workspace.
- **Priority**: Bit 0 (0 for high priority, 1 for low priority).
- **Reserved**: Bit 1 is always 0.

```
 31                                         2   1   0
 +--------------------------------------------+---+---+
 |             Workspace Address              | 0 | P |
 +--------------------------------------------+---+---+
                                                  ^
                                                  |
                                               Priority
```

Hardware uses the Wptr for local memory accesses (e.g., `ldl`, `stl`) and the Priority bit to select appropriate process queues (FptrReg0/1, BptrReg0/1) and timers (ClockReg0/1) during scheduling.
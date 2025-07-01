# Technical Report: IMS T9000 Process Control, Protection, and Trap Mechanisms

**To**: Hardware Logic Design Lead  
**From**: Systems Architecture Group  
**Date**: July 1, 2025  
**Subject**: Detailed Hardware Implementation Guide for T9000 Concurrency, Memory Protection, and Trap Systems

## 1.0 Executive Summary

This report provides a detailed specification for implementing the IMS T9000 transputer's concurrent process scheduler, protected-mode memory management unit, and unified trap-handling mechanism. These interconnected systems underpin the T9000's capabilities as a secure, real-time, parallel processing platform. Derived from Chapters 8, 9, and 10 of the T9000 Instruction Set Manual, this guide outlines programmer-visible registers, memory-based data structures, and operational sequences for process scheduling, context switching, memory protection, address translation, and exception handling. It is intended for the hardware logic design team responsible for the core CPU and control units.

## 2.0 Chapter 8: Concurrent Process Implementation

The T9000 is designed for communicating sequential processes, requiring efficient, low-latency hardware mechanisms for scheduling and managing concurrent processes.

### 2.1 Process Scheduling and Priority

The hardware scheduler supports two priority levels:

- **High Priority (Priority 0)**: Processes run to completion or until blocked by communication or timer events, without timeslicing.
- **Low Priority (Priority 1)**: Processes share processor time via preemptive timeslicing.

**Scheduling Registers**:

| **Register** | **Description** |
|--------------|-----------------|
| FptrReg0 | High Priority Front Pointer: Points to the first process on the high-priority scheduling list. |
| BptrReg0 | High Priority Back Pointer: Points to the last process on the high-priority scheduling list. |
| FptrReg1 | Low Priority Front Pointer: Points to the first process on the low-priority scheduling list. |
| BptrReg1 | Low Priority Back Pointer: Points to the last process on the low-priority scheduling list. |

**Hardware Logic for Scheduling**:
- Read the priority bit from `WdescReg`.
- Select the appropriate queue (`FptrReg0/1`).
- If the queue is empty (`FptrReg` holds `NotProcess.p`), switch to the other priority queue.
- Dequeue the front process by loading its workspace descriptor into `WdescReg` and instruction pointer from `pw.Iptr` into `IptrReg`.
- Update the queue’s front pointer to the next process.

### 2.2 Process Workspace and Data Structure

A non-executing process’s state is stored in its workspace, accessed via negative offsets from the workspace pointer (`Wptr`).

**Workspace Slots**:

| **Offset** | **Slot Name** | **Hardware Function** |
|------------|---------------|-----------------------|
| -1 | pw.Iptr | Stores `IptrReg` when descheduled. |
| -2 | pw.Link | Points to the next process in a scheduling or timer list. |
| -3 | pw.TrapHandler | Points to the Trap-Handler Data Structure (THDS). |
| -4 | pw.Pointer | Points to a message buffer during communication. |
| -4 | pw.State | Stores the state of an ALT construct. |
| -5 | pw.Length | Stores received message length for a `vin` instruction. |
| -5 | pw.TLink | Points to the next process in a timer list. |
| -6 | pw.Time | Stores the alarm time for a process waiting on a timer. |

### 2.3 Timeslicing and Timers

The T9000 uses two 32-bit clocks and timer lists for real-time operations and timeslicing:

- **Clocks**: `ClockReg0` (1µs tick, high-priority), `ClockReg1` (64µs tick, low-priority).
- **Timer Lists**: `TptrReg0` and `TptrReg1` point to ordered lists of processes waiting for specific times. `TnextReg0` and `TnextReg1` cache the next alarm time.

**Hardware Logic for Timeslicing**:
- A counter driven by the high-priority clock tracks the 256µs timeslice period.
- After two periods of uninterrupted low-priority process execution, force a deschedule at the next timeslicing point (e.g., `j` or `lend` instruction).
- Save the current process state and place it at the back of the low-priority queue (`BptrReg1`).

### 2.4 Semaphores

The hardware supports n-valued semaphores via `wait` and `signal` instructions, using a 3-word memory structure.

**Semaphore Data Structure**:

| **Offset** | **Slot Name** | **Hardware Function** |
|------------|---------------|-----------------------|
| 0 | s.Count | Semaphore count; `wait` decrements, `signal` increments. |
| 1 | s.Front | Points to the head of the blocked process queue. |
| 2 | s.Back | Points to the tail of the blocked process queue. |

**Hardware Logic**:
- **wait**: If `s.Count > 0`, decrement it. If `s.Count == 0`, deschedule the process and add it to the semaphore’s queue using `s.Front` and `s.Back`.
- **signal**: If the queue is not empty, dequeue the process at `s.Front` and add it to the scheduling list. If empty, increment `s.Count`.

## 3.0 Chapter 9: Protection and Memory Management

The T9000’s protection mechanism ensures safe execution of untrusted code via P-processes (Protected Processes), controlled by a supervisor L-process.

### 3.1 The Protection Mechanism

When executing a P-process (`sb.IsPprocessBit` set in `StatusReg`), the hardware enforces:

- **Instruction Protection**: Privileged instructions (e.g., scheduling, communication) trigger a `PrivInstruction` trap.
- **Memory Protection**: Validates all memory accesses against permissions.
- **Address Translation**: Translates logical addresses to physical addresses.

### 3.2 Regions and Region Descriptors

The logical address space is divided into four quarters, each with a contiguous memory region defined by `RegionReg0–3`.

**Hardware Logic for Memory Access**:
- Select a `RegionReg` using the top two bits of the logical address.
- Check permissions (read, write, instruction fetch) against the descriptor. Trigger an `AccessViolation` trap if invalid.
- Validate the logical address against the region’s size and position. Trigger an `AccessViolation` trap if out of bounds.
- Translate to a physical address by combining the descriptor’s relocation field with the low-order bits of the logical address.

This sequence must be performed in the pipeline for every P-process memory access.

### 3.3 Key Data Structures and Registers

**Data Structures**:
- **P-state Data Structure (PDS)**: Stores P-process state during traps.
- **Region Descriptor Data Structure (RDDS)**: 4-word structure holding initial values for `RegionReg0–3`.

**Registers**:
- `PstateReg`: Points to the PDS.
- `WdescStubReg`: Holds the supervisor L-process’s descriptor.
- `RegionReg0–3`: Define memory regions.

**Hardware Logic for `goprot`**:
- Save the supervisor L-process state.
- Load `PstateReg` with the PDS address.
- Load `WdescStubReg` with the supervisor’s descriptor.
- Load `RegionReg0–3` from the RDDS.
- Load the P-process context from the PDS.
- Set `sb.IsPprocessBit` to 1.
- Begin P-process execution.

## 4.0 Chapter 10: The Trap Mechanism

The trap mechanism handles all exceptional events, from errors to system calls, via a unified system.

### 4.1 L-Process vs. P-Process Traps

- **L-process Trap**: Transfers control to a trap-handler.
- **P-process Trap**: Transfers control to the supervisor L-process.

### 4.2 The Trap-Handler Data Structure (THDS)

The 12-word THDS, pointed to by `ThReg`, stores trap-related information for L-processes.

**THDS Slots**:

| **Offset** | **Slot Name** | **Hardware Function** |
|------------|---------------|-----------------------|
| 0 | th.Cntl | Saves `StatusReg` (flags and control bits). |
| 1 | th.Iptr | Loads `IptrReg` with the trap-handler’s entry point. |
| 2–3 | th.Fptr, th.Bptr | Queue pointers for processes sharing the trap-handler. |
| 4 | th.Eptr | Saves `EptrReg` (trapping instruction address). |
| 5–6 | th.eWI, th.eWu | Watchpoint region bounds. |
| 7 | th.sWptr | Saves the process’s `Wptr`. |
| 8 | th.sIptr | Saves `IptrReg`. |
| 9–11 | th.sAreg, th.sBreg, th.sCreg | Saves integer stack registers. |

### 4.3 Hardware Trap Sequence (L-Process)

For an L-process trap, the hardware performs an uninterruptible sequence:

1. **Save State**: Write `Wptr`, `IptrReg`, `Areg`, `Breg`, `Creg`, and `EptrReg` to THDS slots.
2. **Save Status**: Write `StatusReg` to `th.Cntl`.
3. **Set "In-Use" Flag**: Set `sb.ThInUse` in `th.Cntl` to prevent re-entrant traps.
4. **Install Handler Context**:
   - Load `Wptr` with the THDS address (from `ThReg`).
   - Load `IptrReg` with the handler’s entry point from `th.Iptr`.
5. **Deliver Trap Information**:
   - Load `Areg` with a 5-bit trap reason (e.g., error, syscall).
   - Load `Breg` with an error type code if applicable.
6. **Begin Handler Execution**: Fetch instructions from the new `IptrReg`.

### 4.4 Trap Causes and Prioritization

The hardware detects trap-causing events, encoding multiple simultaneous events into a single 5-bit trap reason in `Areg`, following Table 10.3 of the ISM.

**Trap Causes**:

| **Cause** | **Priority** | **Description** |
|-----------|--------------|-----------------|
| Error | Highest | Hardware-detected errors (e.g., IntegerOverflow, AccessViolation). |
| Breakpoint (j 0) | High | Breakpoint instruction execution. |
| causeerror | High | Programmatic error trigger. |
| syscall | High | System call instruction. |
| Watchpoint | Medium | Write to a watchpointed memory region. |
| Single-Step | Low | `sb.StepBit` set, instruction completed. |
| Timeslice | Lowest | P-process timeslice expiration. |

Prioritization ensures critical events (errors) are reported first.

## 5.0 Conclusion

Implementing the T9000’s concurrency, protection, and trap-handling systems requires managing multiple register sets, atomic manipulation of memory-based data structures (VLCB, PDS, THDS, RDDS), and precise state-switching sequences. A successful implementation will deliver a high-performance, secure, and robust processor suitable for real-time and parallel operating systems.
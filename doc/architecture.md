# T800 Core Planning

This document sketches the T800 plugin-based architecture and the CPU pipeline.

## Pipeline stages

A five-stage `StageCtrlPipeline` is created by `PipelinePlugin`:

1. **Fetch** – issue address to instruction memory and insert fetched byte.
2. **Decode** – translate opcode to internal control signals.
3. **Execute** – perform ALU/FPU operations.
4. **Memory** – data memory accesses.
5. **WriteBack** – final register updates or scheduler handover.

The pipeline DSL ensures stall/flush control between stages.

## Plugin responsibilities

- `FetchPlugin` – maintains the PC and fetches bytes from `InstrFetchSrv`.
- `PrimaryInstrPlugin` – implements the 16 primary instructions.
- `SecondaryInstrPlugin` – handles the extended OPR instruction set.
- `MemoryPlugin` – simple on-chip ROM and RAM. Provides `InstrFetchSrv` and
  `DataBusSrv`.
- `FpuPlugin` – placeholder FPU interface; future variants may swap a different
  implementation.
- `SchedulerPlugin` – handles process scheduling and timers.
- `StackPlugin` – the A/B/C stack registers.
- `TimerPlugin` – provides high and low priority timer counters.

Each plugin exposes a tiny service API so others can request operations without
directly touching internal signals. The fetch and execute stages rely on the
instruction and data bus services implemented by `MemoryPlugin`. When the FPU is
enabled, the execute stage issues commands through `FpuPlugin`'s pipeline
interface and waits for results using the pipeline DSL's `haltWhen` mechanism.

`TopVerilog` builds a single plugin set for simulation and synthesis.

## Integration concepts

The memory and FPU plugins expose lightweight services so other subsystems can
issue commands without direct coupling. The pipeline service exposes `Node`
handles used by plugins to insert or consume payloads. Coordination between
stages uses `haltWhen` and `insert` from `spinal.lib.misc.pipeline`.

Each pipeline stage receives a `CtrlLink` from `PipelinePlugin`. Plugins attach
their logic to these links, inserting or reading instruction payloads and
stalling downstream stages when resources are busy. For example the execute
stage halts while waiting for a memory read or FPU result. This approach keeps
all components loosely coupled while still allowing deterministic flow control.

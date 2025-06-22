# Transputer Core

[![CI](https://github.com/agentdavo/t800/actions/workflows/ci.yml/badge.svg)](https://github.com/agentdavo/t800/actions/workflows/ci.yml)

*(rev 2025-06-17 – includes plugin & pipeline details)*

---

SpinalHDL ≥ 1.9 • Plugin architecture • Pipeline DSL • Fiber tasks

This repo re-implements the Transputer CPU using modern SpinalHDL.
Each subsystem lives in its own `FiberPlugin` and communicates via small
"services". Pipelines are assembled with the DSL from SpinalLib.

This project re-implements the Transputer CPU in modern SpinalHDL.

* **Plugins** – every subsystem (FPU, Scheduler …) is a hot-swappable `FiberPlugin`.
* **Pipeline DSL** – build safe, stall/flush-aware pipelines with one-liners.
* **Fibers** – allow out-of-order elaboration so plugins can depend on each other.
* **API reference** -- see `doc/SpinalHDL_api.md` for pipeline, fiber, and plugin APIs.
* **Fiber phases** – each plugin runs a `setup` block before hardware `build`; use `awaitBuild()` or `buildBefore()` to coordinate ordering.

---

## Feature matrix & milestone plan

| Milestone | Deliverable | Primary DSL objects | Status |
|-----------|-------------|---------------------|--------|
| **M-1** | OPR `REV ADD SUB AND XOR` | `StageCtrlPipeline`, `haltIt` | ⏳ |
| **M-2** | Literal builder + `LDL 0-15` | same | ⏳ |
| **M-3** | RAM `STL/LDL`, on-chip stack | `S2MLink` | ⏳ |
| **M-4** | Two-queue scheduler, `STARTP/ENDP` | `ForkLink` | ⏳ |
| **M-5** | 64-bit timer + `TIMERWAIT` | `JoinLink` | ⏳ |
| **M-6** | Two-cycle FP adder (plugin) | plugin swap | ⏳ |

Detailed specs live in **AGENTS.md §5**.

---

## Plugin host vs Pipeline DSL

| Layer | Purpose | Key classes |
|-------|---------|-------------|
| **Plugin host** | Compose or swap whole subsystems. | `PluginHost`, `FiberPlugin`, `Plugin[T]` |
| **Pipeline DSL** | Build pipelines without manual `valid/ready` wiring. | `Node`, `StageLink`, `CtrlLink`, `CtrlLaneApi` |

The host now runs inside a `Database` context so plugins can share typed metadata.

Why the DSL helps:

* Insert or remove a register: swap `DirectLink` → `StageLink`.
* Stalls & flushes: `haltIt()`, `throwIt()` – no custom FSM.
* Skid buffers, broadcast, join – pre-built (`S2M`, `Fork`, `Join`).

Quick cheat-sheet in **AGENTS.md §8**.

---

## Repository layout

```

t800/
├─ build.sbt
├─ src/
│  ├─ main/scala/transputer/
│  │  ├─ Top.scala              # creates Database + PluginHost, selects plugins
│  │  └─ plugins/               # ⇐ one subfolder per FiberPlugin
│  │     ├─ cache/
│  │     ├─ decode/
│  │     ├─ execute/
│  │     ├─ fetch/
│  │     ├─ fpu/
│  │     ├─ grouper/
│  │     ├─ mmu/
│  │     ├─ pipeline/
│  │     ├─ pmi/
│  │     ├─ registers/
│  │     ├─ schedule/
│  │     ├─ stack/
│  │     ├─ timers/
│  │     ├─ transputer/
│  │     └─ vcp/
│  └─ test/scala/transputer/
│      ├─ TransputerCoreSim.scala
│      └─ ...
├─ ext/
│  └─ SpinalHDL/                # git sub-module (optional)
├─ doc/
│  ├─ SpinalHDL_docs.txt        # SpinalSim + SpinalHDL documentation
│  ├─ SpinalHDL_api.md          # API reference and DSL guide
│  ├─ SpinalHDL_bmb.md          # overview of the BMB bus
│  ├─ Transputer_core.md        # core architecture notes
│  └─ Transputer_links.md       # link interface description
├─ README.md
└─ AGENTS.md

````

---

## Service naming strategy

SpinalHDL services describe shared functionality between plugins. To keep the
pipeline consistent:

1. **Descriptive names** – use clear names like `FetchService` or
   `FpuControlService` that reveal purpose and stage.
2. **Service suffix** – every trait ends with `Service`; avoid abbreviations
   such as `Srv`.
3. **Stage alignment** – name the service after the pipeline stage it targets
   (Fetch, Decode, Execute, Memory, Writeback). Prefix with the subsystem name
   if it spans multiple stages.
4. **Avoid overlap** – reuse existing services and keep names unique.
5. **SpinalHDL conventions** – define services in `transputer.plugins.<subsystem>`
    packages and use camelCase for methods.

---

## Quick start

```bash
git clone --recursive https://github.com/agentdavo/t800.git
cd t800

# Default plugin set
sbt scalafmtAll
sbt test
sbt "runMain transputer.Generate --word-width 32 --link-count 4 --fpu true"

# Parameters
--word-width    CPU data width in bits
--link-count    Number of communication links
--fpu           Enable or disable the floating-point unit (Generate.scala flag)
```

---

## Synthesis

Run `sbt synth` to generate a bitstream for Lattice ECP5 targets. The task calls
`gen/scripts/synth.tcl`, which expects a constraint LPF and device string. The
default setup uses `gen/constraints/ecp5.lpf` and `LFE5U-45F`. Ensure
`nextpnr-ecp5` and `trellis` are installed as shown in the CI workflow.

---

## Contributing

1. Pick a milestone from **AGENTS.md §5**.
2. Work **inside** `src/main/scala/transputer/plugins/<name>/` for your plugin.
3. Run `sbt scalafmtAll` and keep CI green:

```bash
sbt scalafmtAll
sbt test
```

PR title `[M-n] <topic>` – e.g. `[M-1] ALU ADD`.

---

## Hierarchy violations

SpinalHDL enforces strict ownership rules. A signal can only be read within the
component where it is defined or from its children. Assignments are only allowed
in that component or to outputs of children. Breaking these rules triggers a
`Hierarchy Violation` error during elaboration. Expose required signals via
services or bundles instead of cross-plugin references.

## SpinalHDL design checks

The compiler catches a wide range of design mistakes:

* Assignment overlapping
* Clock domain crossing mistakes
* Hierarchy violations
* Combinatorial loops
* Latches
* Undriven signals
* Width mismatches
* Unreachable switch statements

Each report includes a stack trace to pinpoint the offending code.

### Common runtime errors

Scala executes the hardware description before Verilog generation. Assigning to a
signal prior to its `val` declaration triggers a `NullPointerException` during
elaboration. Always declare hardware objects before driving them.

---

## Simulation with SpinalSim

The `src/test/scala/transputer` directory contains ScalaTest benches that use SpinalHDL's
simulation API. A simple template is:

```scala
import spinal.core._
import spinal.core.sim._

SimConfig
  .withWave
  .withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz)))
  .compile(new TopLevel)
  .doSim { dut =>
    SimTimeout(1000)
    dut.clockDomain.forkStimulus(10)
    // Stimulus and checks here
  }
```

`withWave` records a VCD/FST waveform under `simWorkspace/`. Use `SimTimeout`
so long-running tests fail deterministically. Additional back‑ends like GHDL or
Icarus can be selected via `withGhdl` or `withIVerilog`.

For more advanced features, see `doc/SpinalHDL_docs.txt`.
The BMB bus is described in `doc/SpinalHDL_bmb.md`.

The SpinalHDL API reference is maintained in `doc/SpinalHDL_api.md`.
### Debugging tips

Simulation artifacts live in `simWorkspace/`. Waveforms and log files are
written under the chosen workspace directory, making it easy to inspect multiple
test runs. Set the `SPINALSIM_WORKSPACE` environment variable to redirect logs
and waves. `SimConfig.setTestPath("/tmp")` changes the per-test directory, and
you can query it with `currentTestPath()` during execution. When chasing
intermittent failures, use `DualSimTracer` to record only a short window before
the crash.
Spawn helper threads with `fork { ... }` and block on events using `sleep(n)` or
`waitUntil(cond)`. ClockDomain utilities such as `waitRisingEdge()` help align
checks with clock boundaries. See **AGENTS.md §12** for common runtime errors.
* `clockDomain.forkSimSpeedPrinter(printPeriod)` prints the simulation speed;
  see `doc/SpinalHDL_docs.txt` for details.

See `doc/Transputer_core.md` for a quick overview of the architecture.
An overview of the link services and the upcoming VCP design lives in
`doc/Transputer_links.md`.
The BMB bus and its helpers are covered in `doc/SpinalHDL_bmb.md`.
`HelloWorldSpec` is currently marked with `ignore` until the channel hardware is complete.

---

## License

This project is released under the [MIT License](LICENSE).

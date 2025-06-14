# Transputer-T800 Core

*(rev 2025-06-12 – includes plugin & pipeline details)*

---

SpinalHDL ≥ 1.9 • Plugin architecture • Pipeline DSL • Fiber tasks

This repo re-implements the INMOS T800/T9000 CPU using modern SpinalHDL.
Each subsystem lives in its own `FiberPlugin` and communicates via small
"services". Pipelines are assembled with the DSL from SpinalLib.

This project re-implements the INMOS T800/T9000 CPU in modern SpinalHDL.

* **Plugins** – every subsystem (FPU, Scheduler …) is a hot-swappable `FiberPlugin`.
* **Pipeline DSL** – build safe, stall/flush-aware pipelines with one-liners.
* **Fibers** – allow out-of-order elaboration so plugins can depend on each other.
* **API reference** -- see `docs/spinalAPI.md` for pipeline, fiber, and plugin APIs.
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
│  ├─ main/scala/t800/
│  │  ├─ Top.scala              # creates Database + PluginHost, selects plugins
│  │  └─ plugins/               # ⇐ one subfolder per FiberPlugin
│  │     ├─ fetch/              # FetchPlugin + Service.scala
│  │     ├─ cache/              # MainCachePlugin, WorkspaceCachePlugin
│  │     ├─ decode/             # PrimaryInstrPlugin
│  │     ├─ execute/            # SecondaryInstrPlugin
│  │     ├─ schedule/           # SchedulerPlugin
│  │     ├─ fpu/                # FpuPlugin
│  │     ├─ grouper/            # InstrGrouperPlugin
│  │     ├─ timers/             # TimerPlugin
│  │     ├─ transputer/         # TransputerPlugin
│  │     └─ ...
│  └─ test/scala/t800/
│      ├─ T800CoreSim.scala
│      └─ ...
├─ ext/
│  └─ SpinalHDL/                # git sub-module (optional)
├─ doc/
│  ├─ spinalHDL.txt             # SpinalSim + SpinalHDL documentation
│  └─ bmb.md                    # overview of the BMB bus
├─ README.md
└─ AGENTS.md

````

---

## Ubuntu setup

```bash
#!/usr/bin/env bash
set -euo pipefail

echo "=== Installing T800 build environment ==="
apt-get update
apt-get install -y software-properties-common curl gnupg2

add-apt-repository -y ppa:openjdk-r/ppa
apt-get update

curl -fsSL https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
  | gpg --dearmor -o /usr/share/keyrings/sbt.gpg
echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
  > /etc/apt/sources.list.d/sbt.list

apt-get update
apt-get install -y \
  openjdk-21-jdk sbt gtkwave git make autoconf g++ flex bison \
  help2man device-tree-compiler libboost-all-dev wget

tmp=/tmp/verilator
git clone --depth 1 https://github.com/verilator/verilator.git "$tmp"
cd "$tmp" && autoconf && ./configure && make -j$(nproc) && make install
rm -rf "$tmp"

java  -version | head -n1
sbt   --script-version
verilator --version

Run `sudo ./scripts/setup_env.sh` to execute these steps.

````

---

## Quick start

```bash
git clone --recursive https://github.com/agentdavo/t800.git
cd t800

# Ensure the bundled SpinalHDL submodule is present when
# using `SPINALHDL_FROM_SOURCE=1`.
git submodule update --init --recursive
./scripts/check-submodules.sh

# Install Java, sbt and Verilator once via helper script
sudo ./scripts/setup_env.sh

# Default plugin set
sbt scalafmtAll
sbt test
sbt "runMain t800.Generate --word-width 32 --link-count 4 --fpu true"

# Parameters
--word-width    CPU data width in bits
--link-count    Number of communication links
--fpu           Enable the floating-point unit

# To build against the bundled SpinalHDL sources instead of published
# artifacts, run with `SPINALHDL_FROM_SOURCE=1`:
# SPINALHDL_FROM_SOURCE=1 sbt test
```

---

## Contributing

1. Pick a milestone from **AGENTS.md §5**.
2. Work **inside** `src/main/scala/t800/plugins/<name>/` for your plugin.
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

The `src/test/scala` directory contains ScalaTest benches that use SpinalHDL's
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

For more advanced features, see `doc/spinalHDL.txt`.
The BMB bus is described in `doc/bmb.md`.

The SpinalHDL API reference is maintained in `docs/spinalAPI.md`.
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
  see `doc/spinalHDL.txt` for details.

See `doc/hello_world.md` for a quick overview, and `doc/helloworld.md` for the full source listing.
An overview of the link services and the upcoming VCP design lives in
`doc/link_architecture.md`.
The BMB bus and its helpers are covered in `doc/bmb.md`.
`HelloWorldSpec` is currently marked with `ignore` until the channel hardware is complete.

---

## License

This project is released under the [MIT License](LICENSE).

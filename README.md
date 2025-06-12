# Transputer-T800 Core

*(rev 2025-06-12 – includes plugin & pipeline details)*

---

SpinalHDL ≥ 1.9 • Plugin architecture • Pipeline DSL • Fiber tasks

This project re-implements the INMOS T800/T9000 CPU in modern SpinalHDL.

* **Plugins** – every subsystem (FPU, Scheduler …) is a hot-swappable `FiberPlugin`.
* **Pipeline DSL** – build safe, stall/flush-aware pipelines with one-liners.
* **Fibers** – allow out-of-order elaboration so plugins can depend on each other.

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

Why the DSL helps:

* Insert or remove a register: swap `DirectLink` → `StageLink`.
* Stalls & flushes: `haltIt()`, `throwIt()` – no custom FSM.
* Skid buffers, broadcast, join – pre-built (`S2M`, `Fork`, `Join`).

Quick cheat-sheet in **AGENTS.md §8**.

---

## Repository layout

```

transputer-t800/
├─ build.sbt
├─ src/
│  ├─ main/scala/t800/
│  │  ├─ Top.scala              # creates PluginHost + selects plugins
│  │  └─ plugins/               # ⇐ one FiberPlugin per subsystem
│  │     ├─ FpuPlugin.scala
│  │     ├─ SchedulerPlugin.scala
│  │     └─ ...
│  └─ test/scala/t800/
│      ├─ T800CoreSim.scala
│      └─ ...
├─ ext/
│  └─ SpinalHDL/                # git sub-module (optional)
├─ README.md
└─ AGENTS.md

````

---

## Ubuntu setup

Save as `setup-t800.sh`, then `sudo bash setup-t800.sh`.

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
````

---

## Quick start

```bash
git clone --recursive https://github.com/agentdavo/t800.git
cd transputer-t800

# Default plugin set
sbt test
sbt "runMain t800.TopVerilog"

# Integer-only build (nofpu)
sbt "runMain t800.TopVerilog --variant=min"
```

---

## Contributing

1. Pick a milestone from **AGENTS.md §5**.
2. Work **inside** `src/main/scala/t800/plugins/`.
3. Keep CI green:

```bash
sbt test
```

PR title `[M-n] <topic>` – e.g. `[M-1] ALU ADD`.

---
# Contributor Guide

This file gives every contributor the shared context needed to work in this repository.  
If deeper folders add their own `AGENTS.md`, the most nested copy applies.

---

## 1 Repository overview

| Path | Purpose |
|------|---------|
| `src/main/scala/t800/T800Core.scala` | Monolithic SpinalHDL core skeleton |
| `src/test/scala/t800/` | Unit tests & SpinalSim benches |
| `SpinalHDL/` | **Git sub-module** – local SpinalHDL sources (used when `SPINALHDL_FROM_SOURCE=1`) |
| `doc/` | Architecture notes, opcode sheets, ADRs |
| `synthesis/` | FPGA scripts & timing reports |
| `.github/workflows/` | CI (lint → test → Verilog → nightly synth) |
| `README.md` | User-facing quick-start |
| `AGENTS.md` | *this guide* |

---

## 2 Dev environment

### 2.1 Clone with sub-module

```bash
git clone --recursive https://github.com/agentdavo/t800.git
git submodule update --init --recursive   # run again whenever the sub-module pointer changes
````

If you prefer published binaries, set:

```bash
export SPINALHDL_FROM_SOURCE=0   # sbt will ignore the SpinalHDL/ folder
```

### 2.2 Every-day commands

```bash
sbt scalafmtCheckAll   # style
sbt test               # unit + sim benches
sbt "runMain t800.T800CoreVerilog"   # Verilog
```

---

## 3 Style & contribution rules

* **SpinalHDL ≥ 1.9**, Scala 2.13.
* One RTL statement per line; delete any `// TODO` you resolve.
* CamelCase for regs/wires; `UPPER_SNAKE` for constants.
* New hand-shake bundles extend `IMasterSlave`.
* Keep coverage of touched files ≥ 80 %.
* Remember: **SpinalHDL semantics differ from normal Scala** (last-assignment-wins, elaboration unrolls loops, etc.).

---

## 4 Validation checklist

| Step             | Command                              |
| ---------------- | ------------------------------------ |
| Style            | `sbt scalafmtCheckAll`               |
| Tests            | `sbt test`                           |
| Verilog          | `sbt "runMain t800.T800CoreVerilog"` |
| Timing (nightly) | `nextpnr-ecp5 …`                     |

---

## 5 Ownership matrix

| Area              | Maintainers       |
| ----------------- | ----------------- |
| Core architecture | Architecture team |
| Integer ALU       | ALU team          |
| Scheduler & Timer | Flow team         |
| FPU               | FPU team          |
| Transcendentals   | Math team         |
| Link engines      | Links team        |
| Tests & CI        | QA team           |
| PPA               | PPA team          |

Cross-area edits → draft PR, tag relevant team.

---

## 6 PR template

```markdown
### What & Why
* Implement opcode 0x30 (OPR-REV)
* Added spec `OprRevSpec`

### Validation
- [x] scalafmtCheckAll
- [x] sbt test

Closes #42
```

Branch name: `feat/<topic>` (e.g. `feat/alu-opr-add`).

---

## 7 SpinalHDL ≠ normal Scala (cheat-sheet)

| Scala view                        | Hardware reality                             |
| --------------------------------- | -------------------------------------------- |
| Statement order                   | Unordered; each `:=` adds a rule (last wins) |
| Loops / functions                 | Elaborated once, unrolled                    |
| `Reg(UInt())`                     | Flip-flop                                    |
| `when` / `elsewhen` / `otherwise` | Generates mux                                |
| `\\=`                             | Immediate combinational update               |
| Pipeline DSL                      | Nodes/Links + auto staging                   |

---
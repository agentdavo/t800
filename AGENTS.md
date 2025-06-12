# Contributor Guide (AGENTS.md)

This file gives every contributor—human or automated—the shared context needed to work on this repository. Keep it up-to-date; tooling will read it automatically before changing code.

---

## 1 Repository overview

| Path | What lives here |
|------|-----------------|
| `/src/ims-t800-core.scala` | Monolithic SpinalHDL core skeleton |
| `/test/` | ScalaTest + SpinalSim benches |
| `/doc/` | Architecture notes, opcode sheets, ADRs |
| `/synthesis/` | FPGA scripts & timing reports |
| `.github/workflows/` | CI (lint, test, synth) |
| `README.md` | Quick-start & Ubuntu setup |
| `AGENTS.md` | *this guide* |

If sub-folders add their own `AGENTS.md`, the most-nested file relevant to the edit wins.

---

## 2 Development environment

### 2.1 Ubuntu setup  
Follow the **“Ubuntu setup”** section in `README.md`.

### 2.2 Handy commands

```bash
# Build & run all tests
sbt test

# Generate Verilog
sbt "runMain T800CoreVerilog"

# Tiny sim with waves
sbt test:runMain T800CoreSim
````

---

## 3 Code-style & contribution rules

1. **SpinalHDL ≥ 1.9**, Scala 2.13. Only `spinal.core._` may be wildcard-imported.
2. One hardware statement per line; block comments explain intent.
3. When you replace a `// TODO` in `ExecuteUnit`, **delete** the marker.
4. CamelCase for wires/regs; **UPPER\_SNAKE** for constants. Public bundles end in `_IO`.
5. Bundles that use `valid/ready` extend `IMasterSlave`.
6. Add or update unit tests in `/test` for each new opcode/feature.
7. Run `sbt scalafmtCheckAll`; CI blocks mis-formatted code.
8. Remember: **SpinalHDL ≠ ordinary Scala** (see cheat-sheet below).

---

## 4 Validation checklist

| Command                         | What it checks      |
| ------------------------------- | ------------------- |
| `sbt scalafmtCheckAll`          | Source style        |
| `sbt test`                      | Unit + Sim benches  |
| `sbt "runMain T800CoreVerilog"` | Verilog elaboration |
| `nextpnr-ecp5` (nightly)        | Meets 50 MHz timing |

Coverage of touched files should stay ≥ 80 %.

---

## 5 Module ownership & boundaries

| Area                | Typical files                           | Maintainers       |
| ------------------- | --------------------------------------- | ----------------- |
| Core architecture   | `/src/ims-t800-core.scala`, `/doc/arch` | Architecture team |
| Integer ALU & OPR   | execute section                         | ALU team          |
| Scheduler & Timer   | `/src/sched`, `/src/timer`              | Flow team         |
| Floating-point unit | `/src/fpu`                              | FPU team          |
| Transcendental math | `/src/fpu` (extra pipes)                | Math team         |
| Channel links       | `/src/channel`                          | Links team        |
| Tests & CI          | `/test`, `.github`                      | QA/CI team        |
| Timing / area       | `/synthesis`, core tweaks               | PPA team          |

Cross-team changes → open a **draft PR** and tag the relevant team.

---

## 6 Pull-request template

```markdown
### What & Why
* Implemented opcode 0x30 (OPR-REV)
* Added unit test `OprRevSpec`

### Validation
- [x] `sbt scalafmtCheckAll`
- [x] `sbt test` (all green)

Closes #42
```

Branch name: `feat/<topic>` – e.g. `feat/alu-opr-add`.
Delete any `// TODO` lines you replace.

---

## 7 SpinalHDL ≠ normal Scala (quick cheat-sheet)

| Scala concept             | Hardware reality (Spinal rules)                                   |
| ------------------------- | ----------------------------------------------------------------- |
| Statement order           | Unordered; each `:=` adds a rule. **Last wins.**                  |
| Loops / functions         | Run once at elaboration, unrolled into RTL.                       |
| `Reg(UInt())`             | Flip-flop updated on clock edge.                                  |
| `when/elsewhen/otherwise` | Generates a mux; only active branch drives.                       |
| `\\=`                     | Immediate wire update (combinational only).                       |
| Pipeline DSL              | Declare nodes/links; builder auto-stages and handles ready/valid. |

---
# AGENTS.md

*Project play‑book for Codex‑powered agents collaborating on the **Transputer T800 core** implemented in SpinalHDL.*

---

## 0  Purpose

Centralise the **division of labour, workflow rules, coding standards and CI gates** that every AI or human contributor must follow while turning `ims‑t800‑core.scala` from a skeleton into a cycle‑accurate core.

---

## 1  Milestone roadmap

| Phase                    | Exit criteria                                                        | Lead agent   |
| ------------------------ | -------------------------------------------------------------------- | ------------ |
| **M‑0 Bootstrap**        | Skeleton passes CI (`sbt clean test`); Verilog generated             | **Arch‑Bot** |
| **M‑1 Integer‑Lite**     | Primary integer opcode range 0×00‑0×6F implemented; self‑tests green | **ALU‑Bot**  |
| **M‑2 Memory + Process** | `LDNL/STNL/LDNLP`, scheduler (`STARTP/ENDP/TIMESLICE`), timer wait   | **Flow‑Bot** |
| **M‑3 Channels**         | Four‑link engine sustains ≥1 Mword/s in loop‑back sim                | **Link‑Bot** |
| **M‑4 Full FPU**         | `FPADD/SUB/MUL/DIV`, conversions, flags; latency ≤ spec              | **FPU‑Bot**  |
| **M‑5 Transcendentals**  | `FPSIN/√/LN/EXP` reach ≤1 ulp; iterative pipes verified              | **Math‑Bot** |
| **M‑6 Compliance**       | IMS T800 validation suite & Occam tool‑chain boot on FPGA            | **QA‑Bot**   |
| **M‑7 Optimise**         | Meets 50 MHz @ ECP5; area < 40 k LUT                                 | **PPA‑Bot**  |

*Any phase may spin sub‑tasks, but may not declare "done" until **QA‑Bot** signs off.*

---

## 2  Agent roles & boundaries

| Agent        | Touches                      | Forbidden                              | Reviewer |
| ------------ | ---------------------------- | -------------------------------------- | -------- |
| **Arch‑Bot** | `/src`, `/doc/arch`          | `/src/fpu`, `/src/sched`               | QA‑Bot   |
| **ALU‑Bot**  | `/src/execute`               | `/src/fpu`                             | Arch‑Bot |
| **Flow‑Bot** | `/src/sched`, `/src/timer`   | `/src/fpu`                             | Arch‑Bot |
| **Link‑Bot** | `/src/channel`               | `/src/fpu`, `/src/sched`               | QA‑Bot   |
| **FPU‑Bot**  | `/src/fpu`                   | scheduler code                         | QA‑Bot   |
| **Math‑Bot** | `/src/fpu` (transcendentals) | process control                        | FPU‑Bot  |
| **QA‑Bot**   | `/test`, `.github`           | behaviour code (read‑only unless mock) | All      |
| **PPA‑Bot**  | `/src`, `/synthesis`         | change visible behaviour               | Arch‑Bot |

> Cross‑domain edits **must** be submitted as *draft* PRs and tag the owning agent for review.

---

## 3  Coding conventions

1. **SpinalHDL ≥ 1.9**, Scala 2.13 source level.
2. One statement per line; no wildcard imports except `spinal.core._`.
3. Replacing a `// TODO` in `ExecuteUnit` **requires deleting the marker** in the same commit.
4. CamelCase for regs/wires; **UPPER\_SNAKE** for constants.
5. Public bundles extend `IMasterSlave` if they use `valid/ready` flow control.
6. Every non‑obvious state transition carries an inline `// why:` comment.
7. PR blocked if **coverage < 80 %** for files you changed.

### 3.1  SpinalHDL ≠ “Normal” Scala  ‑ quick mental model

| Concept             | Plain Scala mental model       | **SpinalHDL semantics**                                                                                                                                                   |
| ------------------- | ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Statement order     | Sequential execution           | *Irrelevant*: every `:=` adds a concurrent hardware rule; order is resolved by *last‑assignment‑wins* during elaboration.                                                 |
| Variables           | Hold a single value at runtime | A `UInt/Bits/Bool` *wire* represents a network whose value is the result of **all** assignments that survive the `when/switch` tree.                                      |
| Objects & functions | Execute at run‑time            | Execute **only once, at elaboration**. Loops & higher‑order functions are unrolled into RTL.                                                                              |
| Mutation            | `x = x + 1` updates in place   | Registers mutate only via `Reg()` and clock edges; combinational `\=` gives immediate (single‑line) update but forbidden on regs.                                         |
| Concurrency         | via threads/Futures            | *Always concurrent*. Each RTL clock‑edge updates every `Reg`.                                                                                                             |
| Pipeline‑API        | n/a                            | The `spinal.lib.misc.pipeline` DSL builds nodes/links at elaboration→ generates auto‑staged streams with valid/ready and optional flush/throw, relieving manual retiming. |

> **Rule of thumb:** write hardware *declaratively*: “what must be true every cycle”, **not** “do X then Y”. When in doubt spell out the waveforms in a unit‑test.

---

## 4  Task / issue template

```markdown
### Opcode / Feature
`0x8A LDPI`

### Acceptance tests
- [ ] Unit‑test loads π ≈ 3.141592654f into A‑reg
- [ ] No extra bubbles in pipeline; still single‑cycle

### Files to change
- `ims‑t800‑core.scala` (ExecuteUnit case 0x8A)
- `/test/opcodes/FPUFloatSpec.scala`
```

---

## 5  CI pipeline (GitHub Actions)

1. **Setup** – cache SBT & Spinal tool‑chain.
2. **Lint** – `scalafmt --test`; custom rules via **Lint‑Bot**.
3. **Unit tests** – `sbt +test`; attach failed wave dumps.
4. **RTL gen** – `sbt "runMain T800CoreVerilog"`
5. **Nightly FPGA synth** – Lattice ECP5 via `nextpnr`; timing & area diff posted as comment.

---

## 6  Documentation tree

```
/doc/arch          high‑level architecture notes
/doc/opcodes       1‑pager per opcode (state diagram, corner‑cases)
/doc/decisions     ADR decision records (Arch‑Bot owner)
```

---

## 7  Branches and merge policy

* `main` always green.
* Feature branches → `feat/<agent>/<topic>`.
* Squash‑merge, commit title in **imperative mood**.
* After merge, **QA‑Bot** auto‑closes linked issue and posts coverage delta.

---
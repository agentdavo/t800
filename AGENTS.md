# Contributor Guide

*(rev 2025-06-12 – includes plugin & pipeline details)*

---

## 1 Project map

| Path | Description |
|------|-------------|
| `src/main/scala/t800/plugins/` | Each file is a `FiberPlugin` subsystem |
| `src/main/scala/t800/Top.scala` | Builds `PluginHost`, selects plugin list with `--variant=` |
| `src/test/scala/t800/` | ScalaTest units + SpinalSim benches |
| `ext/SpinalHDL/` | Upstream library as git sub-module |
| `doc/spinalHDL.html` | SpinalSim + SpinalHDL documentation |

---

## 2 Development quick-ref

```bash
# default (full) variant
sbt test
sbt "runMain t800.TopVerilog"

# integer-only variant
sbt "runMain t800.TopVerilog --variant=min"
````

---

## 3 Style & rules

* **One plugin per file** under `src/main/scala/t800/plugins/`.
  Filename: `<Subsystem>Plugin.scala`.

* Plugins expose IO via tiny *service* wrappers:

  ```scala
  case class LinkPins(bundle: Bundle)
  addService(LinkPins(io.links))
  ```

* Dataflow inside a plugin **must** use Pipeline DSL (`Node`, `StageLink`, `CtrlLink`), not ad-hoc `RegNext`.

* Each new opcode requires a unit test that drives bytes through fetch and checks A/B/C or memory.

---

## 4 CI validation

| Stage                   | Command                                                            |
| ----------------------- | ------------------------------------------------------------------ |
| Style                   | `sbt scalafmtCheckAll`                                             |
| Tests (full variant)    | `sbt test`                                                         |
| Verilog (both variants) | `sbt "runMain t800.TopVerilog --variant=min"` and `--variant=full` |
| Nightly timing          | see `.github/workflows/ci.yml`                                     |

---

## 5 Milestones

| ID      | Goal                             | Plugin(s) touched | DSL highlight                 |
| ------- | -------------------------------- | ----------------- | ----------------------------- |
| **M-1** | ALU-Lite (`REV ADD SUB AND XOR`) | `ExecutePlugin`   | `StageCtrlPipeline`, `haltIt` |
| **M-2** | PFIX/NFIX + `LDL`                | `ExecutePlugin`   | same                          |
| **M-3** | RAM `STL/LDL` + stack            | `MemoryPlugin`    | `S2MLink`                     |
| **M-4** | Two-queue scheduler              | `SchedulerPlugin` | `ForkLink`                    |
| **M-5** | 64-bit timer + wait              | `SchedulerPlugin` | `JoinLink`                    |
| **M-6** | FP adder pipeline                | `FpuPlugin`       | plugin swap                   |

Open **one** milestone per PR branch (`feat/m1-alu`, etc.).

---

## 6 PR template

```markdown
### What & Why
* Implement opcode 0x94 (OPR-ADD)
* Added `AluAddSpec`

### Validation
- [x] scalafmt
- [x] `sbt test` (full + min variants)

Closes #42
```

---

## 7 Writing a plugin

```scala
package t800.plugins                // mandatory package!

import spinal.lib.misc.plugin._
import spinal.lib.misc.pipeline._

/** Example: free-running timer service. */
trait TimerSrv { def now: UInt }

class TimerPlugin extends FiberPlugin {
  val cnt = Reg(UInt(64 bits)) init(0)
  during.setup { cnt := cnt + 1 }

  addService(new TimerSrv { def now = cnt })
}
```

Use the service elsewhere:

```scala
val timer = Plugin[TimerSrv]
when(ctrl.isValid) { Areg := timer.now(31 downto 0) }
```

For strict ordering:

```scala
val lock = Lock()
buildBefore(lock)   // ensure this plugin finishes first
```

---

## 8 Pipeline DSL cheat-sheet

| Need             | One-liner                         |
| ---------------- | --------------------------------- |
| Register slice   | `StageLink(n0, n1)`               |
| Skid buffer      | `S2MLink(up, down)`               |
| Broadcast 1→N    | `ForkLink(src, Seq(a,b))`         |
| Join N→1         | `JoinLink(Seq(a,b), dst)`         |
| Stall stage      | `ctrl.haltWhen(cond)`             |
| Flush stage      | `ctrl.throwIt(usingReady = true)` |
| Duplicate        | `ctrl.duplicateIt()`              |
| Multi-issue lane | extend `CtrlLaneApi`              |

Golden rules:

1. Insert each `Payload` once with `insert()`.
2. Other stages only *read* it (`node(payload)`); no secondary writes.
3. Use DSL requests (`haltIt`, `terminateIt`, …) rather than setting `ready/valid` directly.

---

## 9 Troubleshooting

| Symptom                       | Explanation & fix                                                               |
| ----------------------------- | ------------------------------------------------------------------------------- |
| `ctrl.forgetOne unsupported`  | Using `DirectLink`; switch to `StageLink` or enable `.ctrl.forgetOneSupported`. |
| Combinational loop on Payload | Same signal driven in two stages – keep single `insert()`.                      |
| Fiber deadlock                | Circular dependency → break with `buildBefore(lock)`.                           |

---

## 10 Ownership matrix

| Plugin          | Maintainers |
| --------------- | ----------- |
| FetchPlugin     | Front-end   |
| ExecutePlugin   | ALU team    |
| FpuPlugin       | FPU team    |
| SchedulerPlugin | Flow team   |
| MemoryPlugin    | Mem/cache   |
| ChannelPlugin   | Links       |
| TrapPlugin      | Safety      |
| DebugPlugin     | QA          |

---

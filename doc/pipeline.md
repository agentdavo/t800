# SpinalHDL Pipeline API Reference  

*(module `spinal.lib.misc.pipeline`)*

> **Goal** — Provide a strict, self-contained reference for emitting valid, synthesizable pipelines.

---

## 1  Node

A **`Node`** represents one pipeline stage. Each node hosts:

| Signal | Dir | Meaning |
|--------|-----|---------|
| `valid`  | RW | Upstream asserts when a transaction is present. May only drop the cycle after `(valid && ready)` or `cancel`. **Never** combinationally depend on `ready`. |
| `ready`  | RW | Downstream asserts when it can accept the transaction, providing back-pressure. No meaning when `valid = 0`. |
| `cancel` | RW | Downstream asserts to flush the transaction. No meaning when `valid = 0`. |

Read-only aliases:

* `isValid`, `isReady`, `isCancel`
* `isFiring`   = `valid && ready && !cancel`   (transaction successfully moves forward)
* `isMoving`   = `valid && (ready || cancel)` (transaction leaves this stage next cycle)
* `isCanceling` = `valid && cancel`

### 1.1  Handshake Helpers

```scala
node.arbitrateFrom(stream)          // Stream → Node handshake
node.arbitrateTo(stream)            // Node → Stream handshake
node.driveFrom(stream){ (n,d) => …} // handshake + payload copy
node.driveTo(stream){ (d,n) => … }  // handshake + payload copy
````

---

## 2  Payload

A **`Payload[T]`** is a *key* (and a `HardType`) that identifies data flowing through the pipeline.

```scala
val VALUE = Payload(UInt(16 bits))
val PC    = Payload(UInt(32 bits))
```

* **Access:**  `node(PAYLOAD)`   or   `node(PAYLOAD, secondaryKey)`
* **Insert:**  `node.insert(signal)` → returns a new `Payload` bound to `signal`
* **Implicit use inside a node/area:** simply reference the `Payload`; the compiler rewrites to `node(PAYLOAD)`.

Convention: name payloads in **UPPER-CASE** to emphasize they are keys, not signals.

---

## 3  Link Types

| Class        | Registers                           | Purpose                                                         |
| ------------ | ----------------------------------- | --------------------------------------------------------------- |
| `DirectLink` | none                                | Wire-through connection.                                        |
| `StageLink`  | `data`, `valid`                     | Classic pipeline stage with `ready` back-pressure.              |
| `S2mLink`    | `ready`                             | Registers `ready` only (improves timing on back-pressure path). |
| `CtrlLink`   | like `StageLink` + flow-control API | Supports stalls, flushes, duplication, bypassing.               |

### 3.1  `CtrlLink` Flow-control API

```scala
haltWhen(cond)        // block (clear up.ready & down.valid)
throwWhen(cond)       // cancel (clear down.valid)
forgetOneWhen(cond)   // ask upstream to drop its transaction
ignoreReadyWhen(cond) // force up.ready = true
duplicateWhen(cond)   // duplicate (clear up.ready)
terminateWhen(cond)   // hide from downstream (clear down.valid)

// Shorthand when inside a conditional scope
link.haltIt() | duplicateIt() | terminateIt() | forgetOneNow() | ignoreReadyNow() | throwIt()
```

`CtrlLink.bypass(PAYLOAD)` lets you patch a payload value mid-flight (e.g. hazard fix).

---

## 4  Payload Resolution & Keying

```scala
val PC  = Payload(UInt(32 bits))
n0(PC)            := 0x42          // write in stage 0
val PC4 = n1(PC)  + 4              // read in stage 1
n1(PC)            := PC4           // overwrite

// secondaryKey variant (multi-lane example)
n0(PC, laneId) := startPC(laneId)
n1(PC, laneId) := n1(PC, laneId) + 4
```

*Secondary keys* isolate multiple independent copies of the same payload.

---

## 5  Area Scoping & Staging

```scala
val n1 = Node()
val VAL = Payload(UInt(8 bits))

// Scope logic to n1
val logic = new n1.Area {
  val OUT = insert(VAL) + 1   // uses implicit n1(VAL)
}

// StagePipeline example
val pip = new StagePipeline
val A   = pip(0).insert(a)
new pip.Area(1) { val B = insert(A) + 5 }
pip.build()
```

`Node.Area`, `StagePipeline.Area`, and `StageCtrlPipeline.Ctrl` let you group stage-local logic cleanly.

---

## 6  Stream / Flow Interfacing

```scala
n0.arbitrateFrom(upStream)
n0(VALUE) := upStream.payload

n2.arbitrateTo(downStream)
downStream.payload := n2(RESULT)
```

`Payload` can be used as a `HardType` for stream payloads:

```scala
val up = slave Stream(VALUE)   // VALUE.type becomes the stream’s payload type
```

---

## 7  Pipeline Builders

### 7.1  `Builder`

```scala
Builder(link1, link2, …)   // generate registers/arbiters for all links
```

### 7.2  `NodesBuilder`

```scala
val builder = new NodesBuilder
val n0 = new builder.Node { arbitrateFrom(io.in) ; VALUE := io.in.payload }
val n1 = new builder.Node { RESULT := VALUE + 1 }
val n2 = new builder.Node { arbitrateTo(io.out) ; io.out.payload := RESULT }
builder.genStagedPipeline()
```

### 7.3  `StagePipeline`

* Fixed-length data pipeline (`Node → StageLink → Node → …`)
* `pip(stageIdx)` returns the stage node.
* `new pip.Area(stageIdx)` scopes logic.
* Call `pip.build()` when finished.

### 7.4  `StageCtrlPipeline`

Same as `StagePipeline` but each stage is a `CtrlLink`.
Use `pip.ctrl(stageIdx)` to access control-aware nodes, plus `throwWhen`, `haltWhen`, etc.

---

## 8  Arbitration Semantics

| valid | ready | cancel | Meaning               | isFiring | isMoving |
| ----- | ----- | ------ | --------------------- | -------- | -------- |
| 0     | X     | X      | No transaction        | 0        | 0        |
| 1     | 1     | 0      | Transfer (normal)     | 1        | 1        |
| 1     | 0     | 0      | Blocked by downstream | 0        | 0        |
| 1     | X     | 1      | Transaction canceled  | 0        | 1        |

Design rules:

1. `valid` must be registered; never combinationally depend on `ready`.
2. After `isFiring` or `isCanceling`, deassert `valid` on the next cycle.
3. If no back-pressure is desired, simply never reference `ready` (it defaults to *1*).

---

## 9  Design Guidelines for AI Generators

* Emit `Payload` objects once, reuse everywhere.
* Place stage logic inside `Node.Area` / `pip.Area` to guarantee correct placement.
* Finish every manual link list with `Builder(...)`.
* Use `StagePipeline` / `StageCtrlPipeline` for retime-friendly designs.
* Choose `S2mLink` to cut the `ready` timing path when needed.
* Handle hazards with `CtrlLink.bypass` or `throwWhen` rather than ad-hoc rewiring.
* Respect handshake protocol: never derive `valid` from `ready`.

---

## 10  Minimal Example

```scala
import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

class TopLevel extends Component {
  val io = new Bundle {
    val up   = slave  Stream(UInt(16 bits))
    val down = master Stream(UInt(16 bits))
  }

  // Nodes
  val n0, n1, n2 = Node()

  // Links (pipelined registers)
  val l01 = StageLink(n0, n1)
  val l12 = StageLink(n1, n2)

  // Payloads
  val VALUE  = Payload(UInt(16 bits))
  val RESULT = Payload(UInt(16 bits))

  // Stage-0 : input
  n0.driveFrom(io.up) { (self,p) => self(VALUE) := p }

  // Stage-1 : processing
  n1(RESULT) := n1(VALUE) + 0x1200

  // Stage-2 : output
  n2.driveTo(io.down){ (p,self) => p := self(RESULT) }

  // Generate hardware
  Builder(l01, l12)
}
```

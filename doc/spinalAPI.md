# SpinalHDL Libraries Documentation

This document provides a comprehensive overview of the SpinalHDL libraries vendored under `ext/SpinalHDL`. It focuses on three key API groups relevant to hardware design: `spinal.core.fiber`, `spinal.lib.misc.plugin`, and `spinal.lib.misc.pipeline`. Each section summarizes the functionality of these packages, detailing their core components, classes, and methods.

---

## Table of Contents

1. [Introduction](#introduction)
2. [spinal.core.fiber Package](#spinalcorefiber-package)
   - [package.scala](#packagescala)
   - [AsyncCtrl.scala](#asyncctrlscala)
   - [AsyncThread.scala](#asyncthreadscala)
   - [Fiber.scala](#fiberscala)
   - [Handle.scala](#handlescala)
   - [Misc.scala](#miscscala)
   - [Other Files](#other-files)
3. [spinal.lib.misc.plugin Package](#spinallibmiscplugin-package)
   - [Host.scala](#hostscala)
   - [Fiber.scala](#fiberscala-1)
4. [spinal.lib.misc.pipeline Package](#spinallibmiscpipeline-package)
   - [package.scala](#packagescala-1)
   - [Node.scala](#nodescala)
   - [Link.scala](#linkscala)
   - [StageLink.scala](#stagelinkscala)
   - [DirectLink.scala](#directlinkscala)
   - [CtrlLink.scala](#ctrllinkscala)
   - [ForkLink.scala](#forklinkscala)
   - [JoinLink.scala](#joinlinkscala)
   - [S2MLink.scala](#s2mlinkscala)
   - [Builder.scala](#builderscala)
   - [Misc.scala](#miscscala-1)
5. [Summary](#summary)

---

## Introduction

The SpinalHDL libraries under `ext/SpinalHDL` provide a robust framework for designing modular, reusable hardware descriptions. The three primary API groups are:

- **`spinal.core.fiber`**: Manages asynchronous elaboration tasks for out-of-order execution.
- **`spinal.lib.misc.plugin`**: Offers a plugin-based composition framework for service integration.
- **`spinal.lib.misc.pipeline`**: Provides a pipeline construction DSL for building staged hardware designs.

This document details the functionality of each package, focusing on their core classes, methods, and use cases.

---

## spinal.core.fiber Package

Located in `ext/SpinalHDL/core/src/main/scala/spinal/core/fiber`, this package provides primitives for asynchronous elaboration, enabling complex hardware designs with managed task dependencies.

### package.scala

Defines helper methods for task forking and handle management.

| Method | Description |
|--------|-------------|
| `hardFork[T](body)` | Spawns an asynchronous thread, loading its result into a new `Handle`. |
| `hardForkRaw[T](withDep)` | Spawns a thread with explicit dependency management. |
| `hardForkRawHandle[T](withDep)(body: Handle[T] ⇒ T)` | Spawns a thread with a custom body that returns a value to a `Handle`. |
| `soon(handle, others*)` | Informs the scheduler that the current thread will load the specified handle(s). |
| `soon(handles: Seq[Handle[_]])` | Batch version of `soon` for multiple handles. |

### AsyncCtrl.scala

Implements the execution engine for asynchronous tasks.

| Method | Description |
|--------|-------------|
| `EngineContext.newJvmThread` | Manages JVM worker threads for task execution. |
| `schedule(body)` | Creates an `AsyncThread` for the given body. |
| `start()` | Runs scheduled tasks, handling waiting threads and deadlock detection. |
| `sleep(thread)` | Parks an `AsyncThread`. |
| `wakeup(thread)` | Unparks an `AsyncThread`. |
| `Engine.create(body, name)` | Instantiates a new engine and executes a body within it. |

### AsyncThread.scala

Represents a scheduled task in the fiber framework.

| Method | Description |
|--------|-------------|
| `AsyncThread.current` | Static accessor to the currently running thread. |
| `addSoonHandle(handle)` | Records that the thread will load the given handle. |
| `managerResume()` | Internal context-switch primitive for resuming execution. |
| `suspend()` | Suspends the thread. |
| `getLocationShort()` | Returns stack-trace information upon thread termination. |

### Fiber.scala

Core API for scheduling elaboration tasks.

| Method | Description |
|--------|-------------|
| `Fiber.apply(orderId)(body)` | Schedules a body at a specific elaboration order. |
| `setup`, `build`, `patch`, `check` | Convenience wrappers for predefined phases. |
| `callback(orderId)(body)` | Registers a callback for a phase start. |
| `setupCallback`, `buildCallback` | Phase-specific callback registration. |
| `await(orderId)` | Waits for a phase to complete. |
| `awaitSetup`, `awaitBuild`, `awaitPatch`, `awaitCheck` | Phase-specific wait methods. |
| `Fiber.addTask[T](orderId)(body)` | Spawns a task tied to the fiber instance. |
| `addCallback(orderId)(body)` | Stores callbacks within the fiber. |
| `runSync()` | Executes tasks in phase order, respecting locks and retainers. |

### Handle.scala

Synchronization primitive for carrying values of type `T`.

| Method | Description |
|--------|-------------|
| `Handle.apply()` | Creates an uninitialized handle. |
| `Handle.apply(value)` | Creates a handle with an initial value. |
| `Handle.sync(value)` | Creates a synchronously loaded handle. |
| `await()` | Blocks until the handle’s value is available. |
| `get` | Retrieves the handle’s value (if loaded). |
| `waitLoad` | Blocks until the handle is loaded. |
| `isLoaded` | Checks if the handle has a value. |
| `load(value: T)` | Loads a value into the handle. |
| `load(value: Handle[T])` | Loads a value from another handle. |
| `loadAsync(body)` | Loads a value asynchronously via a body. |
| `loadNothing()` | Marks the handle as empty. |
| `unload()` | Clears the handle’s value. |
| `soon(handle*)` | Declares handles to be loaded by the current thread. |
| `map`, `produce`, `derivate`, `derivatedFrom` | Creates dependent handles based on this handle’s value. |

### Misc.scala

Utilities for synchronization and task coordination.

| Method/Class | Description |
|--------------|-------------|
| `Lock` | Simple mutex with `retain()` and `release()`. |
| `Lockable` | Trait exposing a `Lock` with `retain`, `release`, and `await`. |
| `RetainerHold.release()` / `done()` | Used by `Retainer` to wait on condition groups. |
| `Retainer()` | Returns a `RetainerHold`; `await()` waits for all holds. |
| `RetainerGroup` | Collects `Retainer`/`Lock` instances; supports `+=`, `++=`, and `release()`. |

### Other Files

- **Play.scala**: Contains experimental and test code for the fiber API.

---

## spinal.lib.misc.plugin Package

Located in `ext/SpinalHDL/lib/src/main/scala/spinal/lib/misc/plugin`, this package provides a plugin-based framework for composing hardware designs.

### Host.scala

Defines the plugin host and service management.

| Class/Object | Method | Description |
|--------------|--------|-------------|
| `Plugin` | `list[T]` | Returns all services of type `T` from the current `PluginHost`. |
| `Plugin` | `apply[T]` | Fetches exactly one service of type `T`. |
| `PluginHost` | N/A | A `ScopeProperty` enabling `PluginHost.on { … }` to run code under a host. |
| `Hostable` | `setHost` | Trait requiring implementation to bind to a host. |
| `PluginHost` | `rework(body)` | Temporarily switches elaboration context to this host. |
| `PluginHost` | `addService(obj)` | Registers a service. |
| `PluginHost` | `asHostOf(hostables)` | Binds multiple plugins/services to this host. |
| `PluginHost` | `list[T]`, `apply[T]`, `get[T]`, `find[T]`, `findOption[T]` | Looks up services by type. |

### Fiber.scala

Defines a plugin abstraction built on the fiber API.

| Method | Description |
|--------|-------------|
| `withPrefix(prefix)` | Renames the plugin instance. |
| `retains(...)` | Constructs a `RetainerGroup` for required locks/retainers. |
| `addService(obj)` | Exposes a sub-service. |
| `awaitBuild()` | Waits for the build phase of all plugins. |
| `buildBefore(lock)` | Coordinates plugin build order using locks. |
| `setupRetain(lock)` | Ensures setup phase retains specified locks. |
| `setHost(host)` | Registers the plugin with a `PluginHost`. |
| `during.setup {…}` | Schedules code for the setup phase (returns a `Handle`). |
| `during.build {…}` | Schedules code for the build phase (returns a `Handle`). |

### Custom services

Plugins exchange data through small service traits:

| Service | Description |
|---------|-------------|
| `LinkPins` | Marker trait for link-related I/O bundles. |
| `DebugPins` | Area containing optional debug signals. |
| `ExtMemPins` | Off-chip memory interface pins. |
| `StackSrv` | Exposes A/B/C/O registers and workspace pointer with read/write helpers. |
| `FpuSrv` | Command and response interface for the floating-point unit. |
| `SchedSrv` | Scheduler queues and control for process dispatch. |
| `TimerSrv` | High/low timers with load and enable controls. |
| `InstrFetchSrv` | Instruction fetch memory request and response flows. |
| `DataBusSrv` | Data memory bus read and write flows. |
| `LinkBusSrv` | Memory bus for transputer links. |
| `LinkBusArbiterSrv` | Arbitration channels between execution and channel DMA. |
| `MemAccessSrv` | Provides access to ROM and RAM memories. |
| `ChannelSrv` | FIFO-level transmit and receive operations on links. |
| `ChannelPinsSrv` | External channel link pins. |
| `ChannelDmaSrv` | DMA command stream for channel transfers. |
| `GroupedInstrSrv` | Flow of instruction groups (up to eight opcodes). |
| `PipelineSrv` | Global pipeline stage handles and payloads. |

---

## spinal.lib.misc.pipeline Package

Located in `ext/SpinalHDL/lib/src/main/scala/spinal/lib/misc/pipeline`, this package provides a DSL for constructing pipelined hardware designs.

### package.scala

Defines the `Payload[T <: Data]` type alias for `NamedType[T]`.

### Node.scala

Core class for pipeline nodes.

| Class/Object | Method | Description |
|--------------|--------|-------------|
| `Node` | `apply()` | Creates a pipeline node. |
| `OffsetApi` | N/A | Helper to fetch lanes via `node(subSeq)(payload)`. |
| `NodeBaseApi` / `NodeApi` | `valid`, `ready`, `cancel` | Accessors for control signals. |
| `NodeBaseApi` / `NodeApi` | `isValid`, `isReady`, `isCancel` | Read-only control signal accessors. |
| `NodeBaseApi` / `NodeApi` | `isFiring`, `isMoving`, `isCanceling` | Derived control flags. |
| `NodeBaseApi` / `NodeApi` | `apply(payload)`, `apply(payload, subKey)`, `apply(NamedTypeKey)` | Data access methods. |
| `NodeBaseApi` / `NodeApi` | `insert(data)` | Introduces new payload signals. |
| `NodeBaseApi` / `NodeApi` | `arbitrateFrom`, `driveFrom`, `arbitrateTo`, `driveTo`, `toStream`, `toFlow` | Streaming helpers. |
| `Node` | N/A | Maintains payload/data maps and builds hardware. |
| `Node.Area` | N/A | Provides an `Area` with node API. |
| `NodeMirror` | N/A | Exposes the API of an existing node. |

### Link.scala

Base class for connecting pipeline nodes.

| Method | Description |
|--------|-------------|
| `Link.connectDatas(up, down)` | Wires matching payloads between nodes. |
| `Link.connectDatasWithSwap(up, down, map)` | Wires payloads with remapping. |
| `Link` | `ups`, `downs` | Abstract members for upstream/downstream nodes. |
| `Link` | `nodeSetup()`, `propagateDown()`, `propagateUp()`, `build()` | Lifecycle methods. |
| `Link` | `propagateDownAll()`, `propagateUpAll()` | Forwards payload knowledge. |

### StageLink.scala

Connects two nodes with pipeline registers.

| Method | Description |
|--------|-------------|
| `withoutCollapse()` | Disables bubble-swallowing optimization. |
| `withPayloadHold()` | Registers data only when `valid` and `ready` are high. |
| `propagateDown`, `propagateUp`, `build` | Implements the register slice. |

### DirectLink.scala

Provides a combinational connection between nodes.

| Method | Description |
|--------|-------------|
| `propagateDown`, `propagateUp`, `build` | Wires signals directly, exposing `forgetOne`/`cancel` paths. |

### CtrlLink.scala

Adds advanced control features (halt, flush, duplicate).

| Class/Object | Method | Description |
|--------------|--------|-------------|
| `CtrlLink` | `apply(up, down)` | Creates a link between specified nodes. |
| `CtrlLink` | `apply()` | Creates a link with new nodes. |
| `CtrlApi` | `up`, `down` | Accessors for linked nodes. |
| `CtrlApi` | `apply`, `insert`, `bypass` | Convenience data access methods. |
| `CtrlApi` | `haltWhen`, `duplicateWhen`, `terminateWhen`, `forgetOneWhen`, `ignoreReadyWhen`, `throwWhen` | Control requests. |
| `CtrlApi` | `haltIt`, `duplicateIt`, `terminateIt`, `throwIt`, `forgetOneNow`, `ignoreReadyNow` | Short-form helpers for control blocks. |
| `CtrlApi` | `forkStream` | Spawns a `Stream[NoData]` based on firing conditions. |
| `CtrlLink` | N/A | Implements control logic and builds hardware. |
| `CtrlLinkMirror` | N/A | Exposes the API of another `CtrlLink`. |

### ForkLink.scala

Broadcasts one node to multiple downstream nodes.

- Supports synchronous or asynchronous broadcast modes during build.

### JoinLink.scala

Merges multiple upstream nodes into one downstream node.

### S2MLink.scala

Implements a skid-buffer style stage, allowing upstream to continue when downstream stalls.

### Builder.scala

Utilities for constructing pipelines.

| Class/Object | Method | Description |
|--------------|--------|-------------|
| `Builder` | `Builder(head, tail*)` | Propagates metadata and emits hardware for given links. |
| `Builder` | `Builder(connectors)` | Alternative constructor for connectors. |
| `NodesBuilder` | N/A | Creates nodes and calls `genStagedPipeline`. |
| `StagePipeline` | `build()` | Creates numbered nodes linked with `StageLink`. |
| `StageCtrlPipeline` | N/A | Similar to `StagePipeline` but uses `CtrlLink`. |

### Misc.scala

Additional pipeline utilities.

| Class/Object | Method | Description |
|--------------|--------|-------------|
| `FromUp` / `FromDown` | N/A | Tracks payload needs for each node. |
| `Misc` | `nameThat(self, target, key, postfix)` | Sets consistent signal names. |
| `NamedTypeKey` | N/A | Identifies a `Payload` with an optional sub-key. |

---

## Summary

The SpinalHDL libraries enable modular hardware design through:

- **Fiber API**: Manages out-of-order elaboration with `Handle`, `Lock`, and `Retainer` primitives.
- **Plugin Framework**: Uses `PluginHost` to collect services and `FiberPlugin` to coordinate setup/build phases.
- **Pipeline DSL**: Combines `Node` and `Link` classes to insert registers, control logic, and arbitrate flows automatically.

These APIs facilitate the construction of complex, reusable hardware designs. For implementation details, refer to the source files under `ext/SpinalHDL`.

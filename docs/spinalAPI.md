# SpinalHDL API Overview

This document serves as a quick reference for three SpinalHDL subpackages that are heavily used in the project.

## `spinal.lib.misc.pipeline._`

Provides the pipeline DSL used to build stall and flush aware pipelines. Key classes:

- **`Node`** – represents a stage in a pipeline. Payloads are inserted and read on nodes.
- **`Link` / `StageLink`** – connect nodes together and insert the required registers.
- **`Builder`** – finalises a set of links into actual hardware.
- **`Payload`** – a key that identifies data passed between nodes. The builder wires all occurrences of a given payload across the pipeline.

Advantages of the DSL (from `doc/spinalHDL.txt`):

```
spinal.lib.misc.pipeline provides a pipelining API. The main advantages over manual pipelining are :
    * You don't have to predefine all the signal elements needed for the entire staged system upfront. You can create and consume stageable signals in a more ad hoc fashion as your design requires - without needing to refactor all the intervening stages to know about the signal
    * Signals of the pipeline can utilize the powerful parametrization capabilities of SpinalHDL and be subject to optimization/removal if a specific design build does not require a particular parametrized feature, without any need to modify the staging system design or project code base in a significant way.
    * Manual retiming is much easier, as you don't have to handle the registers / arbitration manually
    * Manage the arbitration by itself
The API is composed of 4 main things :
    * Node : which represents a layer in the pipeline
    * Link : which allows to connect nodes to each other
    * Builder : which will generate the hardware required for a whole pipeline
    * Payload : which are used to retrieve hardware signals on nodes along the pipeline
It is important to understand that Payload isn't a hardware data/signal instance, but a key to retrieve a data/signal on nodes along the pipeline, and that the pipeline builder will then automatically interconnect/pipeline every occurrence of a given Payload between nodes.
```

(see `ext/SpinalHDL/lib/.../pipeline` for the full source)

## `spinal.core.fiber._`

Implements the *Fiber* framework for out-of-order elaboration. Tasks produce and consume `Handle[T]` values and may wait on them before hardware is generated. Important pieces:

- **`Handle[T]`** – placeholder for a value that may become available later.
- **`soon(handle)`**, **`handle.load`**, **`handle.get`** – schedule and synchronise tasks around handles.
- **`Retainer`** / **`Lock`** – primitives used to control ordering between fibers.

Excerpt from `doc/spinalHDL.txt`:

```
Fiber is a framework to run the hardware elaboration in an out of order manner, a bit similarly to Makefile, where you can define rules and dependencies which will then be solved when you run a make command.
```

See the examples in `ext/SpinalHDL/lib/src/.../fiber` for more advanced usage.

## `spinal.lib.misc.plugin._`

Defines the plugin framework used throughout the T800 core. Each subsystem extends `FiberPlugin` and registers services on a `PluginHost`. Highlights:

- **`PluginHost`** – container that holds plugins and provides `list` / `apply` to retrieve services.
- **`FiberPlugin`** – base class for plugins; exposes `during setup` and `during build` to separate elaboration phases and manage dependencies with `awaitBuild()` or `buildBefore()`.
- **`Hostable`** – trait implemented by plugins to let them attach to a host.

Plugins interact via small service classes and can retain each other during setup. Details and more examples live under `ext/SpinalHDL/lib/src/.../plugin`.

---

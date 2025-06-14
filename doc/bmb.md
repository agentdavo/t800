# BMB Bus

This document summarises the BMB (Bus Matrix Bus) used by SpinalHDL. The official implementation lives in `lib/src/main/scala/spinal/lib/bus/bmb/` under the bundled SpinalHDL sources.

BMB is a highly configurable memory-mapped bus. Each endpoint exposes a parameter object describing address width, data width, burst length and optional features such as exclusive access or invalidation. The same description drives type-safe interfaces (`Bmb`, `BmbCmd`, `BmbRsp` and friends) and allows bus bridges to be generated automatically.

## Core features

* **Read/write commands** with optional bursts and byte masks.
* **Exclusive transfers** via `BmbExclusiveMonitor` for atomic sequences.
* **Invalidate and synchronisation** channels to support cache coherency.
* **Context fields** so masters can tag requests with side-band metadata.
* **Arbiter and decoder** primitives to build crossbars or simple buses.
* **Interconnect generator** (`BmbInterconnectGenerator`) which wires multiple masters and slaves with round‑robin or priority arbitration.

## Available bridges

The library includes bridges to several other protocols and helpers that transform a BMB interface:

* `BmbToWishbone` / `BmbToApb3Bridge` / `BmbToAxi4Bridge` and `BmbToTilelink` for protocol conversion.
* `TilelinkToBmb` for the opposite direction.
* `BmbUpSizerBridge` and `BmbDownSizerBridge` to change the data width.
* `BmbAligner` to ensure burst alignment.
* `BmbUnburstify` to split bursts into single beats.
* On‑chip RAM primitives such as `BmbOnChipRam`, `BmbIce40Spram` and `BmbEg4S20Bram32K`.

Additional utilities include write retainers, context or sync removers and simulation models located under `lib/src/main/scala/spinal/lib/bus/bmb/sim`.

Refer to the source code for complete definitions and examples.

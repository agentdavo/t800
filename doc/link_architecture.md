# Link Architecture

This note collects information about the current link interface and future VCP integration.

## ChannelPlugin

`ChannelPlugin` exposes link I/O through two small services:

- `ChannelPinsSrv` – yields a `ChannelPins` bundle for the top‑level RTL.
- `ChannelSrv` – provides `rx` and `tx` `Stream[Bits]` vectors used inside the CPU.

Each link is buffered by a pair of two‑word FIFOs. Other plugins see clean `Stream` endpoints and do not worry about pin timing.

## Memory handshake via `LinkBusSrv`

`MemoryPlugin` implements a third bus service named `LinkBusSrv`.  It mirrors the
regular data bus but is reserved for link DMA and VCP traffic.  Plugins issue
`MemReadCmd` and `MemWriteCmd` requests on this service and receive responses in
`rdRsp` and `wrCmd` streams.  The address space exposes the link input/output
registers defined in `Global` (e.g. `Link0Output`, `Link0Input`).

## T9000 VCP design

The T9000 used a small virtual channel processor (VCP) to move packets between
the links and memory.  In this project a future `VcpPlugin` will implement that
logic as yet another `FiberPlugin`.  It will subscribe to `ChannelSrv` for raw
words and use `LinkBusSrv` to fetch or store packet payloads.  Expected register
usage follows the classic INMOS scheme: the link output registers accept command
words while the input registers deliver completed messages.

### Integration points

A `VcpPlugin` would plug into:

- `ChannelSrv` for the physical link streams;
- `LinkBusSrv` for memory access to the per‑link input/output registers;
- optionally `SchedSrv` and `TimerSrv` when scheduling packets.

The plugin processes messages such as `OPEN_CHANNEL`, `DATA`, and `CLOSE` using
these services and updates the RAM locations from `Global.Link0Input` upward.


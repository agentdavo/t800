# Hello World Boot Example

This appendix shows a small program that fits inside the primary
bootstrap area. The code sends a "hello world" string over link 0
using the IServer protocol.

It performs the standard transputer start-up sequence:

1. Initialise the high and low priority queues with `mint`, `sthf` and `stlf`.
2. Clear the timer queues located at `TPtrLoc0` and `TPtrLoc1`.
3. Start the clocks via `sttimer` and clear any previous error state.
4. Reset all event and link channel pointers to `NotProcess.p`.

Once the machine state is known, the main routine reserves stack
space with `ajw`, loads the address of the string literal relative to
`IPtr` via `ldpi`, then calls a helper which transmits the string. The
helper builds a tiny IServer frame consisting of the request code,
stream ID and length, followed by the string bytes.

The code makes use of several of the newly implemented operations such
as `ldpi`, `outbyte`, `outword`, `lb`, `dup` and `pop`.

The companion unit test `HelloWorldSpec` loads the bytes into the ROM
via `MemAccessSrv` and checks that link 0 outputs "hello world\n".
This confirms that the fetch and channel subsystems function together
to run small programs from ROM.

See `link_architecture.md` for details on how the channel services and
future VCP will interact with memory.

For the complete source listing see `helloworld.md`.

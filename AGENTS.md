# AGENTS.md for Transputer Project

*(Rev 2025-06-17)*

## Overview
Guide for OpenAI Codex to assist development of the Transputer project, a SpinalHDL-based hardware design. Key code in `src/main/scala/transputer/`, tests in `src/test/scala/transputer/`, docs in `doc/`.

## Project Structure
| Path | Description |
|------|-------------|
| `src/main/scala/transputer/<Subsystem>/` | One `FiberPlugin` per subfolder with `Service.scala` |
| `src/main/scala/transputer/Generate.scala` | CLI for Verilog generation |
| `src/main/scala/transputer/Param.scala` | Plugin selection parameters |
| `src/test/scala/transputer/` | ScalaTest and SpinalSim tests |
| `gen/` | synthesis scripts and generated sources |
| `ext/SpinalHDL/` | SpinalHDL library official source code (git submodule) |
| `ext/.ivy2/` | ivy2 cached published SpinalHDL jar |
| `doc/SpinalHDL_api.md` | Pipeline and plugin DSL reference |
| `doc/SpinalHDL_bmb.md` | BMB bus guide |
| `doc/SpinalHDL_docs.txt` | SpinalHDL user documentation |
| `doc/Transputer_core.md` | CPU architecture notes |
| `doc/Transputer_links.md` | Link interface description |

## Development
```bash
sbt scalafmtAll                      # Format
sbt test                             # Test
sbt "runMain transputer.Generate"  # Generate Verilog
sbt bareBonesTest                    # Minimal compile & unit test
sbt bareBones                        # Minimal Verilog generation
# bareBonesTest compiles TestPlugins.scala which provides dummy services for
# the minimal build. Extend those dummies whenever new plugins require
# additional services.
```

## Coding Rules
- **Plugins**: One per file named `<Subsystem>Plugin.scala`.
- **Services**: Expose IO via `LinkPins(bundle)`. Use `Service` suffix (e.g., `FetchService`) and not `Srv`. Align with pipeline stages (Fetch, Decode, Execute, Memory, Writeback).
- **Pipeline**: Use DSL from spinal.lib.misc.pipeline (`Node`, `StageLink`, `CtrlLink`), not `RegNext`.
- **Tests**: Unit test new opcodes, checking A/B/C or memory.
- **Docs**: Update `doc/SpinalHDL_api.md` for new DSL patterns/services. Refer to `doc/SpinalHDL_docs.txt` for SpinalHDL library help.

## Pipeline DSL
- **Links**: `StageLink`, `S2MLink`, `ForkLink`, `JoinLink`.
- **Controls**: `ctrl.haltWhen`, `ctrl.throwIt`.
- **Rules**: Single `insert()` per `Payload`, read via `node(payload)`.

## PR Guidelines
- **Branch**: Use `feat/<milestone>` (e.g., `feat/m1-alu`). No direct `main` commits.
- **Template**:
  ```markdown
  ### What & Why
  - Added opcode <name>
  - Added <test-name>
  ### Validation
  - [x] sbt scalafmtAll
  - [x] sbt test
  Closes #<issue>
  ```
- **Conflicts**: Rebase with `git rebase origin/main`. Resolve via `codex resolve-conflicts`.

## CI Checks
| Stage | Command |
|-------|---------|
| Style | `sbt scalafmtAll` |
| Tests | `sbt test` |
| Verilog | `sbt "runMain transputer.Generate"` |

## Simulation
```scala
SimConfig.withWave.compile(new MyCore).doSim { dut =>
  dut.clockDomain.forkStimulus(10)
  SimTimeout(1000)
}
```
- Use `withVerilator`. See `doc/SpinalHDL_docs.txt`.
- Debug: Use `SPINALSIM_WORKSPACE`, `waitUntil`.

## Troubleshooting
- **Combinational Loop**: Single `insert()` per `Payload`.
- **Fiber Deadlock**: Use `buildBefore(lock)`.
- **NO DRIVER**: Add default values or `.setIdle()`.

## Hierarchy
Signals readable only in defining component or children. Share state via services/IO bundles.

### PR Message
Using the original PR template:

```markdown
### What & Why
- Optimized `AGENTS.md` for brevity and clarity, aligning with OpenAI demo style
- Retained essential development and validation guidance

### Validation
- [x] sbt scalafmtAll
- [x] sbt test

Closes #<issue>
```

Replace `<issue>` with the relevant issue number, if applicable.
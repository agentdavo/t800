# Transputer-T800 Core (SpinalHDL)

A cycle-accurate, open-source implementation of the **INMOS T800 Transputer** written in **SpinalHDL ≥ 1.9**.  
The core re-creates the original integer pipeline, floating-point unit, hardware scheduler and four serial links while remaining FPGA-friendly.

---

## Features (road-map)

| Phase | Status |
|-------|--------|
| Skeleton compile & Verilog export | ✅ Done |
| Primary integer opcodes (0x00-0x6F) | 🚧 WIP |
| Scheduler + Timer + memory ops | ✈️ queued |
| Four-link channel engine | ✈️ queued |
| Full IEEE-754 FPU | ✈️ queued |
| Transcendental FP ops | ✈️ queued |
| T800 compliance & Occam boot | ✈️ queued |
| Timing/area optimisation | ✈️ queued |

See **AGENTS.md** for ownership and workflow details.

---

## Ubuntu setup (tested on 20.04 / 22.04)

```bash
# Optional: green prompt so you can spot your dev container
echo 'RESET="\[$(tput sgr0)\]"'  >> ~/.bashrc
echo 'GREEN="\[$(tput setaf 2)\]"' >> ~/.bashrc
echo 'export PS1="${GREEN}\u:\W${RESET} $ "' >> ~/.bashrc

# Java 8 (required by SpinalHDL 1.9)
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install -y openjdk-8-jdk

# Make JDK 8 the default
sudo update-alternatives --set java  $(update-alternatives --list java  | grep java-8-openjdk)
sudo update-alternatives --set javac $(update-alternatives --list javac | grep java-8-openjdk)

# sbt & Verilator repos
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | \
  sudo tee /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add -

sudo apt-get update
sudo apt-get install -y sbt verilator git make g++ flex bison autoconf \
                       pkg-config libtool shtool libusb-1.0-0-dev libyaml-dev \
                       cpio bc unzip rsync mercurial
````

---

## Quick start

```bash
# build & run unit tests
sbt test

# generate synthesizable Verilog
sbt "runMain T800CoreVerilog"

# minimal behavioural sim (wave dumps to ./simWorkspace)
sbt test:runMain T800CoreSim
```

> **Prerequisites:** JDK 8, sbt 1.9+, SpinalHDL 1.9+, Verilator (optional), GTKWave for viewing waves.

---

## Repository layout

```
├── src/                # Hardware source (single-file core for now)
│   └── ims-t800-core.scala
├── test/               # ScalaTest / SpinalSim benches
├── doc/                # Architecture notes, opcode one-pagers, ADRs
├── synthesis/          # FPGA scripts and timing reports
├── .github/workflows/  # CI (lint, test, synth)
├── README.md           # This file
└── AGENTS.md           # Contributor guide
```

---

## Contributing

1. Read **AGENTS.md** for branch naming, style and CI gates.
2. Pick an open issue (or raise one) and assign yourself.
3. Replace `// TODO` stubs with RTL; add/extend unit tests in `/test`.
4. Run `sbt scalafmtCheckAll` and `sbt test`; make sure CI is green before merging.

### Code-style snippet

```scala
// Register
val Areg = Reg(UInt(32 bits)) init(0)
// Wire
val calc = UInt(32 bits)
calc := Areg + 1    // last-assignment-wins rule
```

Use **SpinalHDL’s pipeline DSL** (`spinal.lib.misc.pipeline`) for new multi-stage blocks.

---
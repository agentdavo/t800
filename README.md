# Transputer‑T800 Core (SpinalHDL)

A cycle‑accurate, open‑source implementation of the **INMOS T800 Transputer** written in **SpinalHDL ≥ 1.9**. The core re‑creates the original integer pipeline, floating‑point unit, hardware scheduler and four serial links while remaining FPGA‑friendly.

---

## Features (roadmap)

| Phase                               | Status    |
| ----------------------------------- | --------- |
| Skeleton compile & Verilog export   | ✅ Done    |
| Primary integer opcodes (0x00‑0x6F) | 🚧 WIP    |
| Scheduler + Timer + Memory ops      | ✈️ queued |
| Four‑link channel engine            | ✈️ queued |
| Full IEEE‑754 FPU                   | ✈️ queued |
| Transcendental FP ops               | ✈️ queued |
| T800 compliance & Occam boot        | ✈️ queued |
| Timing/area optimisation            | ✈️ queued |

See **AGENTS.md** for milestone ownership and contributor workflow.

---

## Ubuntu setup (tested on 20.04 / 22.04)

The core builds happily on most modern Linux distributions. If you’re starting from a bare Ubuntu box, the script below installs Java 8, **sbt**, Verilator and a few helper packages:

```bash
echo 'RESET="\[$(tput sgr0)\]"' >> $WORKDIR/.bashrc
echo 'GREEN="\[$(tput setaf 2)\]"' >> $WORKDIR/.bashrc
echo 'export PS1="${GREEN}\u:\W${RESET} $ "' >> $WORKDIR/.bashrc

sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-8-jdk -y

# Set OpenJDK 8 as the default Java version
JAVA8_PATH=$(update-alternatives --list java | grep java-8-openjdk)
JAVAC8_PATH=$(update-alternatives --list javac | grep java-8-openjdk)

if [ -n "$JAVA8_PATH" ] && [ -n "$JAVAC8_PATH" ]; then
    sudo update-alternatives --set java "$JAVA8_PATH"
    sudo update-alternatives --set javac "$JAVAC8_PATH"
else
    echo "Error: OpenJDK 8 paths not found in update-alternatives."
    exit 1
fi

# Verify installation
java -version
javac -version

echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add

sudo apt-get update
sudo apt-get install -y sbt
sudo apt-get install -y verilator
sudo apt-get install -y git make autoconf g++ flex bison
sudo apt-get install -y pkg-config shtool libtool cpio bc unzip rsync mercurial
sudo apt-get install -y libusb-1.0-0-dev libyaml-dev
```

Once installed you can jump straight to the **Quick start** below.

---

## Quick start

```bash
git clone https://github.com/agentdavo/t800.git
cd t800

# Build & run unit tests
sbt test

# Generate synthesizable Verilog (build/ims‑t800‑core.v)
sbt "runMain T800CoreVerilog"

# Minimal behavioural sim (waveform dumps to ./simWorkspace)
sbt test:runMain T800CoreSim
```

> **Prerequisites:** JDK 11+, SBT 1.9+, SpinalHDL 1.9+, Verilator (optional for lint), GTKWave for waves.

---

## Repository layout

```
├── src/                # Hardware source (single‑file core for now)
│   └── ims‑t800‑core.scala
├── test/               # ScalaTest/SpinalSim benches
├── doc/                # Architecture notes, opcode one‑pagers, ADRs
│   └── arch/…
│   └── spinalHDL.html  # SpinalHDL documentation
├── .github/workflows/  # CI scripts (lint, test, RTL, synth)
├── AGENTS.md           # Automated‑agent workflow & coding standards
└── README.md           # This file
```

---

## Contributing

1. Read **AGENTS.md** for branch naming, code‑style and CI gates.
2. Pick an open issue (or raise one) and assign yourself or the relevant bot.
3. Replace `// TODO` stubs with real RTL; add/extend unit tests under `/test`.
4. Ensure `sbt +test` and CI are green, then open a PR.

### Code style snippets

```scala
// Registers
val Areg = Reg(UInt(32 bits)) init(0)
// Wire
val calc = UInt(32 bits)
calc := Areg + 1  // last‑assignment‑wins rule
```

Use **SpinalHDL’s pipeline DSL** (`spinal.lib.misc.pipeline`) for new multi‑stage blocks to ease retiming.

---

## License

MIT

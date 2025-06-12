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

See **AGENTS.md** for contribution workflow details.

---

## Ubuntu setup

```bash
# Optional: green prompt so you can spot your dev container
echo 'RESET="\[$(tput sgr0)\]"'  >> ~/.bashrc
echo 'GREEN="\[$(tput setaf 2)\]"' >> ~/.bashrc
echo 'export PS1="${GREEN}\u:\W${RESET} $ "' >> ~/.bashrc

# Java 8 (required by SpinalHDL 1.9)
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install -y openjdk-8-jdk
sudo update-alternatives --set java  $(update-alternatives --list java  | grep java-8-openjdk)
sudo update-alternatives --set javac $(update-alternatives --list javac | grep java-8-openjdk)

# sbt & tools
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
# clone with the SpinalHDL sub-module
git clone --recursive https://github.com/your-org/transputer-t800.git
cd transputer-t800

# build & run unit tests
sbt test

# generate synthesizable Verilog
sbt "runMain t800.T800CoreVerilog"

# minimal behavioural sim (wave dumps to ./simWorkspace)
sbt test:runMain t800.T800CoreSim
```

> **Optional:** export `SPINALHDL_FROM_SOURCE=0` to pull pre-built jars instead of the sub-module code.

---

## Repository layout

```
transputer-t800/
├── build.sbt
├── SpinalHDL/            # ← Git sub-module (if building Spinal from source)
├── src/
│   ├── main/scala/t800/T800Core.scala
│   └── test/scala/t800/T800CoreSim.scala
├── doc/ …
├── synthesis/ …
├── README.md
└── AGENTS.md
```

---

## Contributing

1. Read **AGENTS.md** for branch naming, style and CI gates.
2. Pick an open issue (or raise one) and assign yourself.
3. Implement the RTL, add/extend tests, run `sbt test`, and keep CI green.

---
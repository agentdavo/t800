# T9000 FPGA Synthesis Makefile
# Target: ECP5-25K FPGA using open-source toolchain

PROJECT = t9000
TOP_MODULE = T9000_FPGA
DEVICE = 25k
PACKAGE = CABGA256
SPEED = 6
FREQ = 24

# Tools
SBT = sbt
YOSYS = yosys
NEXTPNR = nextpnr-ecp5
ECPPACK = ecppack
OPENOCD = openFPGALoader

# Source files
SCALA_SRC = ../src/main/scala/transputer/T9000_FPGA.scala
VERILOG_SRC = $(TOP_MODULE).v
YOSYS_SCRIPT = $(PROJECT).ys
LPF_FILE = $(PROJECT).lpf

# Generated files
JSON_FILE = $(PROJECT).json
CONFIG_FILE = $(PROJECT).config
BITSTREAM = $(PROJECT).bit

.PHONY: all clean sbt yosys nextpnr pack burn help

all: sbt yosys nextpnr pack

help:
	@echo "T9000 FPGA Synthesis Flow"
	@echo "========================"
	@echo "Targets:"
	@echo "  all      - Complete synthesis flow (sbt -> yosys -> nextpnr -> pack)"
	@echo "  sbt      - Generate Verilog from Scala"
	@echo "  yosys    - Synthesize to netlist"
	@echo "  nextpnr  - Place and route"
	@echo "  pack     - Generate bitstream"
	@echo "  burn     - Program FPGA"
	@echo "  clean    - Remove generated files"
	@echo "  stats    - Show synthesis statistics"
	@echo ""
	@echo "Configuration:"
	@echo "  Device:   $(DEVICE)"
	@echo "  Package:  $(PACKAGE)"
	@echo "  Speed:    $(SPEED)"
	@echo "  Freq:     $(FREQ) MHz"

sbt: $(VERILOG_SRC)

$(VERILOG_SRC): $(SCALA_SRC)
	@echo "=== Generating Verilog from Scala ==="
	cd .. && $(SBT) "runMain transputer.T9000_FPGA_Top"
	@echo "Looking for generated Verilog file..."
	@if [ -f "$(TOP_MODULE).v" ]; then \
		echo "Verilog generation completed: $(VERILOG_SRC)"; \
	elif [ -f "../$(TOP_MODULE).v" ]; then \
		mv ../$(TOP_MODULE).v .; \
		echo "Verilog moved from parent directory: $(VERILOG_SRC)"; \
	elif [ -f "../fpga/$(TOP_MODULE).v" ]; then \
		mv ../fpga/$(TOP_MODULE).v .; \
		echo "Verilog moved from fpga directory: $(VERILOG_SRC)"; \
	else \
		echo "ERROR: Verilog file not found in expected locations"; \
		ls -la ../ | grep -E '\.v$$'; \
		ls -la ../fpga/ | grep -E '\.v$$'; \
		exit 1; \
	fi

yosys: $(JSON_FILE)

$(JSON_FILE): $(VERILOG_SRC) $(YOSYS_SCRIPT)
	@echo "=== Running Yosys synthesis ==="
	$(YOSYS) $(YOSYS_SCRIPT)
	@echo "Synthesis completed: $(JSON_FILE)"

nextpnr: $(CONFIG_FILE)

$(CONFIG_FILE): $(JSON_FILE) $(LPF_FILE)
	@echo "=== Running nextpnr place and route ==="
	$(NEXTPNR) \
		--$(DEVICE) \
		--package $(PACKAGE) \
		--speed $(SPEED) \
		--json $(JSON_FILE) \
		--lpf $(LPF_FILE) \
		--textcfg $(CONFIG_FILE) \
		--freq $(FREQ) \
		--timing-allow-fail
	@echo "Place and route completed: $(CONFIG_FILE)"

pack: $(BITSTREAM)

$(BITSTREAM): $(CONFIG_FILE)
	@echo "=== Generating bitstream ==="
	$(ECPPACK) $(CONFIG_FILE) $(BITSTREAM)
	@echo "Bitstream generation completed: $(BITSTREAM)"

burn: $(BITSTREAM)
	@echo "=== Programming FPGA ==="
	$(OPENOCD) $(BITSTREAM)

stats: $(JSON_FILE)
	@echo "=== Synthesis Statistics ==="
	@if [ -f synthesis_report.txt ]; then \
		cat synthesis_report.txt; \
	else \
		echo "Run 'make yosys' first to generate statistics"; \
	fi

clean:
	@echo "=== Cleaning generated files ==="
	rm -f $(VERILOG_SRC)
	rm -f $(JSON_FILE)
	rm -f $(CONFIG_FILE)
	rm -f $(BITSTREAM)
	rm -f synthesis_report.txt
	rm -f *.log
	rm -f *.rpt
	@echo "Clean completed"

# Debug targets
debug-verilog: $(VERILOG_SRC)
	@echo "=== Verilog Preview ==="
	@head -50 $(VERILOG_SRC)

debug-json: $(JSON_FILE)
	@echo "=== JSON Netlist Info ==="
	@wc -l $(JSON_FILE)
	@echo "Modules in netlist:"
	@grep "\"type\":" $(JSON_FILE) | sort | uniq -c

# Phony targets
.PRECIOUS: $(VERILOG_SRC) $(JSON_FILE) $(CONFIG_FILE)
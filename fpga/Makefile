# T9000 FPGA Synthesis Makefile
# Target: ECP5-25K FPGA using open-source toolchain

PROJECT = t9000
TOP_MODULE = T9000_FPGA
DEVICE = 25k
PACKAGE = CABGA256
SPEED = 6
FREQ = 100

# Tools - check both system PATH and oss-cad-suite
SBT = sbt
YOSYS = yosys
NEXTPNR = $(shell which nextpnr-ecp5 2>/dev/null || echo /opt/oss-cad-suite/bin/nextpnr-ecp5)
ECPPACK = ecppack
OPENOCD = $(shell which openFPGALoader 2>/dev/null || echo /opt/oss-cad-suite/bin/openFPGALoader)

# Source files
SCALA_SRC = ../src/main/scala/transputer/Generate.scala
VERILOG_SRC = $(TOP_MODULE).v
YOSYS_SCRIPT = $(PROJECT).ys
LPF_FILE = $(PROJECT).lpf
GENERATED_DIR = ../generated

# Generated files
JSON_FILE = $(PROJECT).json
CONFIG_FILE = $(PROJECT).config
BITSTREAM = $(PROJECT).bit

.PHONY: all clean sbt yosys nextpnr pack burn help test minimal check-tools

all: sbt yosys nextpnr pack

help:
	@echo "T9000 FPGA Synthesis Flow"
	@echo "========================"
	@echo "Targets:"
	@echo "  all         - Complete synthesis flow (sbt -> yosys -> nextpnr -> pack)"
	@echo "  sbt         - Generate Verilog from Scala"
	@echo "  yosys       - Synthesize to netlist"
	@echo "  nextpnr     - Place and route"
	@echo "  pack        - Generate bitstream"
	@echo "  burn        - Program FPGA"
	@echo "  clean       - Remove generated files"
	@echo "  stats       - Show synthesis statistics"
	@echo "  test        - Test Verilog generation only"
	@echo "  minimal     - Generate minimal test design"
	@echo "  check-tools - Check if required tools are installed"
	@echo ""
	@echo "Configuration:"
	@echo "  Device:   $(DEVICE)"
	@echo "  Package:  $(PACKAGE)"
	@echo "  Speed:    $(SPEED)"
	@echo "  Freq:     $(FREQ) MHz"

sbt: $(VERILOG_SRC)

$(VERILOG_SRC): $(SCALA_SRC)
	@echo "=== Generating Verilog from Scala ==="
	cd .. && $(SBT) "runMain transputer.Generate --fpga=ecp5 --hex scripts/hex/hello-iserver-rom.hex"
	@echo "Looking for generated Verilog file..."
	@if [ -f "$(TOP_MODULE).v" ]; then \
		echo "Verilog generation completed: $(VERILOG_SRC)"; \
	elif [ -f "$(GENERATED_DIR)/$(TOP_MODULE).v" ]; then \
		cp "$(GENERATED_DIR)/$(TOP_MODULE).v" .; \
		echo "Verilog copied from generated directory: $(VERILOG_SRC)"; \
	elif [ -f "../$(TOP_MODULE).v" ]; then \
		mv "../$(TOP_MODULE).v" .; \
		echo "Verilog moved from parent directory: $(VERILOG_SRC)"; \
	elif [ -f "fpga/$(TOP_MODULE).v" ]; then \
		mv "fpga/$(TOP_MODULE).v" .; \
		echo "Verilog moved from fpga subdirectory: $(VERILOG_SRC)"; \
	else \
		echo "ERROR: Verilog file not found in expected locations"; \
		echo "Searching for .v files..."; \
		find .. -name "*.v" -type f 2>/dev/null | head -10; \
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
		--timing-allow-fail \
		--lpf-allow-unconstrained
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

# Test targets
test: $(VERILOG_SRC)
	@echo "=== Verilog Generation Test Successful ==="
	@echo "Generated file: $(VERILOG_SRC)"
	@echo "File size: $$(wc -l < $(VERILOG_SRC)) lines"
	@echo "Top module found: $$(grep -c "module $(TOP_MODULE)" $(VERILOG_SRC))"

minimal:
	@echo "=== Generating Minimal Test Design ==="
	cd .. && $(SBT) "runMain transputer.Generate --fpga=ecp5 --demo"
	@if [ -f "$(TOP_MODULE).v" ]; then \
		echo "Minimal design generated: $(VERILOG_SRC)"; \
	elif [ -f "$(GENERATED_DIR)/$(TOP_MODULE).v" ]; then \
		cp "$(GENERATED_DIR)/$(TOP_MODULE).v" .; \
		echo "Minimal design copied: $(VERILOG_SRC)"; \
	else \
		echo "ERROR: Minimal design generation failed"; \
		exit 1; \
	fi

check-tools:
	@echo "=== Checking Required Tools ==="
	@echo -n "SBT: "; which $(SBT) >/dev/null 2>&1 && echo "✓ Found" || echo "✗ Missing"
	@echo -n "Yosys: "; which $(YOSYS) >/dev/null 2>&1 && echo "✓ Found ($$($(YOSYS) -V 2>/dev/null | head -1))" || echo "✗ Missing"
	@echo -n "nextpnr-ecp5: "; which $(NEXTPNR) >/dev/null 2>&1 && echo "✓ Found" || echo "✗ Missing"
	@echo -n "ecppack: "; which $(ECPPACK) >/dev/null 2>&1 && echo "✓ Found" || echo "✗ Missing"
	@echo -n "openFPGALoader: "; which $(OPENOCD) >/dev/null 2>&1 && echo "✓ Found" || echo "✗ Missing"

# Quick synthesis test (just to netlist)
quick-synth: $(JSON_FILE)
	@echo "=== Quick Synthesis Complete ==="
	@echo "Netlist size: $$(wc -l < $(JSON_FILE)) lines"

# Phony targets
.PRECIOUS: $(VERILOG_SRC) $(JSON_FILE) $(CONFIG_FILE)
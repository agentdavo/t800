# Makefile for T9000 FPU Transputer synthesis and programming on Colorlight 5A-75E
# Assumes Transputer.v is in gen/src/verilog, constraints in constraints/ecp5.lpf

VERILOG = gen/src/verilog/Transputer.v
LPF = constraints/ecp5.lpf
DEVICE = LFE5U-45F
OUTPUT_DIR = bin

compile: $(OUTPUT_DIR)/Transputer.bit

prog: $(OUTPUT_DIR)/Transputer.bit
	fujprog $<

$(OUTPUT_DIR)/Transputer.json: $(VERILOG)
	mkdir -p $(OUTPUT_DIR)
	rm -f Transputer.v*.bin
	cp $(VERILOG)*.bin $(OUTPUT_DIR)/ || true
	yosys \
		-p "synth_ecp5 -top Transputer -json $@" \
		$<

$(OUTPUT_DIR)/Transputer.config: $(OUTPUT_DIR)/Transputer.json
	nextpnr-ecp5 \
		--json $< \
		--textcfg $@ \
		--lpf $(LPF) \
		--$(DEVICE) \
		--timing-allow-fail \
		--freq 250 \
		--placer heap

$(OUTPUT_DIR)/Transputer.bit: $(OUTPUT_DIR)/Transputer.config
	ecppack $< $@

clean:
	$(RM) -rf $(OUTPUT_DIR) *.bin

.PHONY: compile prog clean
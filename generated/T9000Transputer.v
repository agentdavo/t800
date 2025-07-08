// Generator : SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
// Component : T9000Transputer
// Git hash  : 3e7cc9e1babbc090674147415474973da6786efc

`timescale 1ns/1ps

module T9000Transputer (
  output wire          systemBus_cmd_valid,
  input  wire          systemBus_cmd_ready,
  output wire          systemBus_cmd_payload_last,
  output wire [3:0]    systemBus_cmd_payload_fragment_source,
  output wire [0:0]    systemBus_cmd_payload_fragment_opcode,
  output wire [31:0]   systemBus_cmd_payload_fragment_address,
  output wire [3:0]    systemBus_cmd_payload_fragment_length,
  output wire [127:0]  systemBus_cmd_payload_fragment_data,
  output wire [15:0]   systemBus_cmd_payload_fragment_mask,
  input  wire          systemBus_rsp_valid,
  output wire          systemBus_rsp_ready,
  input  wire          systemBus_rsp_payload_last,
  input  wire [3:0]    systemBus_rsp_payload_fragment_source,
  input  wire [0:0]    systemBus_rsp_payload_fragment_opcode,
  input  wire [127:0]  systemBus_rsp_payload_fragment_data,
  input  wire          clk,
  input  wire          reset
);
  localparam ProcessState_RUNNING = 2'd0;
  localparam ProcessState_READY = 2'd1;
  localparam ProcessState_WAITING = 2'd2;
  localparam ProcessState_TERMINATED = 2'd3;

  wire                streamFifo_2_io_push_valid;
  reg                 streamFifo_2_io_pop_ready;
  wire                streamFifo_3_io_push_valid;
  reg                 streamFifo_3_io_pop_ready;
  wire                bmbUpSizerBridge_1_io_input_cmd_ready;
  wire                bmbUpSizerBridge_1_io_input_rsp_valid;
  wire                bmbUpSizerBridge_1_io_input_rsp_payload_last;
  wire       [0:0]    bmbUpSizerBridge_1_io_input_rsp_payload_fragment_source;
  wire       [0:0]    bmbUpSizerBridge_1_io_input_rsp_payload_fragment_opcode;
  wire       [63:0]   bmbUpSizerBridge_1_io_input_rsp_payload_fragment_data;
  wire                bmbUpSizerBridge_1_io_output_cmd_valid;
  wire                bmbUpSizerBridge_1_io_output_cmd_payload_last;
  wire       [0:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_source;
  wire       [0:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_opcode;
  wire       [31:0]   bmbUpSizerBridge_1_io_output_cmd_payload_fragment_address;
  wire       [2:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_length;
  wire       [127:0]  bmbUpSizerBridge_1_io_output_cmd_payload_fragment_data;
  wire       [15:0]   bmbUpSizerBridge_1_io_output_cmd_payload_fragment_mask;
  wire       [1:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_context;
  wire                bmbUpSizerBridge_1_io_output_rsp_ready;
  wire                streamFifo_2_io_push_ready;
  wire                streamFifo_2_io_pop_valid;
  wire       [31:0]   streamFifo_2_io_pop_payload;
  wire       [4:0]    streamFifo_2_io_occupancy;
  wire       [4:0]    streamFifo_2_io_availability;
  wire                streamFifo_3_io_push_ready;
  wire                streamFifo_3_io_pop_valid;
  wire       [31:0]   streamFifo_3_io_pop_payload;
  wire       [4:0]    streamFifo_3_io_occupancy;
  wire       [4:0]    streamFifo_3_io_availability;
  wire       [31:0]   _zz_io_push_payload;
  wire                _zz_io_push_valid;
  wire                _zz_io_push_valid_1;
  wire       [31:0]   _zz_io_push_payload_1;
  wire                _zz_io_push_valid_2;
  wire                _zz_io_push_valid_3;
  wire                writeback_down_isReady;
  wire                writeback_down_isValid;
  wire                execute_down_isReady;
  wire                execute_down_isValid;
  wire                addressCache_down_isReady;
  wire                addressCache_down_isValid;
  wire                localDecode_down_isReady;
  wire                localDecode_down_isValid;
  wire                fetchGroup_down_isReady;
  wire                fetchGroup_down_isValid;
  wire                writeback_down_isFiring;
  wire                execute_down_isFiring;
  wire                addressCache_down_isFiring;
  wire                localDecode_down_isFiring;
  wire                fetchGroup_down_isFiring;
  wire                _zz_io_input_cmd_valid;
  wire                _zz_io_input_cmd_valid_1;
  wire       [63:0]   _zz_io_input_cmd_payload_fragment_data;
  wire       [7:0]    _zz_io_input_cmd_payload_fragment_mask;
  wire                _zz_io_input_rsp_ready;
  reg        [31:0]   _zz_io_input_cmd_payload_fragment_address;
  reg                 _zz_io_input_cmd_valid_2;
  reg        [31:0]   _zz_when_SchedulerPlugin_l170;
  reg        [31:0]   _zz_when_SchedulerPlugin_l170_1;
  reg                 _zz_when_SchedulerPlugin_l131;
  reg                 _zz_when_SchedulerPlugin_l131_1;
  reg                 _zz_when_SchedulerPlugin_l131_2;
  reg        [1:0]    _zz_when_SchedulerPlugin_l168;
  wire                io_push_fire;
  wire                io_push_fire_1;
  wire                when_SchedulerPlugin_l131;
  wire                when_SchedulerPlugin_l163;
  wire                when_SchedulerPlugin_l168;
  wire                when_SchedulerPlugin_l170;
  `ifndef SYNTHESIS
  reg [79:0] _zz_when_SchedulerPlugin_l168_string;
  `endif


  BmbUpSizerBridge bmbUpSizerBridge_1 (
    .io_input_cmd_valid                     (_zz_io_input_cmd_valid_1                                       ), //i
    .io_input_cmd_ready                     (bmbUpSizerBridge_1_io_input_cmd_ready                          ), //o
    .io_input_cmd_payload_last              (1'b1                                                           ), //i
    .io_input_cmd_payload_fragment_source   (1'b0                                                           ), //i
    .io_input_cmd_payload_fragment_opcode   (1'b0                                                           ), //i
    .io_input_cmd_payload_fragment_address  (_zz_io_input_cmd_payload_fragment_address[31:0]                ), //i
    .io_input_cmd_payload_fragment_length   (3'b111                                                         ), //i
    .io_input_cmd_payload_fragment_data     (_zz_io_input_cmd_payload_fragment_data[63:0]                   ), //i
    .io_input_cmd_payload_fragment_mask     (_zz_io_input_cmd_payload_fragment_mask[7:0]                    ), //i
    .io_input_rsp_valid                     (bmbUpSizerBridge_1_io_input_rsp_valid                          ), //o
    .io_input_rsp_ready                     (_zz_io_input_rsp_ready                                         ), //i
    .io_input_rsp_payload_last              (bmbUpSizerBridge_1_io_input_rsp_payload_last                   ), //o
    .io_input_rsp_payload_fragment_source   (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_source        ), //o
    .io_input_rsp_payload_fragment_opcode   (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_opcode        ), //o
    .io_input_rsp_payload_fragment_data     (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_data[63:0]    ), //o
    .io_output_cmd_valid                    (bmbUpSizerBridge_1_io_output_cmd_valid                         ), //o
    .io_output_cmd_ready                    (1'b1                                                           ), //i
    .io_output_cmd_payload_last             (bmbUpSizerBridge_1_io_output_cmd_payload_last                  ), //o
    .io_output_cmd_payload_fragment_source  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_source       ), //o
    .io_output_cmd_payload_fragment_opcode  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_opcode       ), //o
    .io_output_cmd_payload_fragment_address (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_address[31:0]), //o
    .io_output_cmd_payload_fragment_length  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_length[2:0]  ), //o
    .io_output_cmd_payload_fragment_data    (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_data[127:0]  ), //o
    .io_output_cmd_payload_fragment_mask    (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_mask[15:0]   ), //o
    .io_output_cmd_payload_fragment_context (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_context[1:0] ), //o
    .io_output_rsp_valid                    (1'b0                                                           ), //i
    .io_output_rsp_ready                    (bmbUpSizerBridge_1_io_output_rsp_ready                         ), //o
    .io_output_rsp_payload_last             (1'b1                                                           ), //i
    .io_output_rsp_payload_fragment_source  (1'b0                                                           ), //i
    .io_output_rsp_payload_fragment_opcode  (                                                               ), //i
    .io_output_rsp_payload_fragment_data    (128'h0                                                         ), //i
    .io_output_rsp_payload_fragment_context (                                                               ), //i
    .clk                                    (clk                                                            ), //i
    .reset                                  (reset                                                          )  //i
  );
  StreamFifo streamFifo_2 (
    .io_push_valid   (streamFifo_2_io_push_valid       ), //i
    .io_push_ready   (streamFifo_2_io_push_ready       ), //o
    .io_push_payload (_zz_io_push_payload_1[31:0]      ), //i
    .io_pop_valid    (streamFifo_2_io_pop_valid        ), //o
    .io_pop_ready    (streamFifo_2_io_pop_ready        ), //i
    .io_pop_payload  (streamFifo_2_io_pop_payload[31:0]), //o
    .io_flush        (1'b0                             ), //i
    .io_occupancy    (streamFifo_2_io_occupancy[4:0]   ), //o
    .io_availability (streamFifo_2_io_availability[4:0]), //o
    .clk             (clk                              ), //i
    .reset           (reset                            )  //i
  );
  StreamFifo streamFifo_3 (
    .io_push_valid   (streamFifo_3_io_push_valid       ), //i
    .io_push_ready   (streamFifo_3_io_push_ready       ), //o
    .io_push_payload (_zz_io_push_payload[31:0]        ), //i
    .io_pop_valid    (streamFifo_3_io_pop_valid        ), //o
    .io_pop_ready    (streamFifo_3_io_pop_ready        ), //i
    .io_pop_payload  (streamFifo_3_io_pop_payload[31:0]), //o
    .io_flush        (1'b0                             ), //i
    .io_occupancy    (streamFifo_3_io_occupancy[4:0]   ), //o
    .io_availability (streamFifo_3_io_availability[4:0]), //o
    .clk             (clk                              ), //i
    .reset           (reset                            )  //i
  );
  `ifndef SYNTHESIS
  always @(*) begin
    case(_zz_when_SchedulerPlugin_l168)
      ProcessState_RUNNING : _zz_when_SchedulerPlugin_l168_string = "RUNNING   ";
      ProcessState_READY : _zz_when_SchedulerPlugin_l168_string = "READY     ";
      ProcessState_WAITING : _zz_when_SchedulerPlugin_l168_string = "WAITING   ";
      ProcessState_TERMINATED : _zz_when_SchedulerPlugin_l168_string = "TERMINATED";
      default : _zz_when_SchedulerPlugin_l168_string = "??????????";
    endcase
  end
  `endif

  assign fetchGroup_down_isFiring = (fetchGroup_down_isValid && fetchGroup_down_isReady);
  assign fetchGroup_down_isValid = 1'b1;
  assign fetchGroup_down_isReady = 1'b1;
  assign localDecode_down_isFiring = (localDecode_down_isValid && localDecode_down_isReady);
  assign localDecode_down_isValid = 1'b1;
  assign localDecode_down_isReady = 1'b1;
  assign addressCache_down_isFiring = (addressCache_down_isValid && addressCache_down_isReady);
  assign addressCache_down_isValid = 1'b1;
  assign addressCache_down_isReady = 1'b1;
  assign execute_down_isFiring = (execute_down_isValid && execute_down_isReady);
  assign execute_down_isValid = 1'b1;
  assign execute_down_isReady = 1'b1;
  assign writeback_down_isFiring = (writeback_down_isValid && writeback_down_isReady);
  assign writeback_down_isValid = 1'b1;
  assign writeback_down_isReady = 1'b1;
  assign systemBus_cmd_valid = 1'b0;
  assign systemBus_cmd_payload_fragment_source = 4'b0000;
  assign systemBus_cmd_payload_fragment_opcode = 1'b0;
  assign systemBus_cmd_payload_fragment_address = 32'h0;
  assign systemBus_cmd_payload_fragment_length = 4'b0000;
  assign systemBus_cmd_payload_fragment_data = 128'h0;
  assign systemBus_cmd_payload_fragment_mask = 16'h0;
  assign systemBus_cmd_payload_last = 1'b1;
  assign systemBus_rsp_ready = 1'b1;
  assign _zz_io_input_cmd_valid_1 = (_zz_io_input_cmd_valid && (! _zz_io_input_cmd_valid_2));
  assign _zz_io_input_rsp_ready = _zz_io_input_cmd_valid;
  assign streamFifo_2_io_push_valid = (_zz_io_push_valid_3 && _zz_io_push_valid_2);
  assign io_push_fire = (streamFifo_2_io_push_valid && streamFifo_2_io_push_ready);
  assign streamFifo_3_io_push_valid = (_zz_io_push_valid_1 && (! _zz_io_push_valid));
  assign io_push_fire_1 = (streamFifo_3_io_push_valid && streamFifo_3_io_push_ready);
  always @(*) begin
    streamFifo_2_io_pop_ready = 1'b0;
    if(when_SchedulerPlugin_l131) begin
      if(streamFifo_2_io_pop_valid) begin
        streamFifo_2_io_pop_ready = 1'b1;
      end
    end
  end

  always @(*) begin
    streamFifo_3_io_pop_ready = 1'b0;
    if(when_SchedulerPlugin_l131) begin
      if(!streamFifo_2_io_pop_valid) begin
        if(streamFifo_3_io_pop_valid) begin
          streamFifo_3_io_pop_ready = 1'b1;
        end
      end
    end
  end

  assign when_SchedulerPlugin_l131 = ((_zz_when_SchedulerPlugin_l131 || _zz_when_SchedulerPlugin_l131_1) && _zz_when_SchedulerPlugin_l131_2);
  assign when_SchedulerPlugin_l163 = ((! _zz_when_SchedulerPlugin_l131_2) && ((5'h0 < streamFifo_2_io_occupancy) || (5'h0 < streamFifo_3_io_occupancy)));
  assign when_SchedulerPlugin_l168 = ((((_zz_when_SchedulerPlugin_l168 == ProcessState_RUNNING) && streamFifo_2_io_pop_valid) && (! _zz_when_SchedulerPlugin_l131)) && (! _zz_when_SchedulerPlugin_l131_1));
  assign when_SchedulerPlugin_l170 = (_zz_when_SchedulerPlugin_l170 != 32'h0);
  always @(posedge clk) begin
    if(reset) begin
      _zz_io_input_cmd_payload_fragment_address <= 32'h80000000;
      _zz_io_input_cmd_valid_2 <= 1'b0;
      _zz_when_SchedulerPlugin_l170 <= 32'h0;
      _zz_when_SchedulerPlugin_l170_1 <= 32'h0;
      _zz_when_SchedulerPlugin_l131 <= 1'b0;
      _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
      _zz_when_SchedulerPlugin_l131_2 <= 1'b1;
      _zz_when_SchedulerPlugin_l168 <= ProcessState_READY;
    end else begin
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing SchedulerPlugin v0.2"); // core.scala:L566
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing TimerPlugin v0.2"); // core.scala:L566
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing PipelineBuilderPlugin v0.5"); // core.scala:L566
          end
        `endif
      `endif
      if((_zz_io_input_cmd_valid_1 && bmbUpSizerBridge_1_io_input_cmd_ready)) begin
        _zz_io_input_cmd_valid_2 <= 1'b1;
        _zz_io_input_cmd_payload_fragment_address <= (_zz_io_input_cmd_payload_fragment_address + 32'h00000008);
      end
      if((bmbUpSizerBridge_1_io_input_rsp_valid && _zz_io_input_rsp_ready)) begin
        _zz_io_input_cmd_valid_2 <= 1'b0;
      end
      if(when_SchedulerPlugin_l131) begin
        if(streamFifo_2_io_pop_valid) begin
          _zz_when_SchedulerPlugin_l170 <= _zz_when_SchedulerPlugin_l170_1;
          _zz_when_SchedulerPlugin_l170_1 <= streamFifo_2_io_pop_payload;
          _zz_when_SchedulerPlugin_l168 <= ProcessState_RUNNING;
          _zz_when_SchedulerPlugin_l131 <= 1'b0;
          _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
        end else begin
          if(streamFifo_3_io_pop_valid) begin
            _zz_when_SchedulerPlugin_l170 <= _zz_when_SchedulerPlugin_l170_1;
            _zz_when_SchedulerPlugin_l170_1 <= streamFifo_3_io_pop_payload;
            _zz_when_SchedulerPlugin_l168 <= ProcessState_RUNNING;
            _zz_when_SchedulerPlugin_l131 <= 1'b0;
            _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
          end else begin
            _zz_when_SchedulerPlugin_l168 <= ProcessState_WAITING;
            _zz_when_SchedulerPlugin_l131_2 <= 1'b0;
          end
        end
      end
      if(when_SchedulerPlugin_l163) begin
        _zz_when_SchedulerPlugin_l131_2 <= 1'b1;
      end
      if(when_SchedulerPlugin_l168) begin
        if(when_SchedulerPlugin_l170) begin
          _zz_when_SchedulerPlugin_l131_1 <= 1'b1;
        end
      end
    end
  end


endmodule

//StreamFifo_1 replaced by StreamFifo

module StreamFifo (
  input  wire          io_push_valid,
  output wire          io_push_ready,
  input  wire [31:0]   io_push_payload,
  output wire          io_pop_valid,
  input  wire          io_pop_ready,
  output wire [31:0]   io_pop_payload,
  input  wire          io_flush,
  output wire [4:0]    io_occupancy,
  output wire [4:0]    io_availability,
  input  wire          clk,
  input  wire          reset
);

  reg        [31:0]   logic_ram_spinal_port1;
  wire       [31:0]   _zz_logic_ram_port;
  reg                 _zz_1;
  wire                logic_ptr_doPush;
  wire                logic_ptr_doPop;
  wire                logic_ptr_full;
  wire                logic_ptr_empty;
  reg        [4:0]    logic_ptr_push;
  reg        [4:0]    logic_ptr_pop;
  wire       [4:0]    logic_ptr_occupancy;
  wire       [4:0]    logic_ptr_popOnIo;
  wire                when_Stream_l1383;
  reg                 logic_ptr_wentUp;
  wire                io_push_fire;
  wire                logic_push_onRam_write_valid;
  wire       [3:0]    logic_push_onRam_write_payload_address;
  wire       [31:0]   logic_push_onRam_write_payload_data;
  wire                logic_pop_addressGen_valid;
  reg                 logic_pop_addressGen_ready;
  wire       [3:0]    logic_pop_addressGen_payload;
  wire                logic_pop_addressGen_fire;
  wire                logic_pop_sync_readArbitation_valid;
  wire                logic_pop_sync_readArbitation_ready;
  wire       [3:0]    logic_pop_sync_readArbitation_payload;
  reg                 logic_pop_addressGen_rValid;
  reg        [3:0]    logic_pop_addressGen_rData;
  wire                when_Stream_l448;
  wire                logic_pop_sync_readPort_cmd_valid;
  wire       [3:0]    logic_pop_sync_readPort_cmd_payload;
  wire       [31:0]   logic_pop_sync_readPort_rsp;
  wire                logic_pop_addressGen_toFlowFire_valid;
  wire       [3:0]    logic_pop_addressGen_toFlowFire_payload;
  wire                logic_pop_sync_readArbitation_translated_valid;
  wire                logic_pop_sync_readArbitation_translated_ready;
  wire       [31:0]   logic_pop_sync_readArbitation_translated_payload;
  wire                logic_pop_sync_readArbitation_fire;
  reg        [4:0]    logic_pop_sync_popReg;
  reg [31:0] logic_ram [0:15];

  assign _zz_logic_ram_port = logic_push_onRam_write_payload_data;
  always @(posedge clk) begin
    if(_zz_1) begin
      logic_ram[logic_push_onRam_write_payload_address] <= _zz_logic_ram_port;
    end
  end

  always @(posedge clk) begin
    if(logic_pop_sync_readPort_cmd_valid) begin
      logic_ram_spinal_port1 <= logic_ram[logic_pop_sync_readPort_cmd_payload];
    end
  end

  always @(*) begin
    _zz_1 = 1'b0;
    if(logic_push_onRam_write_valid) begin
      _zz_1 = 1'b1;
    end
  end

  assign when_Stream_l1383 = (logic_ptr_doPush != logic_ptr_doPop);
  assign logic_ptr_full = (((logic_ptr_push ^ logic_ptr_popOnIo) ^ 5'h10) == 5'h0);
  assign logic_ptr_empty = (logic_ptr_push == logic_ptr_pop);
  assign logic_ptr_occupancy = (logic_ptr_push - logic_ptr_popOnIo);
  assign io_push_ready = (! logic_ptr_full);
  assign io_push_fire = (io_push_valid && io_push_ready);
  assign logic_ptr_doPush = io_push_fire;
  assign logic_push_onRam_write_valid = io_push_fire;
  assign logic_push_onRam_write_payload_address = logic_ptr_push[3:0];
  assign logic_push_onRam_write_payload_data = io_push_payload;
  assign logic_pop_addressGen_valid = (! logic_ptr_empty);
  assign logic_pop_addressGen_payload = logic_ptr_pop[3:0];
  assign logic_pop_addressGen_fire = (logic_pop_addressGen_valid && logic_pop_addressGen_ready);
  assign logic_ptr_doPop = logic_pop_addressGen_fire;
  always @(*) begin
    logic_pop_addressGen_ready = logic_pop_sync_readArbitation_ready;
    if(when_Stream_l448) begin
      logic_pop_addressGen_ready = 1'b1;
    end
  end

  assign when_Stream_l448 = (! logic_pop_sync_readArbitation_valid);
  assign logic_pop_sync_readArbitation_valid = logic_pop_addressGen_rValid;
  assign logic_pop_sync_readArbitation_payload = logic_pop_addressGen_rData;
  assign logic_pop_sync_readPort_rsp = logic_ram_spinal_port1;
  assign logic_pop_addressGen_toFlowFire_valid = logic_pop_addressGen_fire;
  assign logic_pop_addressGen_toFlowFire_payload = logic_pop_addressGen_payload;
  assign logic_pop_sync_readPort_cmd_valid = logic_pop_addressGen_toFlowFire_valid;
  assign logic_pop_sync_readPort_cmd_payload = logic_pop_addressGen_toFlowFire_payload;
  assign logic_pop_sync_readArbitation_translated_valid = logic_pop_sync_readArbitation_valid;
  assign logic_pop_sync_readArbitation_ready = logic_pop_sync_readArbitation_translated_ready;
  assign logic_pop_sync_readArbitation_translated_payload = logic_pop_sync_readPort_rsp;
  assign io_pop_valid = logic_pop_sync_readArbitation_translated_valid;
  assign logic_pop_sync_readArbitation_translated_ready = io_pop_ready;
  assign io_pop_payload = logic_pop_sync_readArbitation_translated_payload;
  assign logic_pop_sync_readArbitation_fire = (logic_pop_sync_readArbitation_valid && logic_pop_sync_readArbitation_ready);
  assign logic_ptr_popOnIo = logic_pop_sync_popReg;
  assign io_occupancy = logic_ptr_occupancy;
  assign io_availability = (5'h10 - logic_ptr_occupancy);
  always @(posedge clk) begin
    if(reset) begin
      logic_ptr_push <= 5'h0;
      logic_ptr_pop <= 5'h0;
      logic_ptr_wentUp <= 1'b0;
      logic_pop_addressGen_rValid <= 1'b0;
      logic_pop_sync_popReg <= 5'h0;
    end else begin
      if(when_Stream_l1383) begin
        logic_ptr_wentUp <= logic_ptr_doPush;
      end
      if(io_flush) begin
        logic_ptr_wentUp <= 1'b0;
      end
      if(logic_ptr_doPush) begin
        logic_ptr_push <= (logic_ptr_push + 5'h01);
      end
      if(logic_ptr_doPop) begin
        logic_ptr_pop <= (logic_ptr_pop + 5'h01);
      end
      if(io_flush) begin
        logic_ptr_push <= 5'h0;
        logic_ptr_pop <= 5'h0;
      end
      if(logic_pop_addressGen_ready) begin
        logic_pop_addressGen_rValid <= logic_pop_addressGen_valid;
      end
      if(io_flush) begin
        logic_pop_addressGen_rValid <= 1'b0;
      end
      if(logic_pop_sync_readArbitation_fire) begin
        logic_pop_sync_popReg <= logic_ptr_pop;
      end
      if(io_flush) begin
        logic_pop_sync_popReg <= 5'h0;
      end
    end
  end

  always @(posedge clk) begin
    if(logic_pop_addressGen_ready) begin
      logic_pop_addressGen_rData <= logic_pop_addressGen_payload;
    end
  end


endmodule

module BmbUpSizerBridge (
  input  wire          io_input_cmd_valid,
  output wire          io_input_cmd_ready,
  input  wire          io_input_cmd_payload_last,
  input  wire [0:0]    io_input_cmd_payload_fragment_source,
  input  wire [0:0]    io_input_cmd_payload_fragment_opcode,
  input  wire [31:0]   io_input_cmd_payload_fragment_address,
  input  wire [2:0]    io_input_cmd_payload_fragment_length,
  input  wire [63:0]   io_input_cmd_payload_fragment_data,
  input  wire [7:0]    io_input_cmd_payload_fragment_mask,
  output wire          io_input_rsp_valid,
  input  wire          io_input_rsp_ready,
  output reg           io_input_rsp_payload_last,
  output wire [0:0]    io_input_rsp_payload_fragment_source,
  output wire [0:0]    io_input_rsp_payload_fragment_opcode,
  output wire [63:0]   io_input_rsp_payload_fragment_data,
  output wire          io_output_cmd_valid,
  input  wire          io_output_cmd_ready,
  output wire          io_output_cmd_payload_last,
  output wire [0:0]    io_output_cmd_payload_fragment_source,
  output wire [0:0]    io_output_cmd_payload_fragment_opcode,
  output wire [31:0]   io_output_cmd_payload_fragment_address,
  output wire [2:0]    io_output_cmd_payload_fragment_length,
  output reg  [127:0]  io_output_cmd_payload_fragment_data,
  output reg  [15:0]   io_output_cmd_payload_fragment_mask,
  output wire [1:0]    io_output_cmd_payload_fragment_context,
  input  wire          io_output_rsp_valid,
  output wire          io_output_rsp_ready,
  input  wire          io_output_rsp_payload_last,
  input  wire [0:0]    io_output_rsp_payload_fragment_source,
  input  wire [0:0]    io_output_rsp_payload_fragment_opcode,
  input  wire [127:0]  io_output_rsp_payload_fragment_data,
  input  wire [1:0]    io_output_rsp_payload_fragment_context,
  input  wire          clk,
  input  wire          reset
);

  reg        [63:0]   _zz_io_input_rsp_payload_fragment_data;
  wire       [0:0]    cmdArea_selStart;
  wire       [0:0]    cmdArea_context_selStart;
  reg        [0:0]    cmdArea_context_selEnd;
  wire                when_BmbUpSizerBridge_l53;
  reg        [63:0]   cmdArea_writeLogic_dataRegs_0;
  reg        [7:0]    cmdArea_writeLogic_maskRegs_0;
  reg        [0:0]    cmdArea_writeLogic_selReg;
  wire                io_input_cmd_fire;
  reg                 io_input_cmd_payload_first;
  wire       [0:0]    cmdArea_writeLogic_sel;
  wire       [63:0]   cmdArea_writeLogic_outputData_0;
  wire       [63:0]   cmdArea_writeLogic_outputData_1;
  wire       [7:0]    cmdArea_writeLogic_outputMask_0;
  wire       [7:0]    cmdArea_writeLogic_outputMask_1;
  wire                when_BmbUpSizerBridge_l85;
  wire                when_BmbUpSizerBridge_l95;
  wire                io_output_cmd_fire;
  wire                io_output_cmd_isStall;
  wire       [0:0]    rspArea_context_selStart;
  wire       [0:0]    rspArea_context_selEnd;
  wire       [1:0]    _zz_rspArea_context_selStart;
  reg        [0:0]    rspArea_readLogic_selReg;
  wire                io_input_rsp_fire;
  reg                 io_input_rsp_payload_first;
  wire       [0:0]    rspArea_readLogic_sel;
  wire                when_BmbUpSizerBridge_l133;

  always @(*) begin
    case(rspArea_readLogic_sel)
      1'b0 : _zz_io_input_rsp_payload_fragment_data = io_output_rsp_payload_fragment_data[63 : 0];
      default : _zz_io_input_rsp_payload_fragment_data = io_output_rsp_payload_fragment_data[127 : 64];
    endcase
  end

  assign cmdArea_selStart = io_input_cmd_payload_fragment_address[3 : 3];
  assign cmdArea_context_selStart = cmdArea_selStart;
  always @(*) begin
    cmdArea_context_selEnd = (io_input_cmd_payload_fragment_address[3 : 3] + 1'b0);
    if(when_BmbUpSizerBridge_l53) begin
      cmdArea_context_selEnd = io_input_cmd_payload_fragment_address[3 : 3];
    end
  end

  assign when_BmbUpSizerBridge_l53 = (io_input_cmd_payload_fragment_opcode == 1'b1);
  assign io_output_cmd_payload_last = io_input_cmd_payload_last;
  assign io_output_cmd_payload_fragment_opcode = io_input_cmd_payload_fragment_opcode;
  assign io_output_cmd_payload_fragment_address = io_input_cmd_payload_fragment_address;
  assign io_output_cmd_payload_fragment_length = io_input_cmd_payload_fragment_length;
  assign io_output_cmd_payload_fragment_source = io_input_cmd_payload_fragment_source;
  assign io_output_cmd_payload_fragment_context = {cmdArea_context_selEnd,cmdArea_context_selStart};
  assign io_input_cmd_fire = (io_input_cmd_valid && io_input_cmd_ready);
  assign cmdArea_writeLogic_sel = (io_input_cmd_payload_first ? cmdArea_selStart : cmdArea_writeLogic_selReg);
  assign cmdArea_writeLogic_outputData_0 = io_output_cmd_payload_fragment_data[63 : 0];
  assign cmdArea_writeLogic_outputData_1 = io_output_cmd_payload_fragment_data[127 : 64];
  assign cmdArea_writeLogic_outputMask_0 = io_output_cmd_payload_fragment_mask[7 : 0];
  assign cmdArea_writeLogic_outputMask_1 = io_output_cmd_payload_fragment_mask[15 : 8];
  always @(*) begin
    io_output_cmd_payload_fragment_data[63 : 0] = io_input_cmd_payload_fragment_data;
    if(when_BmbUpSizerBridge_l85) begin
      io_output_cmd_payload_fragment_data[63 : 0] = cmdArea_writeLogic_dataRegs_0;
    end
    io_output_cmd_payload_fragment_data[127 : 64] = io_input_cmd_payload_fragment_data;
  end

  assign when_BmbUpSizerBridge_l85 = ((! io_input_cmd_payload_first) && (cmdArea_writeLogic_selReg != 1'b0));
  always @(*) begin
    io_output_cmd_payload_fragment_mask[7 : 0] = ((cmdArea_writeLogic_sel == 1'b0) ? io_input_cmd_payload_fragment_mask : cmdArea_writeLogic_maskRegs_0);
    io_output_cmd_payload_fragment_mask[15 : 8] = ((cmdArea_writeLogic_sel == 1'b1) ? io_input_cmd_payload_fragment_mask : 8'h0);
  end

  assign when_BmbUpSizerBridge_l95 = (io_input_cmd_valid && (cmdArea_writeLogic_sel == 1'b0));
  assign io_output_cmd_fire = (io_output_cmd_valid && io_output_cmd_ready);
  assign io_output_cmd_valid = (io_input_cmd_valid && ((cmdArea_writeLogic_sel == 1'b1) || io_input_cmd_payload_last));
  assign io_output_cmd_isStall = (io_output_cmd_valid && (! io_output_cmd_ready));
  assign io_input_cmd_ready = (! io_output_cmd_isStall);
  assign _zz_rspArea_context_selStart = io_output_rsp_payload_fragment_context;
  assign rspArea_context_selStart = _zz_rspArea_context_selStart[0 : 0];
  assign rspArea_context_selEnd = _zz_rspArea_context_selStart[1 : 1];
  assign io_input_rsp_valid = io_output_rsp_valid;
  assign io_input_rsp_payload_fragment_opcode = io_output_rsp_payload_fragment_opcode;
  assign io_input_rsp_payload_fragment_source = io_output_rsp_payload_fragment_source;
  assign io_input_rsp_fire = (io_input_rsp_valid && io_input_rsp_ready);
  assign rspArea_readLogic_sel = (io_input_rsp_payload_first ? rspArea_context_selStart : rspArea_readLogic_selReg);
  always @(*) begin
    io_input_rsp_payload_last = (io_output_rsp_payload_last && (rspArea_readLogic_sel == rspArea_context_selEnd));
    if(when_BmbUpSizerBridge_l133) begin
      io_input_rsp_payload_last = 1'b0;
    end
  end

  assign io_output_rsp_ready = (io_input_rsp_ready && (io_input_rsp_payload_last || (rspArea_readLogic_sel == 1'b1)));
  assign when_BmbUpSizerBridge_l133 = (rspArea_context_selEnd != rspArea_readLogic_sel);
  assign io_input_rsp_payload_fragment_data = _zz_io_input_rsp_payload_fragment_data;
  always @(posedge clk) begin
    if(reset) begin
      cmdArea_writeLogic_maskRegs_0 <= 8'h0;
      io_input_cmd_payload_first <= 1'b1;
      io_input_rsp_payload_first <= 1'b1;
    end else begin
      if(io_input_cmd_fire) begin
        io_input_cmd_payload_first <= io_input_cmd_payload_last;
      end
      if(when_BmbUpSizerBridge_l95) begin
        cmdArea_writeLogic_maskRegs_0 <= io_input_cmd_payload_fragment_mask;
      end
      if(io_output_cmd_fire) begin
        cmdArea_writeLogic_maskRegs_0 <= 8'h0;
      end
      if(io_input_rsp_fire) begin
        io_input_rsp_payload_first <= io_input_rsp_payload_last;
      end
    end
  end

  always @(posedge clk) begin
    if(io_input_cmd_fire) begin
      cmdArea_writeLogic_selReg <= (cmdArea_writeLogic_sel + 1'b1);
    end
    if(!when_BmbUpSizerBridge_l85) begin
      cmdArea_writeLogic_dataRegs_0 <= io_input_cmd_payload_fragment_data;
    end
    rspArea_readLogic_selReg <= rspArea_readLogic_sel;
    if(io_input_rsp_fire) begin
      rspArea_readLogic_selReg <= (rspArea_readLogic_sel + 1'b1);
    end
  end


endmodule

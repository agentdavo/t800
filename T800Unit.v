// Generator : SpinalHDL dev    git head : ce00a2fa220be1185b897985e77e371bda6a1736
// Component : T800Unit
// Git hash  : 31c0360cabfdb5fd309ee65d17c14b2d12e7d479

`timescale 1ns/1ps

module T800Unit (
  input  wire          clk,
  input  wire          reset
);

  wire                core_systemBus_cmd_valid;
  wire                core_systemBus_cmd_payload_last;
  wire       [3:0]    core_systemBus_cmd_payload_fragment_source;
  wire       [0:0]    core_systemBus_cmd_payload_fragment_opcode;
  wire       [31:0]   core_systemBus_cmd_payload_fragment_address;
  wire       [3:0]    core_systemBus_cmd_payload_fragment_length;
  wire       [127:0]  core_systemBus_cmd_payload_fragment_data;
  wire       [15:0]   core_systemBus_cmd_payload_fragment_mask;
  wire                core_systemBus_rsp_ready;

  T800 core (
    .systemBus_cmd_valid                    (core_systemBus_cmd_valid                         ), //o
    .systemBus_cmd_ready                    (                                                 ), //i
    .systemBus_cmd_payload_last             (core_systemBus_cmd_payload_last                  ), //o
    .systemBus_cmd_payload_fragment_source  (core_systemBus_cmd_payload_fragment_source[3:0]  ), //o
    .systemBus_cmd_payload_fragment_opcode  (core_systemBus_cmd_payload_fragment_opcode       ), //o
    .systemBus_cmd_payload_fragment_address (core_systemBus_cmd_payload_fragment_address[31:0]), //o
    .systemBus_cmd_payload_fragment_length  (core_systemBus_cmd_payload_fragment_length[3:0]  ), //o
    .systemBus_cmd_payload_fragment_data    (core_systemBus_cmd_payload_fragment_data[127:0]  ), //o
    .systemBus_cmd_payload_fragment_mask    (core_systemBus_cmd_payload_fragment_mask[15:0]   ), //o
    .systemBus_rsp_valid                    (                                                 ), //i
    .systemBus_rsp_ready                    (core_systemBus_rsp_ready                         ), //o
    .systemBus_rsp_payload_last             (                                                 ), //i
    .systemBus_rsp_payload_fragment_source  (                                                 ), //i
    .systemBus_rsp_payload_fragment_opcode  (                                                 ), //i
    .systemBus_rsp_payload_fragment_data    (                                                 ), //i
    .clk                                    (clk                                              ), //i
    .reset                                  (reset                                            )  //i
  );

endmodule

module T800 (
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


  always @(posedge clk or posedge reset) begin
    if(reset) begin
    end else begin
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L569
        `else
          if(!1'b0) begin
            $display("NOTE Initializing PipelinePlugin v0.2"); // core.scala:L569
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L569
        `else
          if(!1'b0) begin
            $display("NOTE Initializing PipelineBuilderPlugin v0.5"); // core.scala:L569
          end
        `endif
      `endif
    end
  end


endmodule

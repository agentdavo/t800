// Generator : SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
// Component : T9000_FPGA
// Git hash  : b948a2fbad2daff59189098e1675e03fea3ed5e4

`timescale 1ns/1ps

module T9000_FPGA (
  input  wire          io_clk,
  input  wire          io_rst,
  output wire [7:0]    io_led,
  output wire          io_uart_tx,
  input  wire          io_uart_rx,
  output wire [3:0]    io_link_out,
  input  wire [3:0]    io_link_in,
  output wire [31:0]   io_mem_addr,
  output wire [31:0]   io_mem_data_out,
  input  wire [31:0]   io_mem_data_in,
  output wire          io_mem_we,
  output wire          io_mem_oe,
  output wire          io_mem_ce,
  output wire          io_running,
  output wire          io_error,
  output wire          io_halt
);

  reg        [31:0]   t9000Area_counter;

  assign io_led = t9000Area_counter[31 : 24];
  assign io_running = 1'b1;
  assign io_error = 1'b0;
  assign io_halt = 1'b0;
  assign io_uart_tx = (! io_uart_rx);
  assign io_link_out = (~ io_link_in);
  assign io_mem_addr = t9000Area_counter;
  assign io_mem_data_out = t9000Area_counter;
  assign io_mem_we = 1'b0;
  assign io_mem_oe = 1'b0;
  assign io_mem_ce = 1'b0;
  always @(posedge io_clk) begin
    if(io_rst) begin
      t9000Area_counter <= 32'h0;
    end else begin
      t9000Area_counter <= (t9000Area_counter + 32'h00000001);
    end
  end


endmodule

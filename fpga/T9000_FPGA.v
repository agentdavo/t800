// Basic LED Demo for ECP5 FPGA
// Simple counter driving LEDs to verify synthesis flow

module T9000_FPGA (
    input  wire        io_clk,
    input  wire        io_rst,
    output wire [7:0]  io_led,
    output wire        io_uart_tx,
    input  wire        io_uart_rx,
    output wire        io_running,
    output wire        io_error,
    output wire        io_link0_out,
    input  wire        io_link0_in
);

    // 24-bit counter for LED blinking
    reg [23:0] counter;
    
    always @(posedge io_clk or posedge io_rst) begin
        if (io_rst) begin
            counter <= 24'h000000;
        end else begin
            counter <= counter + 1'b1;
        end
    end
    
    // LED pattern - shows system is alive
    assign io_led = counter[23:16];
    
    // Status outputs
    assign io_running = 1'b1;
    assign io_error = 1'b0;
    
    // UART stub
    assign io_uart_tx = 1'b1;
    
    // Link stub  
    assign io_link0_out = 1'b0;

endmodule
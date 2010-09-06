// ----------------------------------------------------------------------------
// Memory arbitrer for VGA
// ----------------------------------------------------------------------------
module mem_arbitrer (
    input clk_i,					// VGA Clock
    input rst_i,					// Main system reset line

    input  [17:1] wb_adr_i,		    // Wishbone slave Interface:
    input  [ 1:0] wb_sel_i,			// These lines com from VGA Memory
    input         wb_we_i,			// Control interface
    input  [15:0] wb_dat_i,
    output [15:0] wb_dat_o,
    input         wb_stb_i,
    output reg    wb_ack_o,

    input  [17:1] csr_adr_i,		// CSR slave Input:
    output [15:0] csr_dat_o,		// From Video frame reads
    input         csr_stb_i,

    output [17:1] wbm_adr_o,		// Wishbone master Interface
    output        wbm_stb_o,		// These lines go to the 
	output        wbm_cyc_o,		// Wishbone RAM for video
    output [ 1:0] wbm_sel_o,
    output        wbm_we_o,
    output [15:0] wbm_dat_o,
    input  [15:0] wbm_dat_i,
    input  		  wb_ack_i
  );

  assign wbm_adr_o = csr_stb_i ? csr_adr_i : wb_adr_i;
  assign wbm_stb_o = csr_stb_i ? csr_stb_i : wb_stb_i;
  assign wbm_cyc_o = 1'b1;
  
  assign wbm_sel_o = csr_stb_i ? 2'b11     : wb_sel_i;
  assign wbm_we_o  = wb_stb_i & !csr_stb_i & wb_we_i;
  assign wbm_dat_o = wb_dat_i;

  assign wb_dat_o  = wbm_dat_i;
  assign csr_dat_o = wbm_dat_i;

/*
  reg  [1:0] wb_ack;
  wire       wb_ack_r = wb_ack[1];
  wire   	 wb_ack_w = wb_stb_i & !csr_stb_i;
  always @(posedge clk_i)
    wb_ack <= rst_i ? 2'b00 : { wb_ack[0], (wb_stb_i & !csr_stb_i & !(|wb_ack)) };

  assign 	 wb_ack_o = wb_we_i ? wb_ack_w : wb_ack_r;
*/
  
/*
  reg  [1:0] wb_ack;
  wire       wb_ack_r = wb_ack[1];
  wire   	 wb_ack_w = wb_stb_i & !csr_stb_i;
  assign 	 wb_ack_o = wb_we_i ? wb_ack_w : wb_ack_r;
  always @(posedge clk_i)
    wb_ack <= rst_i ? 2'b00 : { wb_ack[0], (wb_ack_i & !csr_stb_i & !(|wb_ack)) };
*/


//assign 	 wb_ack_o = wb_ack_i;
  always @(posedge clk_i) wb_ack_o  <= rst_i ? 1'b0 : wb_ack_i; 

// ----------------------------------------------------------------------------
endmodule
// ----------------------------------------------------------------------------

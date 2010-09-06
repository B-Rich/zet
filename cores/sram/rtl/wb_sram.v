// ----------------------------------------------------------------------------
// WB_SRAM  
// ----------------------------------------------------------------------------
module wb_sram (
    input				wb_clk_i,	    // Wishbone slave interface
    input  				wb_rst_i,
    input  		[15:0]	wb_dat_i,
    output reg 	[15:0]	wb_dat_o,
    input  		[17:1]	wb_adr_i,
    input       		wb_we_i,
    input  		[ 1:0]	wb_sel_i,
    input				wb_stb_i,
    input				wb_cyc_i,
    output reg			wb_ack_o,

    output		[17:0]	sram_addr_,	    // SRAM Chip Pad signals
    inout  		[15:0]	sram_data_,
    output reg			sram_we_n_,
    output reg			sram_oe_n_,
    output				sram_ce_n_,
    output reg	[ 1:0]	sram_bw_n_
  );

  reg 	[16:0] 	sram_addr;
  reg	[15:0] 	ww;
  wire 			op 		   = wb_stb_i & wb_cyc_i;
  wire          we         = ~(wb_we_i & op);

  assign 		sram_addr_ = { 1'b0, sram_addr };
  assign 		sram_data_ = sram_we_n_ ? 16'hzzzz : ww;
  assign 		sram_ce_n_ = 1'b0;

  always @(posedge wb_clk_i) ww         <=  wb_dat_i;
  always @(posedge wb_clk_i) sram_addr  <=  wb_adr_i;
  always @(posedge wb_clk_i) sram_we_n_ <=  we;
  always @(posedge wb_clk_i) sram_bw_n_ <= ~wb_sel_i;
  always @(posedge wb_clk_i) sram_oe_n_ <= ~we;
  always @(posedge wb_clk_i) wb_dat_o   <=  sram_data_;
  always @(posedge wb_clk_i) wb_ack_o   <=  wb_rst_i ? 1'b0 : (op & ~wb_ack_o);

// ----------------------------------------------------------------------------
endmodule
// ----------------------------------------------------------------------------

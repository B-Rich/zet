/*
 *  CSR SRAM Module
 */
module csr_sram (
    input sys_clk,

    // CSR slave interface
    input      [17:1] csr_adr_i,
    input      [ 1:0] csr_sel_i,
    input             csr_we_i,
    input      [15:0] csr_dat_i,
    output reg [15:0] csr_dat_o,

    // Pad signals
    output     [17:0] sram_addr_,
    inout      [15:0] sram_data_,
    output reg        sram_we_n_,
    output reg        sram_oe_n_,
    output            sram_ce_n_,
    output reg [ 1:0] sram_bw_n_
  );

  // Registers and nets
  reg [15:0] ww;
  reg [16:0] sram_addr;

  // Continuous assingments
  assign sram_data_ = sram_we_n_ ? 16'hzzzz : ww;
  assign sram_ce_n_ = 1'b0;
  assign sram_addr_ = { 1'b0, sram_addr };

  // Behaviour
  // ww
  always @(posedge sys_clk) ww <= csr_dat_i;

  // sram_addr
  always @(posedge sys_clk) sram_addr <= csr_adr_i;

  // sram_we_n_
  always @(posedge sys_clk) sram_we_n_ <= !csr_we_i;

  // sram_bw_n_
  always @(posedge sys_clk) sram_bw_n_ <= ~csr_sel_i;

  // sram_oe_n_
  always @(posedge sys_clk) sram_oe_n_ <= csr_we_i;

  // csr_dat_o
  always @(posedge sys_clk) csr_dat_o <= sram_data_;
endmodule

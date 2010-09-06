/*
 *  Static RAM Memory for VGA
 */
module mem_sram (
    input wb_clk_i,
    input wb_rst_i,

    // Wishbone slave 1
    input  [17:1] wb_adr_i,
    input  [ 1:0] wb_sel_i,
    input         wb_we_i,
    input  [15:0] wb_dat_i,
    output [15:0] wb_dat_o,
    input         wb_stb_i,
    output        wb_ack_o,

    // SRAM pad signals
    output [17:0] sram_addr_,
    inout  [15:0] sram_data_,
    output        sram_we_n_,
    output        sram_oe_n_,
    output        sram_ce_n_,
    output [ 1:0] sram_bw_n_
	
  );
  
    // CSR slave 1
    wire   [17:1] csr_adr_i,
    wire   [15:0] csr_dat_o,
    wire          csr_stb_i,

    // CSR master
    wire  [17:1] csrm_adr_o,
    wire  [ 1:0] csrm_sel_o,
    wire         csrm_we_o,
    wire  [15:0] csrm_dat_o,
    wire  [15:0] csrm_dat_i

	
  mem_arbitrer arbitrer (
    .clk_i (wb_clk_i),
    .rst_i (wb_rst_i),

    .wb_adr_i (wb_adr_o),
    .wb_sel_i (wb_sel_o),
    .wb_we_i  (wb_we_o),
    .wb_dat_i (wb_dat_o),
    .wb_dat_o (wb_dat_i),
    .wb_stb_i (wb_stb_o),
    .wb_ack_o (wb_ack_i),

    .csr_adr_i (csr_adr_i),
    .csr_dat_o (csr_dat_i),
    .csr_stb_i (csr_stb_i),

    .csrm_adr_o (csrm_adr_o),
    .csrm_sel_o (csrm_sel_o),
    .csrm_we_o  (csrm_we_o),
    .csrm_dat_o (csrm_dat_o),
    .csrm_dat_i (csrm_dat_i)
  );

  csr_sram sram (
    .sys_clk (wb_clk_i),

    .csr_adr_i (csrm_adr_o),
    .csr_sel_i (csrm_sel_o),
    .csr_we_i  (csrm_we_o),
    .csr_dat_i (csrm_dat_o),
    .csr_dat_o (csrm_dat_i),

    .sram_addr_ (sram_addr_),
    .sram_data_ (sram_data_),
    .sram_we_n_ (sram_we_n_),
    .sram_oe_n_ (sram_oe_n_),
    .sram_ce_n_ (sram_ce_n_),
    .sram_bw_n_ (sram_bw_n_)
  );

endmodule

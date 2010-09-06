// --------------------------------------------------------------------
// --------------------------------------------------------------------
// Module:      BIOSROM.v
// Description: Wishbone Compatible BIOS ROM core using megafunction ROM
// The following is to get rid of the warning about not initializing the ROM
// altera message_off 10030
// --------------------------------------------------------------------
// --------------------------------------------------------------------
module BIOSROM(
    input             wb_clk_i,          // Wishbone slave interface
    input             wb_rst_i,
    input      [15:0] wb_dat_i,
    output     [15:0] wb_dat_o,
    input      [19:1] wb_adr_i,
    input             wb_we_i,
    input             wb_tga_i,
    input             wb_stb_i,
    input             wb_cyc_i,
    input      [ 1:0] wb_sel_i,
    output reg        wb_ack_o,
    
    output     [21:0] flash_addr_,		// Pad signals
    input      [15:0] flash_data_,
    output            flash_we_n_,
    output            flash_oe_n_,
    output            flash_ce_n_,
    output            flash_rst_n_    
);

wire ack_o = wb_stb_i & wb_cyc_i;
always @(posedge wb_clk_i) wb_ack_o <= ack_o;

reg  [15:0] rom[0:127]; 	// Instantiate the ROM
initial $readmemh("biosrom.dat", rom);

wire   [ 6:0] rom_addr = wb_adr_i[7:1];
wire   [15:0] rom_dat  = rom[rom_addr];
assign        wb_dat_o = rom_dat;


/*  For Testing 
wire ack_o = wb_stb_i & wb_cyc_i;
always @(posedge wb_clk_i) wb_ack_o <= ack_o;

reg  [15:0] rom[0:127]; 	// Instantiate the ROM
initial $readmemh("biosrom.dat", rom);

wire   [ 6:0] rom_addr = wb_adr_i[7:1];
wire   [15:0] rom_dat  = rom[rom_addr];

wire [19:0] real_addr = {wb_adr_i, 1'b0};
wire        test_rom  = (real_addr < 20'hF_FF00);
assign      wb_dat_o  = test_rom ? flash_dat : rom_dat;

wire   [15:0] flash_dat    = flash_data_;
assign 		  flash_rst_n_ = 1'b1;
assign 		  flash_we_n_  = 1'b1;
assign 		  flash_oe_n_  = !ack_o;
assign 		  flash_ce_n_  = !ack_o;
assign 		  flash_addr_  = {5'h0, wb_adr_i[16:1]};
*/

// --------------------------------------------------------------------
endmodule
// --------------------------------------------------------------------
  

/*
 *  This file is part of the Zet processor. This processor is free
 *  hardware; you can redistribute it and/or modify it under the terms of
 *  the GNU General Public License as published by the Free Software
 *  Foundation; either version 3, or (at your option) any later version.
 *
 *  Zet is distrubuted in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Zet; see the file COPYING. If not, see
 *  <http://www.gnu.org/licenses/>.
 */

module lcd (
    input clk,              // 25 Mhz clock
    input rst,

    input shift_reg1,       // if set: 320x200
    input graphics_alpha,   // if not set: 640x400 text mode

    // CSR slave interface for reading
    output [17:1] csr_adr_o,
    input  [15:0] csr_dat_i,
    output        csr_stb_o,

    // attribute_ctrl
    input  [3:0] pal_addr,
    input        pal_we,
    output [7:0] pal_read,
    input  [7:0] pal_write,

    // dac_regs
    input        dac_we,
    input  [1:0] dac_read_data_cycle,
    input  [7:0] dac_read_data_register,
    output [7:0] dac_read_data,
    input  [1:0] dac_write_data_cycle,
    input  [7:0] dac_write_data_register,
    input  [7:0] dac_write_data,

    // VGA pad signals
    output reg [9:0] vga_red_o,
    output reg [9:0] vga_green_o,
    output reg [9:0] vga_blue_o,
/*    
    output reg [3:0] vga_red_o,
    output reg [3:0] vga_green_o,
    output reg [3:0] vga_blue_o,
*/    
    
    output reg       horiz_sync,
    output reg       vert_sync,

    // CRTC
    input [5:0] cur_start,
    input [5:0] cur_end,
    input [4:0] vcursor,
    input [6:0] hcursor,

    // retrace signals
    output v_retrace,
    output vh_retrace
  );

  // Synchronization constants, these values are taken from:
  //  http://tinyvga.com/vga-timing/640x400@70Hz
  localparam HOR_DISP_END = 10'd639; // Last horizontal pixel displayed
  localparam HOR_SYNC_BEG = 10'd655; // Start of horizontal synch pulse
  localparam HOR_SYNC_END = 10'd751; // End of Horizontal Synch pulse
  localparam HOR_SCAN_END = 10'd799; // Last pixel in scan line

  localparam VER_DISP_END_0 = 10'd400;  // last row displayed
  localparam VER_SYNC_BEG_0 = 10'd412;  // start of vertical synch pulse
  localparam VER_SYNC_END_0 = 10'd415;  // end of vertical synch pulse
  localparam VER_SCAN_END_0 = 10'd448;  // Last scan row in the frame
  localparam VER_DISP_END_1 = 10'd480;  // last row displayed
  localparam VER_SYNC_BEG_1 = 10'd490;  // start of vertical synch pulse
  localparam VER_SYNC_END_1 = 10'd492;  // end of vertical synch pulse
  localparam VER_SCAN_END_1 = 10'd524;  // Last scan row in the frame

  // Registers and nets
  reg        video_on_v;
  reg        video_on_h_i;
  reg [1:0]  video_on_h_p;
  reg [9:0]  h_count;   // Horizontal pipeline delay is 2 cycles
  reg [9:0]  v_count;   // 0 to VER_SCAN_END

  wire       mode640x480;
  wire [9:0] ver_disp_end;
  wire [9:0] ver_sync_beg;
  wire [9:0] ver_sync_end;
  wire [9:0] ver_scan_end;
  wire       video_on;

  wire [3:0] attr_gm;
  wire [3:0] attr_wm;
  wire [3:0] attr_tm;
  wire [3:0] attr;
  wire [7:0] index;

  wire video_on_h_tm;
  wire video_on_h_wm;
  wire video_on_h_gm;
  wire video_on_h;

  reg       horiz_sync_i;
  reg [1:0] horiz_sync_p;
  wire      horiz_sync_tm;
  wire      horiz_sync_wm;
  wire      horiz_sync_gm;

  wire [16:1] csr_tm_adr_o;
  wire        csr_tm_stb_o;
  wire [17:1] csr_wm_adr_o;
  wire        csr_wm_stb_o;
  wire        csr_stb_o_tmp;

  wire [7:0] red;
  wire [7:0] green;
  wire [7:0] blue;

  // Module instances
  text_mode tm (
    .clk (clk),
    .rst (rst),

    // CSR slave interface for reading
    .csr_adr_o (csr_tm_adr_o),
    .csr_dat_i (csr_dat_i),
    .csr_stb_o (csr_tm_stb_o),

    .h_count      (h_count),
    .v_count      (v_count),
    .horiz_sync_i (horiz_sync_i),
    .video_on_h_i (video_on_h_i),
    .video_on_h_o (video_on_h_tm),

    .cur_start  (cur_start),
    .cur_end    (cur_end),
    .vcursor    (vcursor),
    .hcursor    (hcursor),

    .attr         (attr_tm),
    .horiz_sync_o (horiz_sync_tm)
  );

  planar_640x480 wm (
    .clk (clk),
    .rst (rst),

    // CSR slave interface for reading
    .csr_adr_o (csr_wm_adr_o),
    .csr_dat_i (csr_dat_i),
    .csr_stb_o (csr_wm_stb_o),

    .attr_plane_enable (4'hf),

    .h_count      (h_count),
    .v_count      (v_count),
    .horiz_sync_i (horiz_sync_i),
    .video_on_h_i (video_on_h_i),
    .video_on_h_o (video_on_h_wm),

    .attr         (attr_wm),
    .horiz_sync_o (horiz_sync_wm)
  );

  palette_regs pr (
    .clk (clk),

    .attr  (attr),
    .index (index),

    .address    (pal_addr),
    .write      (pal_we),
    .read_data  (pal_read),
    .write_data (pal_write)
  );

  dac_regs dr (
    .clk (clk),

    .index (index),
    .red   (red),
    .green (green),
    .blue  (blue),

    .write (dac_we),

    .read_data_cycle    (dac_read_data_cycle),
    .read_data_register (dac_read_data_register),
    .read_data          (dac_read_data),

    .write_data_cycle    (dac_write_data_cycle),
    .write_data_register (dac_write_data_register),
    .write_data          (dac_write_data)
  );

  // Continuous assignments
  assign mode640x480  = graphics_alpha & !shift_reg1;
  assign ver_disp_end = mode640x480 ? VER_DISP_END_1 : VER_DISP_END_0;
  assign ver_sync_beg = mode640x480 ? VER_SYNC_BEG_1 : VER_SYNC_BEG_0;
  assign ver_sync_end = mode640x480 ? VER_SYNC_END_1 : VER_SYNC_END_0;
  assign ver_scan_end = mode640x480 ? VER_SCAN_END_1 : VER_SCAN_END_0;
  assign video_on     = video_on_h && video_on_v;

  assign attr_gm = 4'h0;

  assign attr = graphics_alpha ?
    (shift_reg1 ? attr_gm : attr_wm) : attr_tm;

  assign video_on_h_gm = video_on_h_i;
  assign video_on_h    = video_on_h_p[1];

  assign horiz_sync_gm = horiz_sync_i;

  assign csr_adr_o = graphics_alpha ? csr_wm_adr_o : { 1'b0, csr_tm_adr_o };

  assign csr_stb_o_tmp = graphics_alpha ? csr_wm_stb_o : csr_tm_stb_o;
  assign csr_stb_o     = csr_stb_o_tmp & (video_on_h_i | video_on_h) & video_on_v;

  assign v_retrace   = !video_on_v;
  assign vh_retrace  = v_retrace | !video_on_h;

  // Sync generation & timing process
  // Generate horizontal and vertical timing signals for video signal
  always @(posedge clk)
    if (rst)
      begin
        h_count      <= 10'b0;
        horiz_sync_i <= 1'b1;
        v_count      <= 10'b0;
        vert_sync    <= 1'b1;
        video_on_h_i <= 1'b1;
        video_on_v   <= 1'b1;
      end
    else
      begin
        h_count      <= (h_count==HOR_SCAN_END) ? 10'b0 : h_count + 10'b1;
        horiz_sync_i <= (h_count==HOR_SYNC_BEG) ? 1'b0
                      : ((h_count==HOR_SYNC_END) ? 1'b1 : horiz_sync_i);
        v_count      <= (v_count==ver_scan_end && h_count==HOR_SCAN_END) ? 10'b0
                      : ((h_count==HOR_SCAN_END) ? v_count + 10'b1 : v_count);
        vert_sync    <= (v_count==ver_sync_beg) ? 1'b0
                      : ((v_count==ver_sync_end) ? 1'b1 : vert_sync);

        video_on_h_i <= (h_count==HOR_SCAN_END) ? 1'b1
                      : ((h_count==HOR_DISP_END) ? 1'b0 : video_on_h_i);
        video_on_v   <= (v_count==10'h0) ? 1'b1
                      : ((v_count==ver_disp_end) ? 1'b0 : video_on_v);
      end

  // Horiz sync
  always @(posedge clk)
    { horiz_sync, horiz_sync_p } <= rst ? 3'b0
       : { horiz_sync_p[1:0], graphics_alpha ?
         (shift_reg1 ? horiz_sync_gm : horiz_sync_wm) : horiz_sync_tm };

  // Video_on pipe
  always @(posedge clk)
    video_on_h_p <= rst ? 2'b0 : { video_on_h_p[0],
      graphics_alpha ? (shift_reg1 ? video_on_h_gm : video_on_h_wm)
                     : video_on_h_tm };

  // Colour signals
  always @(posedge clk)
    if (rst)
      begin
        vga_red_o     <= 10'b0;
        vga_green_o   <= 10'b0;
        vga_blue_o    <= 10'b0;
      end
    else
      begin
        vga_blue_o  <= video_on ? {blue ,2'b0} : 10'h0;
        vga_green_o <= video_on ? {green,2'b0} : 10'h0;
        vga_red_o   <= video_on ? {red  ,2'b0} : 10'h0;
      end
      
/*      
  // Colour signals
  always @(posedge clk)
    if (rst)
      begin
        vga_red_o     <= 4'b0;
        vga_green_o   <= 4'b0;
        vga_blue_o    <= 4'b0;
      end
    else
      begin
        vga_blue_o  <= video_on ? blue[5:2] : 4'h0;
        vga_green_o <= video_on ? green[5:2] : 4'h0;
        vga_red_o   <= video_on ? red[5:2] : 4'h0;
      end
*/
endmodule

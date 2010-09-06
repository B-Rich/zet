/*
 *  8254 timer simplified for Zet PC:
 *   - Wishbone interface
 *   - Modes (binary) 2 and 3 only
 *   - Common clock for all 3 Timers (tclk_i)
 *   - gate input for Timer2 only (gate2_i)
 *  Assumptions:
 *   1. tclk_i is asyncronous simple wire (1.193182 MHz by default)
 *   2. gate2_i is syncronous (comes from Wishbone controlled register)
 *   3. Wishbone clock wb_clk_i is running always and it has much higher 
 *      frequency than tclk_i
 *
 *  Copyright (c) 2010  YS
 *  Copyright (c) 2009  Zeus Gomez Marmolejo &lt;zeus@opencores.org&gt;
 *
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
 *  &lt;http://www.gnu.org/licenses/&gt;.
 */

`define WB_UNBUFFERED_8254

//module 8254pc_timer
module timer
  (
    // Wishbone slave interface
    input             wb_clk_i,
    input             wb_rst_i,
    input             wb_adr_i,
    input      [1:0]  wb_sel_i,
    input      [15:0] wb_dat_i,
    output reg [15:0] wb_dat_o,
    input             wb_stb_i,
    input             wb_cyc_i,
    input             wb_we_i,
    output            wb_ack_o,
    output reg        wb_tgc_o,   // intr

    // CLK 
    input             tclk_i,     // 1.193182 MHz = (14.31818/12) MHz
    // SPEAKER
    input             gate2_i,
    output            out2_o
  );

`ifdef WB_UNBUFFERED_8254
wire [15:0] data_ib;
wire wr_cyc1;
wire rd_cyc1;
wire [1:0] datasel;
`else
reg [15:0] data_ib;
reg wr_cyc1;
reg rd_cyc1, rd_cyc2;
reg [1:0] datasel;
`endif

wire intr, refresh;
reg intr1;
//reg [7:0] dat_o;

wire wrc, wrd0, wrd1, wrd2, rdd0, rdd1, rdd2;
wire [7:0] data0;
wire [7:0] data1;
wire [7:0] data2;

// Making 1 clock pulse on wb_tgc_o from intr
// unnecessary for real 8259A -&gt; subj to remove later
always @(posedge wb_clk_i)
begin
  intr1 &lt;= wb_rst_i ? 1'b0 : intr;
  wb_tgc_o &lt;= wb_rst_i ? 1'b0 : (!intr1 &amp; intr);
end

// 8-bit interface via wb_dat low byte (2-bit [2:1]??? wb_addr_i , no wb_sel_i)
/*
assign wb_ack_o = wb_stb_i &amp; wb_cyc_i;

assign wrc = wb_ack_o &amp; wb_we_i &amp; (wb_adr_i == 2'b11);

assign wrd0 = wb_ack_o &amp; wb_we_i &amp; (wb_adr_i == 2'b00);
assign wrd1 = wb_ack_o &amp; wb_we_i &amp; (wb_adr_i == 2'b01);
assign wrd2 = wb_ack_o &amp; wb_we_i &amp; (wb_adr_i == 2'b10);

assign rdd0 = wb_ack_o &amp; ~wb_we_i &amp; (wb_adr_i == 2'b00);
assign rdd1 = wb_ack_o &amp; ~wb_we_i &amp; (wb_adr_i == 2'b01);
assign rdd2 = wb_ack_o &amp; ~wb_we_i &amp; (wb_adr_i == 2'b10);

always @(wb_adr_i or data0 or data1 or data2)
  case (wb_adr_i)
    2'b00: wb_dat_o = { 8'h0, data0 };
    2'b01: wb_dat_o = { 8'h0, data1 };
    2'b10: wb_dat_o = { 8'h0, data2 };
  endcase

i8254pc_counter cnt0(0, 6'h36, 16'hFFFF, wb_clk_i, wb_rst_i, wrc, wrd0, rdd0, wb_dat_i, data0, tclk_i, 1'b1, intr);    // 16-bit 55 ms Mode 3
i8254pc_counter cnt1(1, 6'h14, 16'h0012, wb_clk_i, wb_rst_i, wrc, wrd1, rdd1, wb_dat_i, data1, tclk_i, 1'b1, refresh); // 8-bit  15 us Mode 2
i8254pc_counter cnt2(2, 6'h36, 16'h04A9, wb_clk_i, wb_rst_i, wrc, wrd2, rdd2, wb_dat_i, data2, tclk_i, gate2_i, out2_o);  // 16-bit  1 ms Mode 3
*/

// 16-bit interface via wb_dat both bytes (1-bit wb_addr_i, 2-bit [1:0] wb_sel_i)
// assumes opposite wb_sel_i only: 2'b10 or 2'b01

reg [7:0] data_i;
reg [15:0] data_ob;

always @(datasel or data0 or data1 or data2)
  case (datasel)
    2'b00: data_ob = { 8'h0, data0 };
    2'b01: data_ob = { data1, 8'h0 };
    2'b10: data_ob = { 8'h0, data2 };
    2'b11: data_ob = { 8'h0, 8'h0 }; // not checked yet!
  endcase

always @(datasel or data_ib)
  case (datasel)
    2'b00: data_i = data_ib[7:0];
    2'b01: data_i = data_ib[15:8];
    2'b10: data_i = data_ib[7:0];
    2'b11: data_i = data_ib[15:8];
  endcase

assign wrc = wr_cyc1 &amp; (datasel == 2'b11);

assign wrd0 = wr_cyc1 &amp; (datasel == 2'b00);
assign wrd1 = wr_cyc1 &amp; (datasel == 2'b01);
assign wrd2 = wr_cyc1 &amp; (datasel == 2'b10);

assign rdd0 = rd_cyc1 &amp; (datasel == 2'b00);
assign rdd1 = rd_cyc1 &amp; (datasel == 2'b01);
assign rdd2 = rd_cyc1 &amp; (datasel == 2'b10);

`ifdef WB_UNBUFFERED_8254
// 1 clock write, 1 clock read

assign wb_ack_o = wb_stb_i &amp; wb_cyc_i;

assign wr_cyc1 = wb_ack_o &amp; wb_we_i;
assign rd_cyc1 = wb_ack_o &amp; ~wb_we_i;
assign datasel = {wb_adr_i,wb_sel_i[1]};

//assign wb_dat_o = data_ob;
always @(data_ob)
  wb_dat_o = data_ob;
assign data_ib = wb_dat_i;

`else
// 2 clocks write, 3 clocks read

assign wb_ack_o = wr_cyc1 | rd_cyc2;

always @(posedge wb_clk_i)
begin
  wr_cyc1 &lt;= (wr_cyc1) ? 1'b0 : wb_stb_i &amp; wb_cyc_i &amp; wb_we_i;            // single clock write pulse
  rd_cyc1 &lt;= (rd_cyc1 | rd_cyc2) ? 1'b0 : wb_stb_i &amp; wb_cyc_i &amp; ~wb_we_i; // single clock read pulse
  rd_cyc2 &lt;= rd_cyc1;                                                     // delayed single clock read pulse
  datasel &lt;= {wb_adr_i,wb_sel_i[1]};

  wb_dat_o &lt;= data_ob;
  data_ib &lt;= wb_dat_i;
end

`endif //def WB_UNBUFFERED_8254

i8254pc_counter cnt0(0, 6'h36, 16'hFFFF, wb_clk_i, wb_rst_i, wrc, wrd0, rdd0, data_i, data0, tclk_i, 1'b1, intr);    // 16-bit 55 ms Mode 3
i8254pc_counter cnt1(1, 6'h14, 16'h0012, wb_clk_i, wb_rst_i, wrc, wrd1, rdd1, data_i, data1, tclk_i, 1'b1, refresh); // 8-bit  15 us Mode 2
i8254pc_counter cnt2(2, 6'h36, 16'h04A9, wb_clk_i, wb_rst_i, wrc, wrd2, rdd2, data_i, data2, tclk_i, gate2_i, out2_o);  // 16-bit  1 ms Mode 3

endmodule


module i8254pc_counter(
  input [1:0] cntnum,   // Counter Number constant 0/1/2
  input [5:0] cw0,      // Initial Control Word constant
  input [15:0] cr0,     // Initial Constant Register constant
  input clkrw,          // Read/Write System Clock
  input rst,            // Reset
  input wrc,            // Write Command 1 clock pulse
  input wrd,            // Write Data 1 clock pulse
  input rdd,            // Read Data full cycle strobe
  input [7:0] data_i,   // Input Data
  output reg [7:0] data_o,  // Output Data
  input clkt,           // Timer Clock (asynchronous to clkrw)
  input gate,           // Timer Gate (synchronous to clkrw)
  output out            // Timer Out (synchronous to clkrw)
);

localparam
        DATL = 2'd0,
        DATH = 2'd1,
        STAT = 2'd2;

reg [15:0] rCounter;          // Timer Counter
reg [15:0] rConstant;         // Constant Register
reg [5:0] rControl;           // Control Word Register
reg [15:0] rLatchD;           // Output Data Latch
reg [7:0] rLatchS;            // Output State Latch
reg bOut;                     
reg bFn;

reg clcd, clcs;               // Latch Data and Latch State command pulses

reg fWroteLow;
reg fWroteHigh;

reg fCount;
reg bCurrentClk;
reg bFilterClk1;
reg bFilterClk2;

reg fLatchData;
reg fLatchStat;

reg rdd1;
reg [1:0] outmux;
reg fToggleHigh;

wire fReadEnd;

wire [2:0] rbc_cnt_mask = data_i[3:1];

wire fMode3 = (rControl[2:1] == 2'b11);

wire fRWLow = rControl[4];
wire fRWHigh = rControl[5];

assign out = bOut;

// Write to Control Word Register
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    rControl &lt;= cw0;
    clcd &lt;= 1'b0;
    clcs &lt;= 1'b0;
  end
  else
  begin
    if (wrc &amp;&amp; data_i[7:6] == cntnum)
    begin
      if (data_i[5:4] == 2'b00) 
        clcd &lt;= 1'b1;            // CLC
      else
        rControl &lt;= data_i[5:0]; // WRC
    end
    else if (wrc &amp;&amp; data_i[7:6] == 2'b11 &amp;&amp; rbc_cnt_mask[cntnum])
    begin
      clcd &lt;= ~data_i[5];        // RBC
      clcs &lt;= ~data_i[4];
    end

    if (clcd)
      clcd &lt;= 1'b0;  // 1 clock pulse clcd

    if (clcs)
      clcs &lt;= 1'b0;  // 1 clock pulse clcs
  end
end

// Write to Constant Register
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    rConstant &lt;= cr0;
    fWroteLow &lt;= 1'b0;
    fWroteHigh &lt;= 1'b0;
  end
  else
  begin
    if (fWroteHigh || wrc)
    begin
      fWroteLow &lt;= 1'b0;
      fWroteHigh &lt;= 1'b0;
    end
    if (wrd) // need 1 clock pulse wrd!!!
    begin
      if (!fWroteLow)
      begin
        if (fRWLow)
          rConstant[7:0] &lt;= data_i[7:0];
        fWroteLow &lt;= 1'b1;
        if (!fRWHigh)
        begin
          rConstant[15:8] &lt;= 8'b00000000;
          fWroteHigh &lt;= 1'b1;
        end
      end
      if (!fWroteHigh &amp;&amp; (fWroteLow || !fRWLow))
      begin
        if (fRWHigh)
          rConstant[15:8] &lt;= data_i[7:0];
        fWroteHigh &lt;= 1'b1;
        if (!fRWLow)
        begin
          rConstant[7:0] &lt;= 8'b00000000;
          fWroteLow &lt;= 1'b1;
        end
      end
    end // if (wrd)
  end
end

// Synchronizing Count Clock with Wishbone Clock
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    fCount &lt;= 1'b0;
    bCurrentClk &lt;= 1'b0;
    bFilterClk1 &lt;= 1'b0;
    bFilterClk2 &lt;= 1'b0;
  end
  else
  begin
    bFilterClk1 &lt;= clkt;
    bFilterClk2 &lt;= bFilterClk1;
    if ((bFilterClk1 == bFilterClk2) &amp;&amp; (bCurrentClk != bFilterClk2))
    begin
      bCurrentClk &lt;= bFilterClk2;
      if (bCurrentClk == 1'b1) // falling edge of clkt
        fCount &lt;= 1'b1;
    end
    if (fCount)
      fCount &lt;= 1'b0; // 1 clock pulse fCount
  end
end  

// Timer Counter in mode 2 or mode 3
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    bOut &lt;= 1'b1;
    rCounter &lt;= cr0 &amp; ((cw0[2:1] == 2'b11) ? 16'hFFFE : 16'hFFFF); // (mode==3) ? :
    bFn &lt;= 1'b0;
  end
  else
  begin
    if (fWroteHigh)
    begin
      rCounter &lt;= rConstant &amp; ((fMode3) ? 16'hFFFE : 16'hFFFF);
      bOut &lt;= 1'b1;
    end
    else if (fCount &amp;&amp; gate) // tclk_i &amp;&amp; gate_i
    begin
      if ((fMode3) ? (bOut == 1'b0 &amp;&amp; rCounter == 16'h0002) : (bOut == 1'b0))
      begin
        rCounter &lt;= rConstant &amp; ((fMode3) ? 16'hFFFE : 16'hFFFF);
        bOut &lt;= 1'b1;
      end
      else if (fMode3 &amp;&amp; bOut == 1'b1 &amp;&amp; rCounter == ((rConstant[0]) ? 16'h0000 : 16'h0002))
      begin
        rCounter &lt;= rConstant &amp; 16'hFFFE;
        bOut &lt;= 1'b0;
      end
      else if (!fMode3 &amp;&amp; rCounter == 16'h0002)
        bOut &lt;= 1'b0;
      else
        rCounter &lt;= rCounter - ((fMode3) ? 16'h0002 : 16'h0001);
    end
  end
end

// Output Latch Control
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    fLatchData &lt;= 1'b0;
    fLatchStat &lt;= 1'b0;
    rLatchD &lt;= 16'b0;
    rLatchS &lt;= 8'b0;
  end
  else
  begin
    if (!fLatchData)
      rLatchD &lt;= rCounter;
    if (!fLatchStat)
      rLatchS &lt;= {bOut, bFn, rControl};
    if (clcd)
      fLatchData &lt;= 1'b1;
    if (clcs)
      fLatchStat &lt;= 1'b1;
    if (fReadEnd)
    begin
      if (fLatchStat)
        fLatchStat &lt;= 1'b0;
      else if (fLatchData)
        fLatchData &lt;= 1'b0;
    end
  end
end

// Output Mux
always @(outmux or rLatchS or rLatchD)
begin
  case (outmux)
    STAT: data_o = rLatchS;
    DATH: data_o = rLatchD[15:8];
    DATL: data_o = rLatchD[7:0];
  endcase
end

assign fReadEnd = !rdd &amp;&amp; rdd1; // 1 clock pulse after read

// Read Data/State
always @(posedge clkrw or posedge rst)
begin
  if (rst)
  begin
    rdd1 &lt;= 1'b0;
    outmux &lt;= DATL;
    fToggleHigh &lt;= 1'b0;
  end
  else
  begin
    // Helper for fReadEnd
    rdd1 &lt;= rdd;

    // Output Mux Control
    if (fLatchStat)
      outmux &lt;= STAT;
    else if ((fRWHigh &amp;&amp; !fRWLow) || (fRWHigh &amp;&amp; fToggleHigh))
      outmux &lt;= DATH;
    else
      outmux &lt;= DATL;

    if (wrc)
      fToggleHigh &lt;= 1'b0;
    else if (fReadEnd &amp;&amp; !fLatchStat)
      fToggleHigh &lt;= !fToggleHigh;
  end
end
  
endmodule

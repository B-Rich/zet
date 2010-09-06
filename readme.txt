My ZET FORK INFO
------------------------------------------------------------------------------
This is my fork of ZET. My fork is focused on the DE0 and DE1 implementations.
I have a separate repository for ZBC, which is a single board computer specifically
designed for ZET.


======================= Terasic DE1 Board=====================================
To build the system for the Altera DE1 board, just open the file
"boards/altera-de1/syn/kotku.qpf" in Quartus II and compile the system.
As a result, you will have a file named "kotku.sof", which is the conf-
iguration stream for the FPGA.

This implementation is virtually identical to the master hub except that I have 
changed it so the PS@ interface is faithful to the PC version so it is HW 
compatible with PS2 mice. Since the DE1 does not have a secondary mouse PS2 
connector, to run that part, you need to do a hack that basically wires a mouse
to the GPIO. I have sucessfully run mice on 3.3V, they are all CMOS chips and 
don't seem to mind, that is the simplest way of doing it.

Also, I have changed out the RS232 port for a more streamlined version that also
actually works. 

This version is listed under \boards\de1-modified\ which refers to the little
mod you need to do to get a mouse port going.


======================= Terasic DE0 Board=====================================
There are 3 versions of the DE0, since it has a mouse port built into it (although
you need the Y cable to make it work), there is no need to mod the board.

The \boards\de0\ I call the standard DE0 version. The only difference between this one
and the master is that I have substituted the SRAM for FPGA ram since the DE0 does
not have SRAM. There is not enough SRAM to run full VGA, but it will sorta do
monochrome CGA.

The \boards\de0-vdu\ is the same as above except it uses the vdu text only vga
driver. The \boards\de0-shadow\ version is a special version that runs the shardow
bios. I did not implement it using the flash that is on the DE0 because this was 
a special test of the serial flash that I did in preparation for ZBC. But you can see
how I did it by looking at this. For it to work, you have to use the special bios
that is listed under \src\shadowbios\.



Explanation of directories
-----------------------------------------------------------------------------------
\boards\              - different board implementations
\boards\de0\          - Standard DE0 version
\boards\de0-vdu\      - same as above excpet uses vdu text vga driver
\boards\de0-shadow\   - uses shadow bios
\boards\de1-modified\ - DE1 implementation with mouse

\src\                 - Source files for some commands used to transform ROMs
\src\zetbios\         - Zet specific Bios for DE1 & DE0 
\src\shadowbios\      - Special shadow bios

\cores\               - cores i have made changes to
\cores\PS2\           - mouse and keyboard dirvers
\cores\uart16450\     - NEW COM port driver (and it really really works)
\cores\Ethernet\      - Ethernet driver for simple 10BaseT phy shown in zbc

all other cores are same. the wb_switch is messed around with for some of the 
board implementations. For those, I put the special version of the wb_switch in
the rtl directory under the board folder.



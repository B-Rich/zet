Quick help
----------
This is my fork of ZET. My fork is focused on the DE0 and DE1 implementations.
I have a separate repository for ZBC.


=== Altera DE1 ===
To build the system for the Altera DE1 board, just open the file
"boards/altera-de1/syn/kotku.qpf" in Quartus II and compile the system.

As a result, you will have a file named "kotku.sof", which is the conf-
iguration stream for the FPGA.


I am only pushing changes to make things simpler. For full zet go to the
zet master (see the little joke there, almost like zen master, Zeus). 

Explanation of directories
--------------------------
cores/          - cores i have made changes to
cores/PS2/      - mouse and keyboard dirvers
cores/uart16450 - COM port driver
src/            - Source files for some commands used to transform ROMs
src/zetbios/    - Zet specific Bios for DE1


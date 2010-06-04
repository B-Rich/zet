# 
#  ZET BIOS BUILD MAKE  06-04-2010 
#  ---------------------------------------------------------------------------- 
# 
#  This is the make file for the Zet Rom BIOS and VGA BIOS Builds.  
#  To build it, you will need the Open Watcom compiler. If you have 
#  it installed on your machine, then you should be able to just type 
#  wmake and watcom should build it. There will be a small number of warning 
#  messages, this is normal. 
# 
 
# 
# This section combines all 3 rom sections built below, into one 128K ROM  
# ready to be installed. When everything is done running and if all goes well 
# you should have a file BIOS.ROM of exactly 128K, this is your bios rom to  
# download into your DE1 board. 
# 
bios.rom: vgabios.rom ff.rom zetbios.rom
  cat vgabios.rom ff.rom zetbios.rom > $@
 
# 
# This is a little utility that makes a 32k padding binary file so that it 
# comes out to exactly 128K for the flash rom. 
# 
ff.rom: ff.exe
  ff 8192 >$@
 
# 
# This section compiles and links the biossums, vgasums and ff utilities. 
# The biossums is used to computer the checksum byte which is then tacked  
# onto the end of zetbios.rom. The vgasums program scans the vgabios.rom 
# file, computes the checksum which is added to the end. It also pads the 
# file out to a full 32k. The ff utility just makes a 32k file of 0xFF which 
# is used to pad it to a full 64k segment. After all that, both rom files will 
# be 64K in length and ready to be programed into the flash. 
# 
# The C option flags for this build are standard console build since 
# these utilities will run on your PC not on the ZET 
# 
COPTS = -i="C:\WATCOM/h;C:\WATCOM/h/nt" -w4 -e25 -zq -od -d2 -6r -bt=nt -fo=.obj -mf
 
ff.exe: ff.obj .autodepend
    wlink name ff d all sys nt op m op maxe=25 op q op symf FIL ff.obj

ff.obj: ff.c
    wcc386 $(COPTS) ff.c
 
biossums.exe: biossums.obj .autodepend
    wlink name biossums d all sys nt op m op maxe=25 op q op symf FIL biossums.obj
 
biossums.obj: biossums.c
    wcc386 $(COPTS) biossums.c
 
vgasums.exe: vgasums.obj .autodepend 
    wlink name vgasums d all sys nt op m op maxe=25 op q op symf FIL vgasums.obj
 
vgasums.obj: vgasums.c
    wcc386 $(COPTS) vgasums.c
 
# 
# Compiler flags: 
#  -0   generate 8086 code 
#  -ms  forces small model 
#  -ot  Optimize for time of execution rather than size 
#  -zu  assume that SS != DS 
#  -ecc force use of __cedecl calls 
#  -wx  set to maximum warning level 
# 
CFLAGS = -0 -wx -zu -s -ot -d0 -ecc -ms
AFLAGS = -0 -wx

# 
# This section compiles and links the ROM BIOS module. The result of this 
# operation will be later fed through the biossums program in order to  
# compute the checksum and tag that on the end. 
# 
zetbios.rom: zetbios_c.obj zetbios_a.obj biossums.exe
    wlink name $@ system dos &
        OPTION quiet &
        OPTION NOFARCALLS &
        OPTION FILLCHAR=0xFF &
        OPTION NOFARCALLS, MAP &
        OUTPUT raw offset=0xf0000 &
        ORDER &
        clname DATA segment _DATA    segaddr=0xf000 offset=0x0000 &
        clname CODE segment _TEXT    segaddr=0xf000 offset=0x1000 &
                    segment _BIOSSEG segaddr=0xf000 offset=0xe000 &
        FILE { zetbios_c.obj zetbios_a.obj } &
        LIBRARY clibs.lib &
        disable 1014
    biossums $@

# 
# This section tells make to compile only if the files have been changed  
# 
zetbios_c.obj: .autodepend
  wcc $(CFLAGS) zetbios_c.c

zetbios_a.obj: .autodepend 
  wasm $(AFLAGS) zetbios_a.asm

# 
# This section compiles and links the VGA BIOS module. The result of this 
# operation will be later fed through the vgasums program in order to  
# compute the checksum and tag that on the end. 
# 
vgabios.rom: vgabios_c.obj vgabios_a.obj vgasums.exe
    wlink name $@ system dos &
        OPTION quiet &
        OPTION FILLCHAR=0xFF &
        OPTION NOFARCALLS, map &
        OUTPUT raw offset=0xC0000 order &
        clname CODE &
                    segment _VGASEG segaddr=0xC000 offset=0x0000 &
                    segment _TEXT   segaddr=0xC000 offset=0x1000 &
        clname DATA segment _DATA   segaddr=0xC000 offset=0x4000 &
        FILE { vgabios_c.obj vgabios_a.obj } &
        LIBRARY clibs.lib &
        disable 1014
    vgasums $@
 
# 
# This section tells make to compile only if the files have been changed  
# 
vgabios_c.obj: .autodepend
    wcc $(CFLAGS) vgabios_c.c

vgabios_a.obj: .autodepend
    wasm $(AFLAGS) vgabios_a.asm

# 
#  End of make. 
# 

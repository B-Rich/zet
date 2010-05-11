# Compiler flags:
#  -0   generate 8086 code
#  -ms  forces small model
#  -w0  no warnings
#  -s   remove runtime stack checks
#  -ot  
#  -zu  assume that SS != DS
#  -oxs maximum size optimizations
#
CFLAGS = -0 -w0 -s -ot -d0 -ecd -ms 

AFLAGS = -zq -0

.c.obj : .autodepend
	wcc $(CFLAGS) $<

.asm.obj : .autodepend
	wasm $(AFLAGS) $<
	
OBJS = zetbios_c.obj zetbios_a.obj

zetbios.rom : $(OBJS) $(__MAKEFILES__)
	wlink name $@ system dos option quiet OPTION NOFARCALLS , map &
	 output raw offset=0xf0000 order &
	 clname DATA segaddr=0xf000 segment _DATA &
	 clname CODE &
	 segment _TEXT segaddr=0xf000 offset=0x1000 &
	 segment _BIOSSEG segaddr=0xf000 offset=0xc000 &
	 file { $(OBJS) } &
	 library clibs.lib &
	 disable 1014




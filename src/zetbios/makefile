# Compiler flags:
#  -0   generate 8086 code
#  -ms  forces small model
#  -s   remove runtime stack checks
#  -ot  Optimize for time of execution rather than size
#  -zu  assume that SS != DS
#  -ecc force use of __cedecl calls
#
CFLAGS = -0 -wx -zu -s -ot -d0 -ecc -ms 

AFLAGS = -0 -wx 

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




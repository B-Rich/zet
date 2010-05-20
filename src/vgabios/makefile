# Compiler flags:
#  -0   generate 8086 code
#  -ms  forces small model
#  -s   remove runtime stack checks
#  -ot  Optimize for time of execution rather than size
#  -zu  assume that SS != DS
#  -ecc force use of __cedecl calls
#
CFLAGS = -0 -wx -zu -s -ot -d0 -ecc -ms 

.c.obj : .autodepend
	wcc $(CFLAGS) $<

OBJS = vgabios.obj

vgabios.rom : $(OBJS) $(__MAKEFILES__)
	wlink name $@ system dos option quiet OPTION NOFARCALLS , map &
	 output raw offset=0xF0000 order &
	 clname DATA segment _DATA segaddr=0xC000 offset=0x0000 &
	 clname CODE segment _TEXT segaddr=0xC000 offset=0x4000 &
	 file { $(OBJS) } &
	 library clibs.lib &
	 disable 1014




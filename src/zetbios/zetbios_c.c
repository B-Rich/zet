//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//  ZET Bios C Helper functions:
//  This file contains various functions in C called fromt the zetbios.asm
//  module. This module provides support fuctions and special code specific
//  to the Zet computer, specifically, special video support and disk support
//  for the SD and Flash types of disks. 
//
//  This code is compatible with the Open Watcom C Compiler.
//  Originally modified from the Bochs bios by Zeus Gomez Marmolejo
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include "zetbios_c.h"

//--------------------------------------------------------------------------
// Low level assembly functions
//--------------------------------------------------------------------------
Bit16u get_CS() { __asm { mov  ax, cs } }
Bit16u get_SS() { __asm { mov  ax, ss } }

//--------------------------------------------------------------------------
//  memset of count bytes
//--------------------------------------------------------------------------
void memsetb(Bit16u s_segment, Bit16u s_offset, Bit8u value, Bit16u count)
{
    __asm {
                    push ax
                    push cx
                    push es
                    push di
                    mov  cx, count        // count 
                    test cx, cx
                    je   memsetb_end
                    mov  ax, s_segment    // segment 
                    mov  es, ax
                    mov  ax, s_offset     // offset 
                    mov  di, ax
                    mov  al, value        // value 
                    cld
                    rep stosb
     memsetb_end:   pop di
                    pop es
                    pop cx
                    pop ax
    }
}
//--------------------------------------------------------------------------
//  memcpy of count bytes 
//--------------------------------------------------------------------------
void memcpyb(d_segment,d_offset,s_segment, s_offset, count)
Bit16u d_segment, d_offset, s_segment, s_offset, count;
{
    __asm {
                    push ax
                    push cx
                    push es
                    push di
                    push ds
                    push si
                    mov  cx, count      // count 
                    test cx, cx
                    je   memcpyb_end
                    mov  ax, d_segment  // dest segment 
                    mov  es, ax
                    mov  ax, d_offset   // dest offset  
                    mov  di, ax
                    mov  ax, s_segment  // ssegment 
                    mov  ds, ax
                    mov  ax, s_offset   // soffset  
                    mov  si, ax
                    cld
                    rep  movsb
      memcpyb_end:  pop si
                    pop ds
                    pop di
                    pop es
                    pop cx
                    pop ax
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// Low level print functions
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void wrch(Bit8u character)
{
    __asm {
            push    bx
            mov     ah, 0x0e        // 0x0e command
            mov     al, character
            xor     bx,bx
            int     0x10            // 0x10 intereupt
            pop     bx
    }
}
//--------------------------------------------------------------------------
void send(Bit16u action, Bit8u  c)
{
    if(action & BIOS_PRINTF_SCREEN) {
        if(c == '\n') wrch('\r');
        wrch(c);
    }
}
//--------------------------------------------------------------------------
void put_int(Bit16u action, short val, short width, bx_bool neg)
{
    short nval = val / 10;
    if(nval) put_int(action, nval, width - 1, neg);
    else {
        while(--width > 0) send(action, ' ');
        if(neg) send(action, '-');
    }
    send(action, val - (nval * 10) + '0');
}
//--------------------------------------------------------------------------
void put_uint(Bit16u action, unsigned short val, short width, bx_bool neg)
{
    unsigned short nval = val / 10;
    if(nval) put_uint(action, nval, width - 1, neg);
    else {
        while(--width > 0) send(action, ' ');
        if(neg) send(action, '-');
    }
    send(action, val - (nval * 10) + '0');
}
//--------------------------------------------------------------------------
void put_luint(Bit16u action, unsigned long val, short width, bx_bool neg)
{
    unsigned long nval = val / 10;
    if(nval) put_luint(action, nval, width - 1, neg);
    else {
        while(--width > 0) send(action, ' ');
        if(neg) send(action, '-');
    }
    send(action, val - (nval * 10) + '0');
}
//--------------------------------------------------------------------------
void put_str(Bit16u action, Bit16u segment, Bit16u offset)
{
    Bit8u c;
    while(c = read_byte(segment, offset)) {
        send(action, c);
        offset++;
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// bios_printf()  A compact variable argument printf function.
//   Supports %[format_width][length]format
//   where format can be x,X,u,d,s,S,c
//   and the optional length modifier is l (ell)
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void bios_printf(action, s, ...)
Bit16u action; Bit8u *s;
{

    Bit8u c, format_char;
    bx_bool  in_format;
    short i;
    Bit16u  *arg_ptr;
    Bit16u   arg_seg, arg, nibble, hibyte, shift_count, format_width, hexadd;

    arg_ptr = (Bit16u  *)&s;
    arg_seg = get_SS();

    in_format = 0;
    format_width = 0;

    if((action & BIOS_PRINTF_DEBHALT) == BIOS_PRINTF_DEBHALT)
        bios_printf(BIOS_PRINTF_SCREEN, "FATAL: ");

    while(c = read_byte(get_CS(), s)) {
        if( c == '%' ) {
            in_format = 1;
            format_width = 0;
        }
        else if(in_format) {
            if( (c>='0') && (c<='9') ) {
                format_width = (format_width * 10) + (c - '0');
            }
            else {
                arg_ptr++;              // increment to next arg
                arg = read_word(arg_seg, arg_ptr);
                if(c == 'x' || c == 'X') {
                    if(format_width == 0) format_width = 4;
                    if(c == 'x') hexadd = 'a';
                    else         hexadd = 'A';
                    for(i = format_width-1; i >= 0; i--) {
                        nibble = (arg >> (4 * i)) & 0x000f;
                        send (action, (nibble<=9)? (nibble+'0') : (nibble-10+hexadd));
                    }
                }
                else if(c == 'u') {
                    put_uint(action, arg, format_width, 0);
                }
                else if(c == 'l') {
                    s++;
                    c = read_byte(get_CS(), s);                     // is it ld,lx,lu? 
                    arg_ptr++;                                                              // increment to next arg
                    hibyte = read_word(arg_seg, arg_ptr);
                    if(c == 'd') {
                        if(hibyte & 0x8000) put_luint(action, 0L-(((Bit32u) hibyte << 16) | arg), format_width-1, 1);
                        else                put_luint(action, ((Bit32u) hibyte << 16) | arg, format_width, 0);
                    }
                    else if(c == 'u') {
                        put_luint(action, ((Bit32u) hibyte << 16) | arg, format_width, 0);
                    }
                    else if(c == 'x' || c == 'X') {
                        if(format_width == 0) format_width = 8;
                        if(c == 'x') hexadd = 'a';
                        else          hexadd = 'A';
                        for (i=format_width-1; i>=0; i--) {
                            nibble = ((((Bit32u) hibyte <<16) | arg) >> (4 * i)) & 0x000f;
                            send (action, (nibble<=9)? (nibble+'0') : (nibble-10+hexadd));
                        }
                    }
                }
                else if(c == 'd') {
                    if(arg & 0x8000) put_int(action, -arg, format_width - 1, 1);
                    else             put_int(action, arg, format_width, 0);
                }
                else if(c == 's') {
                    put_str(action, get_CS(), arg);
                }
                else if(c == 'S') {
                    hibyte = arg;
                    arg_ptr++;
                    arg = read_word(arg_seg, arg_ptr);
                    put_str(action, hibyte, arg);
                }
                else if(c == 'c') {
                    send(action, arg);
                }
                else bios_printf(BIOS_PRINTF_DEBHALT,"bios_printf: unknown format\n");
                in_format = 0;
            }
        }
        else {
            send(action, c);
        }
        s ++;
    }
    if(action & BIOS_PRINTF_HALT) {  // freeze in a busy loop.
        __asm {
                        cli
            halt2_loop: hlt
                        jmp halt2_loop
        }
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// print_bios_banner -  displays a the bios version
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
#define BIOS_COPYRIGHT_STRING   "(c) 2009, 2010 Zeus Gomez Marmolejo and (c) 2002 MandrakeSoft S.A."
#define BIOS_BANNER             "Zet Test BIOS - build date: "
#define BIOS_BUILD_DATE         "05/9/10\n"
#define BIOS_DATE               ". Date: BIOS_DATE\n\n"
#define BIOS_VERS               "Version: Special\n"
void print_bios_banner(void)
{
    bios_printf(BIOS_PRINTF_SCREEN,BIOS_BANNER);
    bios_printf(BIOS_PRINTF_SCREEN,BIOS_BUILD_DATE);
    bios_printf(BIOS_PRINTF_SCREEN,BIOS_VERS);
    bios_printf(BIOS_PRINTF_SCREEN,BIOS_DATE);
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// BIOS Boot Specification 1.0.1 compatibility
//
// Very basic support for the BIOS Boot Specification, which allows expansion
// ROMs to register themselves as boot devices, instead of just stealing the
// INT 19h boot vector.
//
// This is a hack: to do it properly requires a proper PnP BIOS and we aren't
// one; we just lie to the option ROMs to make them behave correctly.
// We also don't support letting option ROMs register as bootable disk
// drives (BCVs), only as bootable devices (BEVs).
//
// http://www.phoenix.com/en/Customer+Services/White+Papers-Specs/pc+industry+specifications.htm
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
static char drivetypes[][20]={"", "Floppy flash image", "SD card" };
void init_boot_vectors()
{
    ipl_entry_t e;
    Bit8u  sd_error, switches;
    Bit16u count = 0;
    Bit16u hdi, fdi;
    Bit16u ss = get_SS();

    memsetb(IPL_SEG, IPL_TABLE_OFFSET, 0, IPL_SIZE);  // Clear out the IPL table. 

    write_word(IPL_SEG, IPL_BOOTFIRST_OFFSET, 0xFFFF);  // User selected device not set 
    sd_error = read_byte (0x40, 0x8d);
    if(sd_error) {
        bios_printf(BIOS_PRINTF_SCREEN,"Error initializing SD card controller (at stage %d)\n", sd_error);

        // Floppy drive 
        e.type = IPL_TYPE_FLOPPY; e.flags = 0; e.vector = 0; e.description = 0; e.reserved = 0;
        memcpyb(IPL_SEG, IPL_TABLE_OFFSET + count * sizeof (e), ss, &e, sizeof (e));
        count++;
        }
    else {            // Get the boot sequence from the switches
        switches = inb(0xf100);
        if(switches) { hdi = 1; fdi = 0; }
        else         { hdi = 0; fdi = 1; }

        e.type = IPL_TYPE_HARDDISK; e.flags = 0; e.vector = 0; e.description = 0; e.reserved = 0;
        memcpyb(IPL_SEG, IPL_TABLE_OFFSET + hdi * sizeof (e), ss, &e, sizeof (e));

        e.type = IPL_TYPE_FLOPPY; e.flags = 0; e.vector = 0; e.description = 0; e.reserved = 0;
        memcpyb(IPL_SEG, IPL_TABLE_OFFSET + fdi * sizeof (e), ss, &e, sizeof (e));
        count = 2;
        }
    write_word(IPL_SEG, IPL_COUNT_OFFSET, count);   // Remember how many devices we have 
    write_word(IPL_SEG, IPL_SEQUENCE_OFFSET, 1);    // Try to boot first boot device 
}

//--------------------------------------------------------------------------
// get boot vector 
//--------------------------------------------------------------------------
static Bit8u get_boot_vector(i, e)
Bit16u i; ipl_entry_t *e;
{
    Bit16u count;
    Bit16u ss = get_SS();
  
    count = read_word(IPL_SEG, IPL_COUNT_OFFSET); // Get the count of boot devices, and refuse to overrun the array 
    if(i >= count) return 0;    // OK to read this device 
  
    memcpyb(ss, e, IPL_SEG, IPL_TABLE_OFFSET + i * sizeof (*e), sizeof (*e));
    return 1;
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// print_boot_device
//   displays the boot device
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void print_boot_device(ipl_entry_t *e)
{
    Bit16u type;
    char description[33];
    Bit16u ss = get_SS();
    type = e->type;
  
    if(type == IPL_TYPE_BEV) type = 0x4; // NIC appears as type 0x80 
    if(type == 0 || type > 0x4) BX_PANIC("Bad drive type\n");
    bios_printf(BIOS_PRINTF_SCREEN, "Booting from %s", drivetypes[type]);
 
    if(type == 4 && e->description != 0) {    // print product string if BEV, first 32 bytes are significant 
        memcpyb(ss, &description, (Bit16u)(e->description >> 16), (Bit16u)(e->description & 0xffff), 32);
        description[32] = 0; // terminate string 
        bios_printf(BIOS_PRINTF_SCREEN, " [%S]", ss, description);
    }
    bios_printf(BIOS_PRINTF_SCREEN, "...\n\n");
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// print_boot_failure
//   displays the reason why boot failed
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void print_boot_failure(type, reason)
Bit16u type; Bit8u reason;
{
    if(type == 0 || type > 0x3) BX_PANIC("Bad drive type\n");
    printf("Boot failed");
    if(type < 4) {      // Report the reason too 
        if(reason==0)   printf(": not a bootable disk");
        else            printf(": could not read the boot disk");
    }
    printf("\n\n");
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// INT16 Support function
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void int16_function(rDI, rSI, rBP, rSP, rBX, rDX, rCX, rAX, rFLAGS)
Bit16u rDI, rSI, rBP, rSP, rBX, rDX, rCX, rAX, rFLAGS;
{
    Bit8u scan_code, ascii_code, shift_flags, led_flags, count;
    Bit16u kbd_code, max;

    shift_flags = read_byte(0x0040, 0x17);
    led_flags = read_byte(0x0040, 0x97);

    switch (GET_AH()) {
        case 0x00:      // read keyboard input 
            if(!dequeue_key(&scan_code, &ascii_code, 1)) {
                BX_PANIC("KBD: int16h: out of keyboard input\n");
            }
            if(scan_code !=0 && ascii_code == 0xF0) ascii_code = 0;
            else if(ascii_code == 0xE0) ascii_code = 0;
            rAX = (scan_code << 8) | ascii_code;
            break;

        case 0x01:      // check keyboard status 
            if(!dequeue_key(&scan_code, &ascii_code, 0)) {
                SET_ZF();
                return;
            }
            if (scan_code !=0 && ascii_code == 0xF0) ascii_code = 0;
            else if (ascii_code == 0xE0) ascii_code = 0;
            rAX = (scan_code << 8) | ascii_code;
            CLEAR_ZF();
            break;

        case 0x02:     // get shift flag status 
            shift_flags = read_byte(0x0040, 0x17);
            SET_AL(shift_flags);
            break;

        case 0x05:     // store key-stroke into buffer 
            if(!enqueue_key(GET_CH(), GET_CL())) SET_AL(1);
            else                                 SET_AL(0);
            break;

        case 0x09: // GET KEYBOARD FUNCTIONALITY 
            // bit Bochs Description
            //  7    0   reserved
            //  6    0   INT 16/AH=20h-22h supported (122-key keyboard support)
            //  5    1   INT 16/AH=10h-12h supported (enhanced keyboard support)
            //  4    1   INT 16/AH=0Ah supported
            //  3    0   INT 16/AX=0306h supported
            //  2    0   INT 16/AX=0305h supported
            //  1    0   INT 16/AX=0304h supported
            //  0    0   INT 16/AX=0300h supported
            SET_AL(0x30);
            break;

        case 0x10: // read MF-II keyboard input 
            if(!dequeue_key(&scan_code, &ascii_code, 1) ) {
                BX_PANIC("KBD: int16h: out of keyboard input\n");
            }
            if (scan_code !=0 && ascii_code == 0xF0) ascii_code = 0;
            rAX = (scan_code << 8) | ascii_code;
            break;

        case 0x11:  // check MF-II keyboard status 
            if(!dequeue_key(&scan_code, &ascii_code, 0) ) {
                SET_ZF();
                return;
            }
            if (scan_code !=0 && ascii_code == 0xF0) ascii_code = 0;
            rAX = (scan_code << 8) | ascii_code;
            CLEAR_ZF();
            break;

        case 0x12: // get extended keyboard status 
            shift_flags = read_byte(0x0040, 0x17);
            SET_AL(shift_flags);
            shift_flags = read_byte(0x0040, 0x18) & 0x73;
            shift_flags |= read_byte(0x0040, 0x96) & 0x0c;
            SET_AH(shift_flags);
            #if DEBUG_INT16
                bios_printf(BIOS_PRINTF_SCREEN, "int16: func 12 sending %04x\n",AX);
            #endif
            break;

        case 0x92:        // keyboard capability check called by DOS 5.0+ keyb *
            SET_AH(0x80); // function int16 ah=0x10-0x12 supported
            break;

        case 0xA2:       // 122 keys capability check called by DOS 5.0+ keyb 
                         // don't change AH : function int16 ah=0x20-0x22 NOT supported
          break;

        case 0x6F:
            if (GET_AL() == 0x08)  SET_AH(0x02); // unsupported, aka normal keyboard

        default:
            bios_printf(BIOS_PRINTF_INFO,"KBD: unsupported int 16h function %02x\n", GET_AH());
    }
}

//--------------------------------------------------------------------------
// De-queue the key 
//--------------------------------------------------------------------------
BOOL dequeue_key(scan_code, ascii_code, incr)
Bit8u *scan_code; Bit8u *ascii_code; unsigned int incr;
{
    Bit16u buffer_start, buffer_end, buffer_head, buffer_tail;
    Bit16u ss;
    Bit8u  acode, scode;

#if BX_CPU < 2
    buffer_start = 0x001E;
    buffer_end   = 0x003E;
#else
    buffer_start = read_word(0x0040, 0x0080);
    buffer_end   = read_word(0x0040, 0x0082);
#endif

    buffer_head = read_word(0x0040, 0x001a);
    buffer_tail = read_word(0x0040, 0x001c);

    if(buffer_head != buffer_tail) {
        ss = get_SS();
        acode = read_byte(0x0040, buffer_head);
        scode = read_byte(0x0040, buffer_head+1);
        write_byte(ss, ascii_code, acode);
        write_byte(ss, scan_code, scode);
        if(incr) {
            buffer_head += 2;
            if(buffer_head >= buffer_end) buffer_head = buffer_start;
            write_word(0x0040, 0x001a, buffer_head);
        }
        return(1);
    }
    else {
        return(0);
    }
    return(0);
}

//--------------------------------------------------------------------------
// INT09 Support function
//--------------------------------------------------------------------------
void int09_function(rDI, rSI, rBP, rSP, rBX, rDX, rCX, rAX)
Bit16u rDI, rSI, rBP, rSP, rBX, rDX, rCX, rAX;
{
    Bit8u scancode, asciicode, shift_flags;
    Bit8u mf2_flags, mf2_state;

    scancode = GET_AL();    // DS has been set to F000 before call
    if(scancode == 0) {
        BX_INFO("KBD: int09 handler: AL=0\n");
        return;
    }

    shift_flags = read_byte(0x0040, 0x17);
    mf2_flags = read_byte(0x0040, 0x18);
    mf2_state = read_byte(0x0040, 0x96);
    asciicode = 0;

    switch (scancode) {
        case 0x3a: // Caps Lock press 
            shift_flags ^= 0x40;
            write_byte(0x0040, 0x17, shift_flags);
            mf2_flags |= 0x40;
            write_byte(0x0040, 0x18, mf2_flags);
            break;

        case 0xba: // Caps Lock release 
            mf2_flags &= ~0x40;
            write_byte(0x0040, 0x18, mf2_flags);
            break;

        case 0x2a: // L Shift press 
            shift_flags |= 0x02;
            write_byte(0x0040, 0x17, shift_flags);
            break;

        case 0xaa: // L Shift release 
            shift_flags &= ~0x02;
            write_byte(0x0040, 0x17, shift_flags);
            break;

        case 0x36: // R Shift press 
            shift_flags |= 0x01;
            write_byte(0x0040, 0x17, shift_flags);
            break;

        case 0xb6: // R Shift release 
            shift_flags &= ~0x01;
            write_byte(0x0040, 0x17, shift_flags);
            break;

        case 0x1d: // Ctrl press 
            if((mf2_state & 0x01) == 0) {
                shift_flags |= 0x04;
                write_byte(0x0040, 0x17, shift_flags);
                if (mf2_state & 0x02) {
                    mf2_state |= 0x04;
                    write_byte(0x0040, 0x96, mf2_state);
                }
                else {
                    mf2_flags |= 0x01;
                    write_byte(0x0040, 0x18, mf2_flags);
                }
            }
            break;

        case 0x9d: // Ctrl release 
            if((mf2_state & 0x01) == 0) {
                shift_flags &= ~0x04;
                write_byte(0x0040, 0x17, shift_flags);
                if (mf2_state & 0x02) {
                    mf2_state &= ~0x04;
                    write_byte(0x0040, 0x96, mf2_state);
                }
                else {
                    mf2_flags &= ~0x01;
                    write_byte(0x0040, 0x18, mf2_flags);
                }
            }
            break;

        case 0x38: // Alt press 
            shift_flags |= 0x08;
            write_byte(0x0040, 0x17, shift_flags);
            if(mf2_state & 0x02) {
                mf2_state |= 0x08;
                write_byte(0x0040, 0x96, mf2_state);
            }
            else {
                mf2_flags |= 0x02;
                write_byte(0x0040, 0x18, mf2_flags);
            }
            break;

        case 0xb8: // Alt release 
            shift_flags &= ~0x08;
            write_byte(0x0040, 0x17, shift_flags);
            if(mf2_state & 0x02) {
                mf2_state &= ~0x08;
                write_byte(0x0040, 0x96, mf2_state);
            }
            else {
                mf2_flags &= ~0x02;
                write_byte(0x0040, 0x18, mf2_flags);
            }
            break;

        case 0x45: // Num Lock press 
            if((mf2_state & 0x03) == 0) {
                mf2_flags |= 0x20;
                write_byte(0x0040, 0x18, mf2_flags);
                shift_flags ^= 0x20;
                write_byte(0x0040, 0x17, shift_flags);
            }
            break;

        case 0xc5: // Num Lock release 
            if((mf2_state & 0x03) == 0) {
                mf2_flags &= ~0x20;
                write_byte(0x0040, 0x18, mf2_flags);
            }
            break;

        case 0x46: // Scroll Lock press 
            mf2_flags |= 0x10;
            write_byte(0x0040, 0x18, mf2_flags);
            shift_flags ^= 0x10;
            write_byte(0x0040, 0x17, shift_flags);
            break;

        case 0xc6: // Scroll Lock release 
            mf2_flags &= ~0x10;
            write_byte(0x0040, 0x18, mf2_flags);
            break;

        default:
            if(scancode & 0x80) {
                break; // toss key releases ... 
            }
            if(scancode > MAX_SCAN_CODE) {
                bios_printf(BIOS_PRINTF_INFO,"KBD: int09h_handler(): unknown scancode read: 0x%02x!\n", scancode);
                return;
            }
            if(shift_flags & 0x08) { // ALT 
                asciicode = scan_to_scanascii[scancode].alt;
                scancode = scan_to_scanascii[scancode].alt >> 8;
            }
            else if(shift_flags & 0x04) { // CONTROL 
                asciicode = scan_to_scanascii[scancode].control;
                scancode = scan_to_scanascii[scancode].control >> 8;
            }
            else if(((mf2_state & 0x02) > 0) && ((scancode >= 0x47) && (scancode <= 0x53))) {
                asciicode = 0xe0; // extended keys handling 
                scancode = scan_to_scanascii[scancode].normal >> 8;
            }
            else if(shift_flags & 0x03) { // LSHIFT + RSHIFT 
                // check if lock state should be ignored  because a SHIFT key are pressed 
                if(shift_flags & scan_to_scanascii[scancode].lock_flags) {
                    asciicode = scan_to_scanascii[scancode].normal;
                    scancode = scan_to_scanascii[scancode].normal >> 8;
                }
                else {
                    asciicode = scan_to_scanascii[scancode].shift;
                    scancode = scan_to_scanascii[scancode].shift >> 8;
                }
            }
            else {         // check if lock is on 
            if(shift_flags & scan_to_scanascii[scancode].lock_flags) {
                asciicode = scan_to_scanascii[scancode].shift;
                scancode = scan_to_scanascii[scancode].shift >> 8;
            }
            else {
                asciicode = scan_to_scanascii[scancode].normal;
                scancode = scan_to_scanascii[scancode].normal >> 8;
            }
        }
        if(scancode==0 && asciicode==0) {
            BX_INFO("KBD: int09h_handler(): scancode & asciicode are zero?\n");
        }
        enqueue_key(scancode, asciicode);
        break;
    }
    if((scancode & 0x7f) != 0x1d) {
        mf2_state &= ~0x01;
    }
    mf2_state &= ~0x02;
    write_byte(0x0040, 0x96, mf2_state);
}

//--------------------------------------------------------------------------
// Enqueue Key
//--------------------------------------------------------------------------
BOOL enqueue_key(scan_code, ascii_code)
Bit8u scan_code, ascii_code;
{
    Bit16u buffer_start, buffer_end, buffer_head, buffer_tail, temp_tail;

#if BX_CPU < 2
    buffer_start = 0x001E;
    buffer_end   = 0x003E;
#else
    buffer_start = read_word(0x0040, 0x0080);
    buffer_end   = read_word(0x0040, 0x0082);
#endif

    buffer_head = read_word(0x0040, 0x001A);
    buffer_tail = read_word(0x0040, 0x001C);

    temp_tail = buffer_tail;
    buffer_tail += 2;
    if(buffer_tail >= buffer_end) buffer_tail = buffer_start;

    if(buffer_tail == buffer_head) {
        return(0);
    }
    write_byte(0x0040, temp_tail, ascii_code);
    write_byte(0x0040, temp_tail+1, scan_code);
    write_word(0x0040, 0x001C, buffer_tail);
    return(1);
}

//--------------------------------------------------------------------------
// INT13 Interupt handler function
//--------------------------------------------------------------------------
#define SET_DISK_RET_STATUS(status) write_byte(0x0040, 0x0074, status)
//--------------------------------------------------------------------------
void int13_harddisk(rDS, rES, rDI, rSI, rBP, rELDX, rBX, rDX, rCX, rAX, rIP, rCS, rFLAGS)
Bit16u rDS, rES, rDI, rSI, rBP, rELDX, rBX, rDX, rCX, rAX, rIP, rCS, rFLAGS;
{
    Bit8u    drive, num_sectors, sector, head, status;
    Bit8u    drive_map, sd_error;
    Bit8u    n_drives;
    Bit16u   max_cylinder, cylinder;
    Bit16u   hd_cylinders;
    Bit8u    hd_heads, hd_sectors;
    Bit8u    sector_count;
    Bit16u   tempbx;
    Bit16u   addr_l, addr_h;
    Bit32u   log_sector;

    write_byte(0x0040, 0x008e, 0);  // clear completion flag

    // at this point, DL is >= 0x80 to be passed from the floppy int13h handler code 
    // check how many disks first (cmos reg 0x12), return an error if drive not present 
    sd_error = read_byte (0x40, 0x8d);
    if (sd_error) drive_map = 0;
    else          drive_map = 1;

    n_drives = 1;

    if(!(drive_map & (1<<(GET_ELDL()&0x7f)))) { // allow 0, 1, or 2 disks 
        SET_AH(0x01);
        SET_DISK_RET_STATUS(0x01);
        SET_CF(); /* error occurred */
        return;
    }

    switch(GET_AH()) {

        case 0x00: // disk controller reset 
            SET_AH(0);
            SET_DISK_RET_STATUS(0);
            set_diskette_ret_status(0);
            set_diskette_current_cyl(0, 0); // current cylinder, diskette 1 
            set_diskette_current_cyl(1, 0); // current cylinder, diskette 2 
            CLEAR_CF(); // successful 
            return;
            break;

        case 0x01: /* read disk status */
            status = read_byte(0x0040, 0x0074);
            SET_AH(status);
            SET_DISK_RET_STATUS(0);
            if (status) SET_CF();         // set CF if error status read 
            else        CLEAR_CF();
            return;
            break;

        case 0x04: // verify disk sectors
        case 0x02: // read disk sectors
            drive = GET_ELDL();

            // get_hd_geometry(drive, &hd_cylinders, &hd_heads, &hd_sectors);
            // fixed geometry:
            hd_cylinders = HD_CYLINDERS;
            hd_heads     = HD_HEADS;
            hd_sectors   = HD_SECTORS;

            num_sectors = GET_AL();
            cylinder    = (GET_CL() & 0x00c0) << 2 | GET_CH();
            sector      = (GET_CL() & 0x3f);
            head        = GET_DH();

            if((cylinder >= hd_cylinders)||(sector > hd_sectors)||(head >= hd_heads)) {
                SET_AH(1);
                SET_DISK_RET_STATUS(1);
                SET_CF(); /* error occurred */
                return;
            }
            if(GET_AH() == 0x04 ) {
                SET_AH(0);
                SET_DISK_RET_STATUS(0);
                CLEAR_CF();
                return;
            }
            log_sector = ((Bit32u)cylinder) * ((Bit32u)hd_heads) * ((Bit32u)hd_sectors)
                         + ((Bit32u)head) * ((Bit32u)hd_sectors) + ((Bit32u)sector) - 1;

            sector_count = 0;
            tempbx = rBX;

            __asm { sti } //;; enable higher priority interrupts

            while(1) {
                addr_l = ((Bit16u) log_sector) << 9;
                addr_h =  (Bit16u) (log_sector >> 7);
                __asm {
                    mov   di, tempbx            //;; store temp bx in real DI register
                    cmp   di, 0xfe00            //;; adjust if there will be an overrun
                    jbe   i13_f02_no_adjust
                i13_f02_adjust:
                    sub   di, 0x0200 //; sub 512 bytes from offset
                    mov   ax, es
                    add   ax, 0x0020 //; add 512 to segment
                    mov   es, ax

                i13_f02_no_adjust:
                    //;; ES: destination segment
                    //;; DI: destination offset
                    mov   bx, addr_l
                    mov   cx, addr_h

                    //; SD card command CMD17
                    mov   dx, 0x0100
                    mov   ax, 0x51   //; CS = 0, CMD17
                    out   dx, ax
                    mov   al, ch      //; addr[31:24]
                    out   dx, al
                    mov   al, cl      //; addr[23:16]
                    out   dx, al
                    mov   al, bh      //; addr[15:8]
                    out   dx, al
                    mov   al, bl      //; addr[7:0]
                    out   dx, al
                    mov   al, 0xff    //; CRC (not used)
                    out   dx, al
                    out   dx, al      //; wait

                i13_f02_read_res_cmd17:
                    in    al, dx      //; card response
                    cmp   al, 0
                    jne   i13_f02_read_res_cmd17

                    //; read data token: 0xfe
                i13_f02_read_tok_cmd17:
                    in    al, dx
                    cmp   al, 0xfe
                    jne   i13_f02_read_tok_cmd17
                    mov   cx, 0x100
                i13_f02_read_bytes:
                    in    al, dx     //; low byte
                    mov   bl, al
                    in    al, dx     //; high byte
                    mov   bh, al
                    mov   es:[di], bx   // eseg
                    add   di, 2
                    loop  i13_f02_read_bytes

                    //; we are done, retrieve checksum
                    mov   ax, 0xffff
                    out   dx, al     //; Checksum, 1st byte
                    out   dx, al     //; Checksum, 2nd byte
                    out   dx, al     //; wait
                    out   dx, al     //; wait
                    out   dx, ax     //; CS = 1 (disable SD)

                i13_f02_done:        //;; store real DI register back to temp bx
                    mov  tempbx, di
                }
                sector_count++;
                log_sector++;
                num_sectors--;
                if (num_sectors) continue;
                else break;
            }
            SET_AH(0);
            SET_DISK_RET_STATUS(0);
            SET_AL(sector_count);
            CLEAR_CF(); /* successful */
            return;
            break;

        case 0x03: // write disk sectors 
            drive = GET_ELDL ();
            // get_hd_geometry(drive, &hd_cylinders, &hd_heads, &hd_sectors);
            // fixed geometry:
            hd_cylinders = HD_CYLINDERS;
            hd_heads     = HD_HEADS;
            hd_sectors   = HD_SECTORS;

            num_sectors = GET_AL();
            cylinder    = GET_CH();
            cylinder    |= ( ((Bit16u) GET_CL()) << 2) & 0x300;
            sector      = (GET_CL() & 0x3f);
            head        = GET_DH();

            if((cylinder >= hd_cylinders) || (sector > hd_sectors) || (head >= hd_heads)) {
                SET_AH(1);
                SET_DISK_RET_STATUS(1);
                SET_CF();   // error occurred 
                return;
            }
            log_sector = ((Bit32u)cylinder) * ((Bit32u)hd_heads) * ((Bit32u)hd_sectors)
                        + ((Bit32u)head) * ((Bit32u)hd_sectors) + ((Bit32u)sector) - 1;

            sector_count = 0;
            tempbx = rBX;

            __asm { sti }  //;; enable higher priority interrupts
            while(1) {
                addr_l = ((Bit16u) log_sector) << 9;
                addr_h = (Bit16u) (log_sector >> 7);

                __asm {
                        mov   si, tempbx        //;; store temp bx in real SI register
                        cmp   si, 0xfe00        //;; adjust if there will be an overrun
                        jbe   i13_f03_no_adjust
                i13_f03_adjust:
                        sub   si, 0x0200 //; sub 512 bytes from offset
                        mov   ax, es
                        add   ax, 0x0020 //; add 512 to segment
                        mov   es, ax

                i13_f03_no_adjust:
                        //;; ES: source segment
                        //;; SI: source offset
                        mov   bx, addr_l
                        mov   cx, addr_h

                        //; SD card command CMD24
                        mov   dx, 0x0100
                        mov   ax, 0x58    //; CS = 0, CMD24
                        out   dx, ax
                        mov   al, ch      //; addr[31:24]
                        out   dx, al
                        mov   al, cl      //; addr[23:16]
                        out   dx, al
                        mov   al, bh      //; addr[15:8]
                        out   dx, al
                        mov   al, bl      //; addr[7:0]
                        out   dx, al
                        mov   al, 0xff    //; CRC (not used)
                        out   dx, al
                        out   dx, al      //; wait

                i13_f03_read_res_cmd24:
                        in    al, dx      //; command response
                        cmp   al, 0
                        jne   i13_f03_read_res_cmd24
                        mov   al, 0xff   //; wait
                        out   dx, al
                        mov   al, 0xfe   //; start of block: token 0xfe
                        out   dx, al
                        mov   cx, 0x100
                        
                i13_f03_write_bytes:
                        mov   ax, es:[si]      // eseg
                        out   dx, al
                        mov   al, ah
                        out   dx, al
                        add   si, 2
                        loop  i13_f03_write_bytes
                        //; send dummy checksum
                        mov   al, 0xff
                        out   dx, al
                        out   dx, al

                        //; data response
                        in    al, dx
                        and   al, 0xf
                        cmp   al, 0x5
                        je    i13_f03_good_write
                        hlt               //; problem writing

                        //; write finished?
                i13_f03_good_write:
                        in    al, dx
                        cmp   al, 0
                        je    i13_f03_good_write

                        //; goodbye mr. writer!
                        mov   ax,  0xffff
                        out   dx, al     //; wait
                        out   dx, al     //; wait
                        out   dx, ax     //; CS = 1 (disable SD)

                i13_f03_done:   //;; store real SI register back to temp bx
                        mov  tempbx, si
                }

            sector_count++;
            log_sector++;
            num_sectors--;
            if (num_sectors) continue;
            else break;
        }
        SET_AH(0);
        SET_DISK_RET_STATUS(0);
        SET_AL(sector_count);
        CLEAR_CF(); /* successful */
        return;
        break;

        case 0x08:
            drive = GET_ELDL ();
            // same as get_hd_geometry(drive, &hd_cylinders, &hd_heads, &hd_sectors);
            // fixed geometry:
            hd_cylinders = HD_CYLINDERS;
            hd_heads     = HD_HEADS;
            hd_sectors   = HD_SECTORS;
            max_cylinder = hd_cylinders - 2; // 0 based 
            SET_AL(0);
            SET_CH(max_cylinder & 0xff);
            SET_CL(((max_cylinder >> 2) & 0xc0) | (hd_sectors & 0x3f));
            SET_DH(hd_heads - 1);
            SET_DL(n_drives);       // returns 0, 1, or 2 hard drives 
            SET_AH(0);
            SET_DISK_RET_STATUS(0);
            CLEAR_CF();             // successful 
            return;
            break;

        case 0x09: // initialize drive parameters 
        case 0x0c: // seek to specified cylinder 
        case 0x0d: // alternate disk reset 
        case 0x10: // check drive ready 
        case 0x11: // recalibrate 
            SET_AH(0);
            SET_DISK_RET_STATUS(0);
            CLEAR_CF(); // successful 
            return;
            break;

        case 0x14: // controller internal diagnostic 
            SET_AH(0);
            SET_DISK_RET_STATUS(0);
            CLEAR_CF(); // successful 
            SET_AL(0);
            return;
            break;

        case 0x15: // read disk drive size 
            drive = GET_ELDL();
            // same as get_hd_geometry(drive, &hd_cylinders, &hd_heads, &hd_sectors);
            // fixed geometry:
            hd_cylinders = HD_CYLINDERS;
            hd_heads     = HD_HEADS;
            hd_sectors   = HD_SECTORS;

            #if 1
                rDX = HD_HEADS*HD_SECTORS;
                rCX = rDX*(HD_CYLINDERS-1);
            #else
            
            __asm {
                    mov  al, hd_heads
                    mov  ah, hd_sectors
                    mul  al, ah         //;; ax = heads * sectors
                    mov  bx, hd_cylinders
                    dec  bx         //;; use (cylinders - 1) ???
                    mul  ax, bx     //;; dx:ax = (cylinders -1) * (heads * sectors)
                    //;; now we need to move the 32bit result dx:ax to what the
                    //;; BIOS wants which is cx:dx.
                    //;; and then into CX:DX on the stack
                    mov  rCX, dx
                    mov  rDX, ax
            }
            #endif
            SET_AH(3);  // hard disk accessible
            SET_DISK_RET_STATUS(0); // ??? should this be 0
            CLEAR_CF(); // successful
            return;
            break;

        default:
            BX_INFO("int13_harddisk: function %02xh unsupported, returns fail\n", GET_AH());
            goto int13_fail;
            break;
    }
    __asm {
        _int13_fail:
    }
    int13_fail:
        SET_AH(0x01); // defaults to invalid function in AH or invalid parameter
        
    int13_fail_noah:
        SET_DISK_RET_STATUS(GET_AH());
        
    int13_fail_nostatus:
        SET_CF();     // error occurred
    return;

    int13_success:
        SET_AH(0x00); // no error
        
    int13_success_noah:
        SET_DISK_RET_STATUS(0x00);
        CLEAR_CF();   // no error
    return;
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
//  Transfer Sector drive
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void transf_sect_drive_a(s_segment, s_offset)
Bit16u s_segment; Bit16u s_offset;
{
    __asm {
                push ax
                push bx
                push cx
                push dx
                push di
                push ds
                mov  ax, s_segment //; segment
                mov  ds, ax
                mov  bx, s_offset  //; offset
                mov  dx, 0xe000
                mov  cx, 256
                xor  di, di
    one_sect:   in   ax, dx        //; read word from flash
                mov  [bx+di], ax   //; write word
                inc  dx
                inc  dx
                inc  di
                inc  di
                loop one_sect
                pop  ds
                pop  di
                pop  dx
                pop  cx
                pop  bx
                pop  ax
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// The RAM Disk is stored at 0x110000 to 0x277FFF in the SDRAM
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
Bit16u GetRamdiskSector(Sector)
Bit16u Sector;
{
    Bit16u Page;
    // The bits above the upper five bits tells us which memory location
    // The lower five bits tells us where in the 16K Page the Sector is
    Page = RAM_DISK_BASE + (Sector >> 5);
    outb(EMS_PAGE1_REG, Page);       // Set the first 16K
    return ((Sector & 0x001F) << 9); // Return the memory location within the sector
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// Make RAM disk
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void MakeRamdisk()
{
    Bit16u Sector;
    Bit16u base_count;

    // The principle of this routine is to copy directly from flash to the ram disk
    // Using the same call that is used to read the flash disk

    outb(EMS_ENABLE_REG, EMS_ENABLE_VAL);      // Turn on EMS from 0xB0000 - 0xBFFFF

    // Configure the sector address
    for (Sector = 0; Sector < SECTOR_COUNT; Sector++) {
        outw(FLASH_PAGE_REG, Sector);              // Select the Flash Disk Sector
        base_count = GetRamdiskSector(Sector);               // Select the Flash Page and get the address within the page of the Sector
        transf_sect_drive_a(EMS_SECTOR_OFFSET, base_count);    // We now have the correct page of flash selected and the sector is always in the same place so just pass the place to copy it too
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
//  Called from INT13 - Floppy diskette API in BIOS.asm                     
//  New diskette parameter table adding 3 parameters from IBM Since no
//  provisions are made for multiple drive types, most values in this
//  table are ignored. set parameters for 1.44M floppy here                  
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
Bit8u diskette_param_table2[] = {
         0xAF,
         0x02,   // head load time 0000001, DMA used 
         0x25,
         0x02,
           18,
         0x1B,
         0xFF,
         0x6C,
         0xF6,
         0x0F,
         0x08,
           79,   // maximum track      
            0,   // data transfer rate 
            4    // drive type in cmos 
};

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// INT13 Diskette service function
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void int13_diskette_function(rDS, rES, rDI, rSI, rBP, rELDX, rBX, rDX, rCX, rAX, rIP, rCS, rFLAGS)
Bit16u rDS, rES, rDI, rSI, rBP, rELDX, rBX, rDX, rCX, rAX, rIP, rCS, rFLAGS;
{
    Bit8u  drive, num_sectors, track, sector, head, status;
    Bit16u base_address, base_count, base_es;
    Bit8u  page, mode_register, val8, dor;
    Bit8u  return_status[7];
    Bit8u  drive_type, num_floppies, ah;
    Bit16u es, last_addr;
    Bit16u log_sector, tmp, i, j;
    Bit16u RamAddress;

    ah = GET_AH();

    switch (ah) {
    
        case 0x00:  // Disk controller reset
            drive = GET_ELDL();  // Was here but that meant that drive was not set for other cases
            SET_AH(0);
            set_diskette_ret_status(0);
            CLEAR_CF();                         // Successful
            set_diskette_current_cyl(drive, 0); // Current cylinder
            break;

        case 0x02: // Read Diskette Sectors
            num_sectors = GET_AL();
            track       = GET_CH();
            sector      = GET_CL();
            head        = GET_DH();
            drive       = GET_ELDL();                    // Was here but that meant that drive was not set for other cases

            if((drive > 1) || (head > 1) || (sector == 0) || (num_sectors == 0) || (num_sectors > 72)) {
                BX_INFO("int13_diskette: read/write/verify: parameter out of range\n");
                SET_AH(1);
                set_diskette_ret_status(1);
                SET_AL(0);    // No sectors have been read
                SET_CF();     // An error occurred
                return;
            }

            page            = (rES >> 12);      // The upper 4 bits give the page
            base_es         = (rES << 4);       // The lower 16bits contributed by ES
            base_address    = base_es + rBX;    // The lower 16 bits of address

            // Contributed by ES:BX
            if(base_address < base_es) {           // If the base_address is less than the base_es then there was an overflow above so we need to go to the next page
                page++;                            // In case of carry, adjust page by 1
            }
            base_count = (num_sectors * 512) - 1;     // Work out the number of bytes to be transfered less one (last address to be transfered)

            // Check for 64K boundary overrun
            last_addr = base_address + base_count;   // Add the base address to work out if the last address is in the same segment
            if(last_addr < base_address) {           // If the last address is less than the base then there must have been an overflow above !
                SET_AH(0x09);
                set_diskette_ret_status(0x09);
                SET_AL(0);                         // No sectors have been read
                SET_CF();                          // An error occurred
                return;
            }

            log_sector    = track * 36 + head * 18 + sector - 1;  // Calculate the first sector we are going to read
            last_addr    = page << 12;                                            

            if(drive == DRIVE_A) {      // This is the Flash Based Drive
                for(j = 0; j < num_sectors; j++) {
                    outw(FLASH_PAGE_REG, log_sector + j);
                    base_count = base_address + (j << 9);
                    transf_sect_drive_a(last_addr, base_count);   // We now have the correct page of flash selected and the sector is always in the same place so just pass the place to copy it too
                }
            }
            else {                  // This is the SDRAM based drive
                for(j = 0; j < num_sectors; j++) {
                    RamAddress = GetRamdiskSector(log_sector + j);  // Pass in the sector which will set the right RAM page and give back the ram address
                    base_count = base_address + (j << 9);
                    memcpyb(last_addr, base_count, EMS_SECTOR_OFFSET, RamAddress, SECTOR_SIZE);  // Copy the sector
                }
            }
            // ??? should track be new val from return_status[3] ?
            set_diskette_current_cyl(drive, track);

            // AL = number of sectors read (same value as passed)
            SET_AH(0x00); // success
            CLEAR_CF();   // success
            break;

        case 0x08: // read diskette drive parameters
            //BX_DEBUG_INT13_FL("floppy f08\n");
            drive = GET_ELDL();

            if(drive > 1) {
                rAX = 0;
                rBX = 0;
                rCX = 0;
                rDX = 0;
                rES = 0;
                rDI = 0;
                SET_DL(num_floppies);
                SET_CF();
                return;
            }
            drive_type = 0x44;      /// inb_cmos(0x10);
            num_floppies = 0;
            if(drive_type & 0xf0) num_floppies++;
            if(drive_type & 0x0f) num_floppies++;
            if (drive == 0) drive_type >>= 4;
            else            drive_type &= 0x0f;
            SET_BH(0);
            SET_BL(drive_type);
            SET_AH(0);
            SET_AL(0);
            SET_DL(num_floppies);

            switch(drive_type) {
                case 0: // none
                    rCX = 0;
                    SET_DH(0); // max head #
                    break;

                case 1: // 360KB, 5.25"
                    rCX = 0x2709; // 40 tracks, 9 sectors
                    SET_DH(1); // max head #
                    break;

                case 2: // 1.2MB, 5.25"
                    rCX = 0x4f0f; // 80 tracks, 15 sectors
                    SET_DH(1); // max head #
                    break;

                case 3: // 720KB, 3.5"
                    rCX = 0x4f09; // 80 tracks, 9 sectors
                    SET_DH(1); // max head #
                    break;

                case 4: // 1.44MB, 3.5"
                    rCX = 0x4f12; // 80 tracks, 18 sectors
                    SET_DH(1); // max head #
                    break;

                case 5: // 2.88MB, 3.5"
                    rCX = 0x4f24; // 80 tracks, 36 sectors
                    SET_DH(1); // max head #
                    break;

                case 6: // 160k, 5.25"
                    rCX = 0x2708; // 40 tracks, 8 sectors
                    SET_DH(0); // max head #
                    break;

                case 7: // 180k, 5.25"
                    rCX = 0x2709; // 40 tracks, 9 sectors
                    SET_DH(0); // max head #
                    break;

                case 8: // 320k, 5.25"
                    rCX = 0x2708; // 40 tracks, 8 sectors
                    SET_DH(1);    // max head #
                    break;

                default: // ?
                    BX_PANIC("floppy: int13: bad floppy type\n");
            }
            // Set es & di to point to 11 byte diskette param table in ROM
            __asm {
                mov ax, WORD PTR diskette_param_table2
                mov rDI, ax
                mov rES, cs
            }
            CLEAR_CF(); // success, disk status not changed upon success 
            break;
        
        case 0x15: // read diskette drive type
             drive = GET_ELDL();     // BX_DEBUG_INT13_FL("floppy f15\n");
            if(drive > 1) {
                SET_AH(0);  // only 2 drives supported
                SET_CF();   // set_diskette_ret_status here ???
                return;
            }
            drive_type = 0x44;            // inb_cmos(0x10);
            if(drive == 0) {
                drive_type >>= 4;
            }
            else {
                drive_type &= 0x0f;
            }
            CLEAR_CF(); // successful, not present
            if(drive_type == 0) SET_AH(0); // drive not present
            else                SET_AH(1); // drive present, does not support change line
            break;

        case 0x03:    // Write disk sector
            num_sectors = GET_AL();
            track       = GET_CH();
            sector      = GET_CL();
            head        = GET_DH();
            drive       = GET_ELDL();                      // Was here but that meant that drive was not set for other cases

            if(drive == DRIVE_B) {                         // Writing only works on Drive B
                if((drive > 1) || (head > 1) || (sector == 0) || (num_sectors == 0) || (num_sectors > 72)) {
                    BX_INFO("int13_diskette: read/write/verify: parameter out of range\n");
                    SET_AH(1);
                    set_diskette_ret_status(1);
                    SET_AL(0);                             // No sectors have been read
                    SET_CF();                              // An error occurred
                    return;
                }
                page               = (rES >> 12);          // The upper 4 bits give the page
                base_es            = (rES << 4);           // The lower 16bits contributed by ES
                base_address    = base_es + rBX;           // The lower 16 bits of address

                // Contributed by ES:BX
                if (base_address < base_es) page++;        // If the base_address is less than the base_es then there was an overflow above so we need to go to the next page

                base_count = (num_sectors << 9) - 1;       // Work out the number of bytes to be transfered less one (last address to be transfered)

                // Check for 64K boundary overrun
                last_addr = base_address + base_count;     // Add the base address to work out if the last address is in the same segment
                if(last_addr < base_address) {             // If the last address is less than the base then there must have been an overflow above !
                    SET_AH(0x09);
                    set_diskette_ret_status(0x09);
                    SET_AL(0);                                        // No sectors have been read
                    SET_CF();                                        // An error occurred
                    return;
                }

                log_sector    = track * 36 + head * 18 + sector - 1;            // Calculate the first sector we are going to read
                last_addr    = page << 12;                                            

                // This is the SDRAM based drive
                // This is the SDRAM based drive
                for(j = 0; j < num_sectors; j++) {
                    RamAddress = GetRamdiskSector(log_sector + j);   // Pass in the sector which will set the right RAM page and give back the ram address
                    base_count = base_address + (j << 9);
                    memcpyb(EMS_SECTOR_OFFSET, RamAddress, last_addr, base_count, SECTOR_SIZE);        // Copy the sector
                }

                // ??? should track be new val from return_status[3] ?
                set_diskette_current_cyl(drive, track);

                // AL = number of sectors read (same value as passed)
                SET_AH(0x00); // success
                CLEAR_CF();   // success
                break;
            }
            // Fall Through to what it used to do!

        default:
            BX_INFO("int13_diskette: unsupported AH=%02x\n", GET_AH());
            SET_AH(0x01); // ???
            set_diskette_ret_status(1);
            SET_CF();
            break;
    }
}
//--------------------------------------------------------------------------
void set_diskette_ret_status(value)
Bit8u value;
{
    write_byte(0x0040, 0x0041, value);
}
//--------------------------------------------------------------------------
void set_diskette_current_cyl(drive, cyl)
Bit8u drive;  Bit8u cyl;
{
    if(drive > 1) drive = 1;    // Temporary hack: for MSDOS
    write_byte(0x0040, 0x0094 + drive, cyl);
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// INT 19 Support Function
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void int19_function(Bit16u seq_nr)
{
    Bit16u ebda_seg=read_word(0x0040,0x000E);
    Bit16u bootdev;
    Bit8u  bootdrv, sd_error;
    Bit8u  bootchk;
    Bit16u bootseg;
    Bit16u bootip;
    Bit16u status;
    Bit16u bootfirst;

    ipl_entry_t e;

    // Here we assume that BX_ELTORITO_BOOT is defined, so
    //   CMOS regs 0x3D and 0x38 contain the boot sequence:
    //     CMOS reg 0x3D & 0x0f : 1st boot device
    //     CMOS reg 0x3D & 0xf0 : 2nd boot device
    //     CMOS reg 0x38 & 0xf0 : 3rd boot device
    //   boot device codes:
    //     0x00 : not defined
    //     0x01 : first floppy
    //     0x02 : first harddrive
    //     0x03 : first cdrom
    //     0x04 - 0x0f : PnP expansion ROMs (e.g. Etherboot)
    //     else : boot failure

    // Read user selected device 
    bootdev = read_word(IPL_SEG, IPL_SEQUENCE_OFFSET);

    // Translate from CMOS runes to an IPL table offset by subtracting 1 
    bootdev -= 1;

    // Read the boot device from the IPL table 
    if(get_boot_vector(bootdev, &e) == 0) {
        printf("Invalid boot device (0x%x)\n", bootdev);
        return;
    }

    // Do the loading, and set up vector as a far pointer to the boot
    // address, and bootdrv as the boot drive 
    print_boot_device(&e);

    switch(e.type) {
        case IPL_TYPE_FLOPPY:   // FDD 
        case IPL_TYPE_HARDDISK: // HDD 
            bootdrv = (e.type == IPL_TYPE_HARDDISK) ? 0x80 : 0x00;
            bootseg = 0x07c0;
            status = 0;

            __asm {
                push ax
                push bx
                push cx
                push dx
                mov  dl, bootdrv
                mov  ax, bootseg
                mov  es, ax         //;; segment
                xor  bx, bx         //;; offset
                mov  ah, 0x02       //;; function 2, read diskette sector
                mov  al, 0x01       //;; read 1 sector
                mov  ch, 0x00       //;; track 0
                mov  cl, 0x01       //;; sector 1
                mov  dh, 0x00       //;; head 0
                int  0x13           //;; read sector
                jnc  int19_load_done
                mov  ax, 0x0001
                mov  status, ax

            int19_load_done:
                pop  dx
                pop  cx
                pop  bx
                pop  ax
            }

            if(status != 0) {
                print_boot_failure(e.type, 1);
                return;
            }
            if(read_word (0x07c0, 0x1fe)!=0xaa55) {
                print_boot_failure(e.type, 0);
                return;
            }

            // Canonicalize bootseg:bootip 
            bootip = (bootseg & 0x0fff) << 4;
            bootseg &= 0xf000;
            break;

        default:
            return;
    }

    // Debugging info 
    BX_INFO("Booting from %x:%x\n", bootseg, bootip);

    // Jump to the boot vector 
    __asm {
        //;; Build an iret stack frame that will take us to the boot vector.
        //;; iret pops ip, then cs, then flags, so push them in the opposite order.
        pushf
        mov  ax, bootseg
        push ax
        mov  ax, bootip
        push ax
        mov  ax, 0xaa55    //;; Set the magic number in ax and the boot drive in dl.
        mov  dl, bootdrv
        xor  bx, bx     //;; Zero some of the other registers.
        mov  ds, bx
        mov  es, bx
        mov  bp, bx
        iret            //;; Go!
    }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// BOOT HALT
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void boot_halt ()
{
    printf("No more devices to boot - System halted.\n");
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// INT 1A Support function
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
void int1a_function(regs, ds, iret_addr)
    pusha_regs_t regs;        // regs pushed from PUSHA instruction
    Bit16u ds;                // previous DS:, DS set to 0x0000 by asm wrapper
    iret_addr_t  iret_addr;   // CS,IP,Flags pushed from original INT call
{
    Bit8u val8;

    __asm { sti }

    switch(regs.u.r8.ah) {
        case 0: // get current clock count
            __asm { cli }
            regs.u.r16.cx = BiosData->ticks_high;
            regs.u.r16.dx = BiosData->ticks_low;
            regs.u.r8.al  = BiosData->midnight_flag;
            BiosData->midnight_flag = 0; // reset flag
            __asm { sti }
            // AH already 0
            ClearCF(iret_addr.flags); // OK
            break;

        default:
            SetCF(iret_addr.flags); // Unsupported
    }
}   

//---------------------------------------------------------------------------
//  End
//---------------------------------------------------------------------------


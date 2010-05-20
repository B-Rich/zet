//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//  ZET VGA Bios:
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#ifndef vgabios_h_included
#define vgabios_h_included

//---------------------------------------------------------------------------
// Type definitions
//---------------------------------------------------------------------------
typedef unsigned char  Bit8u;
typedef unsigned short Bit16u;
typedef unsigned long  Bit32u;
typedef unsigned short Boolean;

//---------------------------------------------------------------------------
// Macro Definitions
//---------------------------------------------------------------------------
#define SET_AL(val8) AX = ((AX & 0xff00) | (val8))
#define SET_BL(val8) BX = ((BX & 0xff00) | (val8))
#define SET_CL(val8) CX = ((CX & 0xff00) | (val8))
#define SET_DL(val8) DX = ((DX & 0xff00) | (val8))
#define SET_AH(val8) AX = ((AX & 0x00ff) | ((val8) << 8))
#define SET_BH(val8) BX = ((BX & 0x00ff) | ((val8) << 8))
#define SET_CH(val8) CX = ((CX & 0x00ff) | ((val8) << 8))
#define SET_DH(val8) DX = ((DX & 0x00ff) | ((val8) << 8))

#define GET_AL() ( AX & 0x00ff )
#define GET_BL() ( BX & 0x00ff )
#define GET_CL() ( CX & 0x00ff )
#define GET_DL() ( DX & 0x00ff )
#define GET_AH() ( AX >> 8 )
#define GET_BH() ( BX >> 8 )
#define GET_CH() ( CX >> 8 )
#define GET_DH() ( DX >> 8 )

#define SET_CF()        FLAGS |= 0x0001
#define CLEAR_CF()      FLAGS &= 0xfffe
#define GET_CF()        (FLAGS & 0x0001)

#define SET_ZF()        FLAGS |= 0x0040
#define CLEAR_ZF()      FLAGS &= 0xffbf
#define GET_ZF()        (FLAGS & 0x0040)

#define SCROLL_DOWN     0
#define SCROLL_UP       1
#define NO_ATTR         2
#define WITH_ATTR       3

#define SCREEN_SIZE(x,y)        (((x*y*2)|0x00ff)+1)
#define SCREEN_MEM_START(x,y,p) ((((x*y*2)|0x00ff)+1)*p)
#define SCREEN_IO_START(x,y,p)  ((((x*y)|0x00ff)+1)*p)

//---------------------------------------------------------------------------
// Function prototypes:
//---------------------------------------------------------------------------
static Bit8u    read_byte();
static Bit16u   read_word();
static void     write_byte();
static void     write_word();
static Bit8u    inb();
static Bit16u   inw();
static void     outb();
static void     outw();
static Bit16u   get_SS();
static void     printf();     // Output

static Bit8u    find_vga_entry();

static void     memsetb(Bit16u s_segment, Bit16u s_offset, Bit8u value, Bit16u count);
static void     memsetw(Bit16u s_segment, Bit16u s_offset, Bit16u value, Bit16u count);
static void     memcpyb(Bit16u d_segment, Bit16u d_offset, Bit16u s_segment, Bit16u s_offset, Bit16u count);
static void     memcpyw(Bit16u d_segment, Bit16u d_offset, Bit16u s_segment, Bit16u s_offset, Bit16u count);

static void     biosfn_set_video_mode();
static void     biosfn_set_cursor_shape();
static void     biosfn_set_cursor_pos();
static void     biosfn_get_cursor_pos(Bit8u page, Bit16u *shape, Bit16u *pos);
static void     biosfn_set_active_page();
static void     biosfn_scroll();
static void     biosfn_read_char_attr(Bit8u page, Bit16u *car);
static void     biosfn_write_char_attr();
static void     biosfn_write_char_only();
static void     biosfn_write_teletype();
static void     biosfn_load_text_8_16_pat();
static void     biosfn_write_string();
extern Bit16u    video_save_pointer_table[];


static void     vgabios();
static void     vgabios_init_func();
static void     biosfn_get_video_mode();
static void     vgabios_int10_handler();
static void     init_vga_card();
static void     init_bios_area();
static void     display_info();
static void     display_string(char *ascii_string);
static void     int10_func(Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u);
static void     biosfn_group_1A();
static void     biosfn_set_text_block_specifier();
static void     biosfn_read_video_dac_state();
static void     idiv_u();
static void     biosfn_group_10();
static void     biosfn_set_overscan_border_color();
static void     biosfn_set_all_palette_reg();
static void     biosfn_toggle_intensity();
static void     biosfn_get_single_palette_reg();
static void     biosfn_read_overscan_border_color();
static void     biosfn_get_all_palette_reg();
static void     biosfn_set_single_dac_reg();
static void     biosfn_set_all_dac_reg();
static void     biosfn_read_pel_mask();
static void     biosfn_set_pel_mask();
static void     biosfn_read_all_dac_reg();
static void     biosfn_read_single_dac_reg();
static void     biosfn_select_video_dac_color_page();
static void     biosfn_set_single_palette_reg();


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Compatibility Functions:
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//#ifdef __WATCOMC__
#if 0

Bit8u inb(Bit16u port);
#pragma aux inb = "in al,dx" parm [dx] value [al] modify [] nomemory;

Bit16u inw(Bit16u port);
#pragma aux inw = "in ax,dx" parm [dx] value [ax] modify [] nomemory;

void outb(Bit16u port, Bit8u val);
#pragma aux outb = "out dx,al" parm [dx] [al] modify [] nomemory;

void outw(Bit16u port, Bit16u val);
#pragma aux outw = "out dx,ax" parm [dx] [ax] modify [] nomemory;

#else
//---------------------------------------------------------------------------
Bit8u inb(Bit16u port) {
    __asm {
        push dx
        mov  dx, port
        in   al, dx
        pop  dx
    }
}
//---------------------------------------------------------------------------
void outb(Bit16u port, Bit8u  val)
{
    __asm {
        push ax
        push dx
        mov  dx, port
        mov  al, val
        out  dx, al
        pop  dx
        pop  ax
    }   
}
//---------------------------------------------------------------------------
Bit16u inw(Bit16u port)
{
    __asm {
        push dx
        mov  dx, port
        in   ax, dx
        pop  dx
    }
}
//---------------------------------------------------------------------------
void outw(Bit16u port, Bit16u  val)
{
    __asm {
        push ax
        push dx
        mov  dx, port
        mov  ax, val
        out  dx, ax
        pop  dx
        pop  ax
    }
}
#endif

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Assembly functions to access memory directly
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
Bit8u read_byte(Bit16u s_segment, Bit16u s_offset)
{
    __asm {
        push bx
        push ds
        mov  ax, s_segment   // segment 
        mov  ds, ax
        mov  bx, s_offset    // offset 
        mov  al, ds:[bx]     // al = return value (byte) 
        pop  ds
        pop  bx
    }
}
//---------------------------------------------------------------------------
Bit16u read_word(Bit16u s_segment, Bit16u s_offset)
{
    __asm {
        push bx
        push ds
        mov  ax, s_segment // segment 
        mov  ds, ax
        mov  bx, s_offset  // offset 
        mov  ax, ds:[bx]   // ax = return value (word) 
        pop  ds
        pop  bx
    }
}
//---------------------------------------------------------------------------
void write_byte(Bit16u s_segment, Bit16u s_offset, Bit8u data)
{
    __asm {
        push ax
        push bx
        push ds
        mov  ax, s_segment  // segment  
        mov  ds, ax
        mov  bx, s_offset   // offset 
        mov  al, data       // data byte 
        mov  ds:[bx], al    // write data byte 
        pop  ds
        pop  bx
        pop  ax
    }
}
//---------------------------------------------------------------------------
void write_word(Bit16u s_segment, Bit16u s_offset, Bit16u data)
{
    __asm {
        push ax
        push bx
        push ds
        mov  ax, s_segment   // segment 
        mov  ds, ax
        mov  bx, s_offset    //  offset 
        mov  ax, data        //  data word 
        mov  ds:[bx], ax     //  write data word 
        pop  ds
        pop  bx
        pop  ax
    }
}
//---------------------------------------------------------------------------

#endif



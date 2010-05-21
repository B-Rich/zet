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
//---------------------------------------------------------------------------
// Function prototypes:
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static void     biosfn_set_video_mode(Bit8u mode);
static void     biosfn_set_cursor_shape(Bit8u CH, Bit8u CL);
static void     biosfn_set_cursor_pos(Bit8u page, Bit16u cursor);
static void     biosfn_get_cursor_pos(Bit8u page, Bit16u *shape, Bit16u *pos);
static void     biosfn_set_active_page(Bit8u page);
static void     biosfn_scroll(Bit8u, Bit8u, Bit8u, Bit8u, Bit8u, Bit8u, Bit8u, Bit8u);
static void     biosfn_read_char_attr(Bit8u page, Bit16u *car);
static void     biosfn_write_char_attr(Bit8u car, Bit8u page, Bit8u attr, Bit16u count);
static void     biosfn_write_char_only(Bit8u car, Bit8u page, Bit8u attr, Bit16u count);
static void     biosfn_write_teletype(Bit8u car, Bit8u page, Bit8u attr, Bit8u flag);
static void     set_scan_lines(Bit8u lines);
static void     get_font_access();
static void     release_font_access();
static void     biosfn_load_text_8_16_pat(Bit8u AL, Bit8u BL);
static void     biosfn_write_string(Bit8u, Bit8u, Bit8u, Bit16u, Bit8u, Bit8u, Bit16u, Bit16u);
static void     printf(Bit8u *s);     
static Bit8u    find_vga_entry(Bit8u mode);


//---------------------------------------------------------------------------
// Prototypes for Utility Functions
//---------------------------------------------------------------------------
static Bit8u    inb(Bit16u port);
static Bit16u   inw(Bit16u port);
static void     outb(Bit16u port, Bit8u  val);
static void     outw(Bit16u port, Bit16u  val);
static Bit8u    read_byte(Bit16u s_segment, Bit16u s_offset);
static Bit16u   read_word(Bit16u s_segment, Bit16u s_offset);
static void     write_byte(Bit16u s_segment, Bit16u s_offset, Bit8u data);
static void     write_word(Bit16u s_segment, Bit16u s_offset, Bit16u data);
static Bit16u   get_SS();
static void     memsetb(Bit16u s_segment, Bit16u s_offset, Bit8u value, Bit16u count);
static void     memsetw(Bit16u s_segment, Bit16u s_offset, Bit16u value, Bit16u count);
static void     memcpyb(Bit16u d_segment, Bit16u d_offset, Bit16u s_segment, Bit16u s_offset, Bit16u count);
static void     memcpyw(Bit16u d_segment, Bit16u d_offset, Bit16u s_segment, Bit16u s_offset, Bit16u count);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Exported Function prototypes:
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void int10_func(Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u,Bit16u);


//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------



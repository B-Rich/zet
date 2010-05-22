;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; Super Simple ROM BIOS compatability entry points:
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; $0000 ; Start of ROM Utilities
;; $e05b ; POST Entry Point
;; $e6f2 ; INT 19h Boot Load Service Entry Point
;; $f045 ; INT 10 Functions 0-Fh Entry Point
;; $f065 ; INT 10h Video Support Service Entry Point
;; $f0a4 ; MDA/CGA Video Parameter Table (INT 1Dh)
;; $fff0 ; Power-up Entry Point
;; $fff5 ; ASCII Date ROM was built - 8 characters in MM/DD/YY
;; $fffe ; System Model ID
;;--------------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; ROM Utilities Externals
;;--------------------------------------------------------------------------
EBDA_SEG                equ     09FC0h   ; EBDA is used for PS/2 mouse support, and IDE BIOS, etc.
EBDA_SIZE               equ     1        ; In KiB
BASE_MEM_IN_K           equ     640 - EBDA_SIZE
IPL_SEG                 equ     09ff0h   ; 256 bytes at 0x9ff00 -- 0x9ffff is used for the IPL boot table.
IPL_COUNT_OFFSET        equ     0080h    ; u16: number of valid table entries
IPL_SEQUENCE_OFFSET     equ     0082h    ; u16: next boot device
IPL_BOOTFIRST_OFFSET    equ     0084h    ; u16: user selected device
IPL_TABLE_ENTRIES       equ     8        ; num Table entries
IPL_TYPE_BEV            equ     080h     ;

;;--------------------------------------------------------------------------
;; ROM Utilities Externals
;;--------------------------------------------------------------------------
                        EXTRN  _print_bios_banner      :proc      ; Print the BIOS Banner message
                        EXTRN  _int13_diskette_function:proc      ; Contained in C source module
                        EXTRN  _int13_harddisk         :proc      ; Contained in C source module
                        EXTRN  _boot_halt              :proc      ; Contained in C source module
                        EXTRN  _int19_function         :proc      ; Contained in C source module
                        EXTRN  _int1a_function         :proc      ; Time-of-day Service Entry Point
                        EXTRN  _int09_function         :proc      ; BIOS Interupt 09
                        EXTRN  _int16_function         :proc      ; Keyboard Service Entry Point
                        EXTRN  _init_boot_vectors      :proc      ; Initialize Boot Vectors

;;--------------------------------------------------------------------------
;; Set vector macro
;;--------------------------------------------------------------------------
SET_INT_VECTOR MACRO parm1, parm2, parm3
                        mov     ax, parm3
                        mov     ds:[parm1*4], ax
                        mov     ax, parm2
                        mov     ds:[parm1*4+2], ax
ENDM

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
startofrom              equ     0e000h
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        .Model  Tiny    ;; this forces it to nears on code and data
                        .8086           ;; this forces it to use 80186 and lower
_BIOSSEG                SEGMENT 'CODE'
                        assume  cs:_BIOSSEG
biosrom:                org     0000h           ;; start of ROM, get placed at 0E000h
bios_name_string:       db      "zetbios1.0"
                        db      0,0,0,0

;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;;  - POST -  POST Entry Point
;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------
                        org     (0e05bh - startofrom)   
post:                   xor     ax, ax          ;; Clear AX register
normal_post:            cli                     ;; case 0: normal startup
                        mov     dx, 0f200h      ; CSR_HPDMC_SYSTEM = HPDMC_SYSTEM_BYPASS|HPDMC_SYSTEM_RESET|HPDMC_SYSTEM_CKE;
                        mov     ax, 7           ; Bring CKE high
                        out     dx, ax          ; Initialize the SDRAM controller
                        mov     dx, 0f202h      ; Precharge All
                        mov     ax, 0400bh      ; CSR_HPDMC_BYPASS = 0x400B;
                        out     dx, ax          ; Output the word of data to the SDRAM Controller
                        mov     ax, 0000dh      ; CSR_HPDMC_BYPASS = 0xD;
                        out     dx, ax          ; Auto refresh
                        mov     ax, 0000dh      ; CSR_HPDMC_BYPASS = 0xD;
                        out     dx, ax          ; Auto refresh
                        mov     ax, 023fh       ; CSR_HPDMC_BYPASS = 0x23F;
                        out     dx, ax          ; Load Mode Register, Enable DLL
                        mov     cx, 50          ; Wait about 200 cycles
a:                      loop    a               ; Loop until 50 goes to zero
                        mov     dx, 0f200h      ; CSR_HPDMC_SYSTEM = HPDMC_SYSTEM_CKE;
                        mov     ax, 4           ; Leave Bypass mode and bring up hardware controller
                        out     dx, ax          ; Output the word of data to the SDRAM Controller
                        mov     ax, 0fffeh      ; We are done with the controller, we can use the memory now
                        mov     sp, ax
                        xor     ax, ax
                        mov     ds, ax
                        mov     ss, ax
                        mov     es, ax         ;; zero out BIOS data area (40:00..40:ff)
                        mov     cx, 0080h      ;; 128 words
                        mov     di, 0400h
                        cld
                        rep     stosw          ;; repeat
                        xor     bx, bx         ;; set all interrupts to default handleroffset index
                        mov     cx, 0100h                  ;; counter (256 interrupts)
                        mov     ax, dummy_iret_handler     ;; dummy_iret_handler
                        mov     dx, 0F000h                 ;; load the Bios Data Segment

post_default_ints:      mov     [bx], ax             ;; Store dummy return handler offset
                        add      bx,  2              ;; Go to next word
                        mov     [bx], dx             ;; Store Bios Segment word
                        add      bx,  2              ;; Go to next word
                        loop    post_default_ints

                        SET_INT_VECTOR 079h, 0, 0     ;; set vector 0x79 to zero this is used by 'gardian angel' protection system
                        mov     ax, BASE_MEM_IN_K     ;; base memory in K 40:13 (word)
                        mov     ds:00413h, ax

                        SET_INT_VECTOR 018h, 0F000h, int18_handler    ;; Bootstrap failure vector
                        SET_INT_VECTOR 019h, 0F000h, int19_handler    ;; Bootstrap Loader vector
                        SET_INT_VECTOR 01ch, 0F000h, int1c_handler    ;; User Timer Tick vector
                        SET_INT_VECTOR 012h, 0F000h, int12_handler    ;; Memory Size Check vector
                        SET_INT_VECTOR 011h, 0F000h, int11_handler    ;; Equipment Configuration Check vector
                        SET_INT_VECTOR 015h, 0F000h, int15_handler    ;; BIOS system services

ebda_post:              xor     ax, ax                                ; mov EBDA seg into 40E
                        mov     ds, ax
                        mov     word ptr ds:[040Eh], EBDA_SEG

                        SET_INT_VECTOR 008h, 0F000h, int08_handler    ;; PIT setup - int 1C already points at dummy_iret_handler (above)
                        
                        SET_INT_VECTOR 009h, 0F000h, int09_handler    ;; Keyboard Hardware Service Entry Point
                        SET_INT_VECTOR 016h, 0F000h, int16_handler    ;; Keyboard Service Entry Point

                        xor     ax, ax
                        mov     ds, ax
                        mov     BYTE PTR ds:00417h, al      ; keyboard shift flags, set 1
                        mov     BYTE PTR ds:00418h, al      ; keyboard shift flags, set 2
                        mov     BYTE PTR ds:00419h, al      ; keyboard alt-numpad work area
                        mov     BYTE PTR ds:00471h, al      ; keyboard ctrl-break flag
                        mov     BYTE PTR ds:00497h, al      ; keyboard status flags 4
                        mov     al, 010h
                        mov     BYTE PTR ds:00496h, al      ; keyboard status flags 3
                        mov     bx, 001Eh                   ; keyboard head of buffer pointer
                        mov     WORD PTR ds:0041Ah, bx
                        mov     WORD PTR ds:0041Ch, bx      ; keyboard end of buffer pointer
                        mov     bx, 001Eh                   ; keyboard pointer to start of buffer
                        mov     WORD PTR ds:00480h, bx
                        mov     bx, 003Eh                   ; keyboard pointer to end of buffer
                        mov     WORD PTR ds:00482h, bx

                        SET_INT_VECTOR 01Ah, 0F000h, int1a_handler    ;; CMOS RTC
                        SET_INT_VECTOR 010h, 0F000h, int10_handler    ;; int10_handler - Video Support Service Entry Point

                        mov     cx, 0c000h             ;; init vga bios
                        mov     ax, 0c780h
                        call    rom_scan               ;; Scan ROM  
                        call    _print_bios_banner     ;; Print the openning banner

                        call    hard_drive_post        ;; Hard Drive setup
                        call    _init_boot_vectors     ;; Initialize the boot vectors

                        mov     cx, 0c800h             ;; Initialize option roms
                        mov     ax, 0e000h             ;; Initialize option roms
                        call    rom_scan               ;; Call the rom scan again

                        sti                            ;; enable interrupts
                        int     019h                   ;; Now load dos boot sector and jump to it

;;--------------------------------------------------------------------------
;; Padding
;;--------------------------------------------------------------------------
                        db      0,0,0,0,0,0,0,0
                        
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;- INT18h - ;; Boot Failure recovery: try the next device.
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
int18_handler:          mov     ax, 0fffeh       
                        mov     sp, ax              ;; Reset SP and SS
                        xor     ax, ax              ;; Clear ax regiater
                        mov     ss, ax              ;; Clears SS
                        mov     bx, IPL_SEG         ;; Get the boot sequence number out of the IPL memory
                        mov     ds, bx                                  ;; Set segment
                        mov     bx, WORD PTR ds:[IPL_SEQUENCE_OFFSET]   ;; BX is now the sequence number
                        inc     bx                                      ;; Increment BX register
                        mov     ax, WORD PTR ds:[IPL_COUNT_OFFSET]
                        cmp     ax, bx
                        jg      i18_next
                        call    _boot_halt
                        hlt                                     ;; Halt the processor
i18_next:               xor     ax, ax
                        mov     WORD PTR ds:[IPL_SEQUENCE_OFFSET], bx    ;; Write it back
                        mov     ds, ax                                   ;; and reset the segment to zero.
                        push    bx                              ;; Carry on in the INT 19h handler, 
                        jmp     int19_next_boot                 ;; using the new sequence number

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;- POST: HARD DRIVE - IRQ 14 = INT 76h, INT 76h calls INT 15h function ax=9100 
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
hard_drive_post:        xor     ax, ax           ; 
                        mov     ds, ax           ; Data segment = 0, control table location
                        mov     ds:0474h, al     ; hard disk status of last operation
                        mov     ds:0477h, al     ; hard disk port offset (XT only ???)
                        mov     ds:048ch, al     ; hard disk status register
                        mov     ds:048dh, al     ; hard disk error register
                        mov     ds:048eh, al     ; hard disk task complete flag
                        mov     al, 01h          ; Drive #
                        mov     ds:0475h, al     ; hard disk number attached
                        mov     al, 0c0h         ; Control byte
                        mov     ds:0476h, al     ; hard disk control byte

                        SET_INT_VECTOR 013h, 0F000h, int13_handler
                        SET_INT_VECTOR 076h, 0F000h, int76_handler

                        mov     al, 0ffh         ; Initialize the SD card controller
                        mov     dx, 0100h        ; Location of SD controller 
                        mov     cx, 10           ; Number of times to repeat
  
hd_post_init80:         out     dx, al           ; 80 cycles of initialization
                        loop    hd_post_init80   ; Looop 80 times
                        mov     ax, 040h         ; CS = 0, CMD0: reset the SD card
                        out     dx, ax           ; Reset the card 
                        xor     al, al           ; Clear al
                        out     dx, al           ; 32-bit zero value
                        out     dx, al
                        out     dx, al
                        out     dx, al
                        mov     al, 095h         ; Fixed value
                        out     dx, al           ; load it
                        mov     al, 0ffh         
                        out     dx, al           ; wait
                        in      al, dx           ; status
                        mov     cl, al
                        mov     ax, 0ffffh
                        out     dx, ax          ; CS = 1
                        cmp     cl, 1
                        je      hd_post_cmd1
                        mov     al, 1           ; error 1
                        mov     ds:0048dh, al
                        ret

hd_post_cmd1:           mov     ax, 041h        ; CS = 0, CMD1: activate the init sequence
                        out     dx, ax
                        xor     al, al
                        out     dx, al          ; 32-bit zero value
                        out     dx, al
                        out     dx, al
                        out     dx, al
                        mov     al, 0ffh
                        out     dx, al          ; CRC (not used)
                        out     dx, al          ; wait
                        in      al, dx          ; status
                        mov     cl, al
                        mov     ax, 0ffffh
                        out     dx, ax          ; CS = 1
                        test    cl, 0ffh
                        jnz     hd_post_cmd1
                        
                        mov     ax, 050h        ; CS = 0, CMD16: set block length
                        out     dx, ax
                        xor     al, al
                        out     dx, al          ; 32-bit value
                        out     dx, al
                        mov     al, 2           ; 512 bytes
                        out     dx, al
                        xor     al, al
                        out     dx, al
                        mov     al, 0ffh
                        out     dx, al          ; CRC (not used)
                        out     dx, al          ; wait
                        in      al, dx          ; status
                        mov     cl, al
                        mov     ax, 0ffffh
                        out     dx, ax         ; CS = 1
                        test    cl, 0ffh
                        jz      hd_post_success
                        mov     al, 2        ; error 2
                        mov     ds:0048dh, al
                        ret
                        
hd_post_success:        ret

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; record completion in BIOS task complete flag
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
int76_handler:          push    ax
                        push    ds
                        mov     ax, 00040h
                        mov     ds, ax
                        mov     BYTE PTR ds:008Eh, 0ffh
                        pop     ds
                        pop     ax
                        iret  

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;  ROM Checksum calculation
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
rom_checksum:           push    ax
                        push    bx
                        push    cx
                        xor     ax, ax
                        xor     bx, bx
                        xor     cx, cx
                        mov     ch, BYTE PTR ds:[2]
                        shl     cx, 1
checksum_loop:          add     al, BYTE PTR [bx]
                        inc     bx
                        loop    checksum_loop
                        and     al, 0ffh
                        pop     cx
                        pop     bx
                        pop     ax
                        ret

;;--------------------------------------------------------------------------
;;  We need a copy of this string, but we are not actually a PnP BIOS,
;;  so make sure it is *not* aligned, so OSes will not see it if they scan.
;;--------------------------------------------------------------------------
;;                        align   2
                        even
                        db      0
pnp_string:             DB      "$PnP"

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; Scan for existence of valid expansion ROMS.
;;   Video ROM:   from 0xC0000..0xC7FFF in 2k increments
;;   General ROM: from 0xC8000..0xDFFFF in 2k increments
;;   System  ROM: only 0xE0000
;;
;; Header:
;;   Offset    Value
;;   0         055h
;;   1         0AAh
;;   2         ROM length in 512-byte blocks
;;   3         ROM initialization entry point (FAR CALL)
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
rom_scan:
rom_scan_loop:          push    ax                      ;; Save AX
                        mov     ds, cx
                        mov     ax, 0004h               ;; start with increment of 4 (512-byte) blocks = 2k
                        cmp     WORD PTR ds:[0], 0AA55h ;; look for signature
                        jne     rom_scan_increment
                        call    rom_checksum
                        jnz     rom_scan_increment
                        mov     al, BYTE PTR ds:[2]     ;; change increment to ROM length in 512-byte blocks
                        test    al, 003h                ;; We want our increment in 512-byte quantities, rounded to
                        jz      block_count_rounded     ;; the nearest 2k quantity, since we only scan at 2k intervals.
                        and     al, 0fch                ;; needs rounding up
                        add     al, 004h

block_count_rounded:    xor     bx, bx              ;; Restore DS back to 0000:
                        mov     ds, bx
                        push    ax                  ;; Save AX
                        push    di                  ;; Save DI  Push addr of ROM entry point
                        push    cx                  ;; Push seg
                        mov     ax, 00003h          ;; Offset
                        push    ax                  ;; Put offset on stack            
                        mov     ax, 0F000h          ;; Point ES:DI at "$PnP", which tells the ROM that we are a PnP BIOS.
                        mov     es, ax              ;; That should stop it grabbing INT 19h; we will use its BEV instead.
                        lea     di, pnp_string+startofrom
                        mov     bp, sp                          ;; Call ROM init routine using seg:off on stack
                        call    DWORD PTR ss:[bp]       ;; should assemble to 0ff05eh 0 (and it does under tasm)
                        cli                             ;; In case expansion ROM BIOS turns IF on
                        add     sp, 2                   ;; Pop offset value
                        pop     cx                      ;; Pop seg value (restore CX)
                                                                ;; Look at the ROM's PnP Expansion header.  Properly, we're supposed
                                                                ;; to init all the ROMs and then go back and build an IPL table of
                                                                ;; all the bootable devices, but we can get away with one pass.
                        mov     ds, cx                          ;; ROM base
                        mov     bx, WORD PTR ds:[01ah]          ;; 0x1A is the offset into ROM header that contains...
                        mov     ax, [bx]                        ;; the offset of PnP expansion header, where...
                        cmp     ax, 05024h                      ;; we look for signature "$PnP"
                        jne     no_bev
                        mov     ax, 2[bx]
                        cmp     ax, 0506eh
                        jne     no_bev
                        mov     ax, 01ah[bx]            ;; 0x1A is also the offset into the expansion header of...
                        cmp     ax, 00000h              ;; the Bootstrap Entry Vector, or zero if there is none.
                        je      no_bev                  ;; Found a device that thinks it can boot the system.
                                                        ;; Record its BEV and product name string.
                        mov     di, 010h[bx]            ;; Pointer to the product name string or zero if none
                        mov     bx, IPL_SEG             ;; Go to the segment where the IPL table lives
                        mov     ds, bx
                        mov     bx, WORD PTR ds:[IPL_COUNT_OFFSET]  ;; Read the number of entries so far
                        cmp     bx, IPL_TABLE_ENTRIES
                        je      no_bev                           ;; Get out if the table is full
                        push    cx
                        mov     cx, 04h                          ;; Zet: Needed to be compatible with 8086
                        shl     bx, cl                           ;; Turn count into offset (entries are 16 bytes)
                        pop     cx
                        mov     WORD PTR 0[bx], IPL_TYPE_BEV    ;; This entry is a BEV device
                        mov     WORD PTR 6[bx], cx              ;; Build a far pointer from the segment...
                        mov     WORD PTR 4[bx], ax              ;; and the offset
                        cmp     di, 00000h
                        je      no_prod_str
                        mov     0Ah[bx], cx                     ;; Build a far pointer from the segment...
                        mov     8[bx], di                       ;; and the offset
no_prod_str:            push    cx
                        mov     cx, 04h
                        shr     bx, cl                          ;; Turn the offset back into a count
                        pop     cx
                        inc     bx                               ;; We have one more entry now
                        mov     WORD PTR ds:IPL_COUNT_OFFSET, bx ;; Remember that.
no_bev:                 pop     di                               ;; Restore DI
                        pop     ax                               ;; Restore AX
rom_scan_increment:     push    cx
                        mov     cx, 5                       ;; convert 512-bytes blocks to 16-byte increments
                        shl     ax, cl                      ;; because the segment selector is shifted left 4 bits.
                        pop     cx
                        add     cx, ax
                        pop     ax                          ;; Restore AX
                        cmp     cx, ax
                        jbe     rom_scan_loop       ;; This is a far jump
                        xor     ax, ax                      ;; Restore DS back to 0000:
                        mov     ds, ax
                        ret
      
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;- INT 13h Fixed Disk Services Entry Point -
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; located back to 0e3feh
;; needs a rewrite:
;;   - call a function that detect which function to call
;;   - make all called C function get the same parameters list
;;--------------------------------------------------------------------------
                        org     (0e3feh - startofrom)   ;; INT 13h Fixed Disk Services Entry Point
int13_handler:          pushf                       ;; Push all registers onto stack
                        push    ax                  ;; This will save them all and pass them to
                        push    cx                  ;; the C program
                        push    dx                  ;;
                        push    bx                  ;;
                        push    bp                  ;;
                        push    si                  ;;
                        push    di                  ;;
                        push    es                  ;; The order of parms in C program 
                        push    ds                  ;; DS, ES, DI, SI, BP, BX, DX, CX, AX, FLAGS

                        push    bx                  ;; Save BX reg just for a second here
                        mov     bx, 0f000h          ;; Load the Bios Data segment
                        mov     ds, bx              ;; Set the data seg to the bios
                        pop     bx                  ;; Restore BX back
                        
                        test    dl,80h                      ;; Test to see if current drive is HD
                        jnz     int13_HardDisk              ;; If so, do that, otherwise do floppy

                        call    _int13_diskette_function    ;; Call the diskette function
                        jmp     int13_out                   ;; Jump to exit code

int13_HardDisk:         call    _int13_harddisk             ;; Otherwise call the hardisk function
int13_out:                                                  ;; diskette jumps to here when done
                        pop     ds                  ;; Restore all registers
                        pop     es                  ;; Back in the reverse order pushed from above
                        pop     di                  ;;
                        pop     si                  ;;
                        pop     bp                  ;;
                        pop     bx                  ;;
                        pop     dx                  ;;
                        pop     cx                  ;;
                        pop     ax                  ;;
                        popf                        ;; Pop off flags, but iret wipes this out, unless...
                        
                        jc      int13_carry_set                 ;; Check the CF flag set
                        push    bp                              ;; Save base pointer registet
                        mov     bp, sp                          ;; Get stack pointer, then set the ZF flag
                        and     WORD PTR ss:[bp + 06h], 0fffeh  ;; while it is sitting on the stack
                        or      WORD PTR ss:[bp + 06h], 00200h  ;; Perftorms equivalent of STi on iret
                        pop     bp                              ;; The iret will popf 
                        iret                                    ;; return from interupt
int13_carry_set:        push    bp                              ;; Save the base pointer
                        mov     bp, sp                          ;; load it with the stack pointer
                        or      WORD PTR ss:[bp + 06h], 00201h  ;; locate the flags register on the stack
                        pop     bp                              ;; Restore the BP register
                        iret                                    ;; return from interupt

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT19h - INT 19h Boot Load Service Entry Point
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0e6f2h - startofrom) 
int19_handler:          push    bp                  ;; int19 was beginning to be really complex, so now it
                        mov     bp, sp              ;; just calls a C function that does the work
                        mov     ax, 0fffeh          ;; Reset SS and SP
                        mov     sp, ax              ;; Set Stack pointer to top o ram
                        xor     ax, ax              ;; Clear AX register
                        mov     ss, ax              ;; Reset Stack Segment
int19_next_boot:        call    _int19_function     ;; Call the C code for the next boot device
                        int     018h                ;; Boot failed: invoke the boot recovery function

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT14h - IBM entry point for Serial com. RS232 services
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0e729h - startofrom) 
baud_rates:             dw      0417h               ;  110 baud clock divisor
                        dw      0300h               ;  150 baud clock divisor
                        dw      0180h               ;  300 baud clock divisor
                        dw      00C0h               ;  600 baud clock divisor
                        dw      0060h               ; 1200 baud clock divisor
                        dw      0030h               ; 2400 baud clock divisor
                        dw      0018h               ; 4800 baud clock divisor
                        dw      000Ch               ; 9600 baud clock divisor
int14_handler:          sti                         ; Serial com. RS232 services
                        iret                                    ;; return from interupt

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT 16h Keyboard Service Entry Point -
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0e82eh - startofrom)
int16_handler:          sti                             ;; Enable interupts
                        push    ds                      ;; Save all registers that
                        push    dx                      ;; are not supposed to change
                        push    bx                      ;; on return
                        push    bp                      ;;
                        push    si                      ;;
                        push    di                      ;;
                        cmp     ah, 00h                 ;; Check to see what command
                        je      int16_F00               ;; was issued with the call
                        cmp     ah, 010h                ;; If either 0x00 or 0x10
                        je      int16_F00               ;; then it is a wait for key loop command
                        mov     bx, 0f000h              ;; Otherwise, just check for a key
                        mov     ds, bx                  ;; First set the data seg to the bios
                        pushf                           ;; Push the parms on the stack
                        push    cx                      ;; for the C program to receive
                        push    ax                      ;; Pass the user command
                        call    _int16_function         ;; Now call the function
                        pop     ax                      ;; Now restor the stack
                        pop     cx                      ;;
                        popf                            ;; Flags should have ZF set correctly
                        pop     di                      ;; Now restore all the saved
                        pop     si                      ;; registers from above
                        pop     bp                      ;; 
                        pop     bx                      ;;
                        pop     dx                      ;;
                        pop     ds                      ;;
                        
                        jz      int16_zero_set          ;; Check the ZF flag set
int16_zero_clear:       push    bp                              ;; This section of code
                        mov     bp, sp                          ;; Sets the ZF flag
                        and     BYTE PTR ss:[bp + 06h], 0bfh    ;; while it is sitting on the stack
                        pop     bp                              ;; The iret will popf 
                        iret                                    ;; return from interupt
int16_zero_set:         push    bp                              ;; Save the base pointer
                        mov     bp, sp                          ;; load it with the stack pointer
                        or      BYTE PTR ss:[bp + 06h], 040h    ;; locate the flags register on the stack
                        pop     bp                              ;; Restore the BP register
                        iret                                    ;; return from interupt

int16_F00:              mov     bx, 0040h               ;; Point to the correct data seg
                        mov     ds, bx                  ;; Set up the DS
int16_wait_for_key:     cli                             ;; Loop and Wait for the key
                        mov     bx, ds:001ah            ;; The head and tail of the buffer
                        cmp     bx, ds:001ch            ;; If they are not equal a key was put on the buffer
                        jne     int16_key_found         ;; Found the key, lets go process it
                        sti                             ;; Enable interupts agains
                        nop                             ;; No operation
                        jmp     int16_wait_for_key      ;; Continue looping until key is received
                        
int16_key_found:        mov     bx, 0f000h              ;; Otherwise, just check for a key
                        mov     ds, bx                  ;; First set the data seg to the bios
                        pushf                           ;; Push the parms on the stack
                        push    cx                      ;; for the C program to receive
                        push    ax                      ;; Pass the user command
                        call    _int16_function         ;; Now call the function
                        pop     ax                      ;; Now restor the stack
                        pop     cx                      ;;
                        popf                            ;; Flags should have ZF set correctly

                        pop     di                      ;; Now restore all the saved
                        pop     si                      ;; registers from above
                        pop     bp                      ;; 
                        pop     bx                      ;;
                        pop     dx                      ;;
                        pop     ds                      ;;
                        iret                            ;; return from interupt

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT09h : Keyboard Hardware Service Entry Point -
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0e987h - startofrom)
int09_handler:          cli                         ;; Clear interupt enable flag
                        push    ax                  ;; Save the AX register      
                        
                        in      al, 060h            ;; read key from keyboard controller
                        sti                         ;; Enable interupts again
                        
                        push    ds                  ;; next pushes 6 equivalent of pusha
                        push    cx                  ;; Save all register contents
                        push    dx                  ;; Onto the stack
                        push    bx                  ;;
                        push    bp                  ;;
                        push    si                  ;;
                        push    di                  ;;
                        
                        cmp     al, 0e0h            ;; check for extended key
                        jne     int09_check_pause   ;; check if the pause key pressed
                        
                        xor     ax, ax                      ;; Clear the ax register
                        mov     ds, ax                      ;; Load the data segment reg with 0
                        mov     al, BYTE PTR ds:[0496h]     ;; mf2_state |= 0x02
                        or      al, 002h                    ;; set bit 2
                        mov     BYTE PTR ds:[0496h], al     ;; Store in correct key buf location
                        jmp     int09_done                  ;; Leave this routine

int09_check_pause:      cmp     al, 0e1h                    ;; check for pause key
                        jne     int09_process_key           ;; Pause was not pressed
                        
                        xor     ax, ax                      ;; Pause was pressed
                        mov     ds, ax                      ;; Load the data segment reg with 0
                        mov     al, BYTE PTR ds:[0496h]     ;; mf2_state |= 0x01
                        or      al, 001h                    ;; Set bit 1
                        mov     BYTE PTR ds:[0496h], al     ;; Store in correct key buf location
                        jmp     int09_done                  ;; Leave this routine

int09_process_key:      mov     bx, 0f000h                  ;; Load the Data seg register with
                        mov     ds, bx                      ;; The bios segment location
                        push    ax                          ;; Push AX onto stack to pass that parameter 
                        call    _int09_function             ;; To the C program on the stack
                        pop     ax                          ;; Restore the stack
                            
int09_done:             pop     di                  ;; Retore all the saved registers
                        pop     si                  ;; That were saved on entry
                        pop     bp                  ;;
                        pop     bx                  ;;
                        pop     dx                  ;;
                        pop     cx                  ;;
                        pop     ds                  ;;
                        pop     ax                  ;;  
;;                      cli                         ;; Clear interupt enable flag
                        iret                        ;; Return from interupt

;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; Disk interrupt entry
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0ef57h - startofrom)
int0E_handler:          sti                    ; Floppy disk attention
                        push    ds
                        push    ax
                        mov     ax,40h
                        MOV     ds, ax
                        or      Byte ptr DS:3Eh,10000000b       ; Raise "attention" flag
                        mov     al, 20h                          ; Send end_of_interrupt code
                        out     20h, al                          ;  ...to 8259 interrupt chip
                        pop     ax
                        pop     ds
                        iret                        ;; Return from interupt

;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;  Diskette parameter table adding 3 parameters from IBM Since no
;;  provisions are made for multiple drive types, most values in this
;;  table are ignored. set parameters for 1.44M floppy here                  
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0efc7h - startofrom)  ; IBM entry for disk param
int1E_table:            db      0xAF                   ; Disk parameter table
                        db      0x02    ;; head load time 0000001, DMA used 
                        db      0x25
                        db      0x02
                        db      18
                        db      0x1B
                        db      0xFF
                        db      0x6C
                        db      0xF6
                        db      0x0F
                        db      0x08
                        db      79      ;; maximum track      
                        db      0       ;; data transfer rate 
                        db      4       ;; drive type in cmos                         
                        
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; INT17 -  IBM entry for parallel LPT
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0efd2h - startofrom)     
int17_handler:          sti      ; Parallel printer services
                        iret
                                                
;;--------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;; - INT10h - Video Support Service Entry Point
;;--------------------------------------------------------------------------
;;---------------------------------------------------------------------------
                        org     (0f065h - startofrom)    
int10_handler:                  ; dont do anything, since 
                        iret    ; the VGA BIOS handles int10h requests


;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; MDA/CGA Video Parameter Table (INT 1Dh)
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0f0a4h - startofrom)    
int1d_Table:            db      38h,28h,2Dh,0Ah,1Fh,6,19h ; Init string for 40 x 25
                        db      1Ch,2,7,6,7
                        db      0,0,0,0
                        db      71h,50h,5Ah,0Ah,1Fh,6,19h       ; Init string for 80 x 25 col
                        db      1Ch,2,7,6,7
                        db      0,0,0,0
                        db      38h,28h,2Dh,0Ah,7Fh,6,64h       ; Init string for GRAPHIX
                        db      70h,2,1,6,7
                        db      0,0,0,0
                        db      61h,50h,52h,0Fh,19h,6,19h       ; Init string for 80 x 25 b/w
                        db      19h,2,0Dh,0Bh,0Ch
                        db      0,0,0,0
REGENL:                 dw      0800h                           ; Regen len, 40 x 25
                        dw      1000h                           ;            80 x 25
                        dw      4000h                           ;            GRAPHIX
                        dw      4000h
MAXCOL:                 db      28h,28h,50h,50h,28h,28h,50h,50h ; Maximum columns
MODES:                  db      2Ch,28h,2Dh,29h,2Ah,2Eh,1Eh,29h ; Table of mode sets
TABMUL:                 db      00h,00h,10h,10h,20h,20h,20h,30h ; Table lookup for multiply
                        ret                     ;  ...error  return if not

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT12h - ; INT 12h Memory Size Service Entry Point
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0f841h - startofrom)   ; ??? different for Pentium (machine check)?
int12_handler:          push    ds
                        mov     ax, 0040h
                        mov     ds, ax
                        mov     ax, WORD PTR ds:[0013h]
                        pop     ds
                        iret

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;;- INT11h -  INT 11h Equipment List Service Entry Point
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0f84dh - startofrom)
int11_handler:          push    ds
                        mov     ax, 0040h
                        mov     ds, ax
                        mov     ax, WORD PTR ds:[0010h]
                        pop     ds
                        iret

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
; Interrupt 15h - Cassette
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0f859h - startofrom)
int15_handler:          stc                  ; Cassette service (error ret)
                        mov     ah, 086h
                        retf    2
                        
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT1Ah - INT 1Ah Time-of-day Service Entry Point
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0fe6eh - startofrom)    
int1a_handler:          
                        push    ds                      ;; Save all registers that
                        push    bx                      ;; on return
                        push    bp                      ;;
                        push    si                      ;;
                        push    di                      ;;
                        
                        mov     ax, 0f000h              ;; Bios data segment
                        mov     ds, ax                  ;; set the data seg to the bios                        
                        pushf                           ;; Push the parms on the stack
                        push    dx                      ;; for the C program to receive
                        push    cx                      ;; for the C program to receive
                        push    ax                      ;; Pass the user command
                        call    _int1a_function         ;; Now call the function
                        pop     ax                      ;; Now restor the stack
                        pop     cx                      ;;
                        pop     dx                      ;;
                        popf                            ;; Flags should have ZF set correctly
                        
                        pop     di                      ;; Now restore all the saved
                        pop     si                      ;; registers from above
                        pop     bp                      ;; The next section insures that this
                        pop     bx                      ;; routines returns with the Zero Flag
                        pop     ds                      ;; Set correctly

                        jc      int1a_carry_set                 ;; Check the CF flag set
                        push    bp                              ;; Save base pointer registet
                        mov     bp, sp                          ;; Get stack pointer, then set the CF flag
                        and     BYTE PTR ss:[bp + 06h], 0feh    ;; while it is sitting on the stack
                        pop     bp                              ;; The iret will popf 
                        iret                                    ;; return from interupt
int1a_carry_set:        push    bp                              ;; Save the base pointer
                        mov     bp, sp                          ;; load it with the stack pointer
                        or      BYTE PTR ss:[bp + 06h], 001h    ;; locate the flags register on the stack
                        pop     bp                              ;; Restore the BP register
                        iret                                    ;; return from interupt


;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; - INT08 -  System Timer ISR Entry Point
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0fea5h - startofrom)     
int08_handler:          sti
                        push    ax
                        push    bx
                        push    ds
                        xor     ax, ax
                        mov     ds, ax
                        mov     ax, WORD PTR ds:046ch    ;; get ticks dword
                        mov     bx, WORD PTR ds:046eh    ;; get ticks dword
                        inc     ax
                        jne     i08_linc_done
                        inc     bx            ;; inc high word
i08_linc_done:          push    bx 
                        sub     bx, 0018h ;; compare eax to one days worth of timer ticks at 18.2 hz
                        jne     i08_lcmp_done
                        cmp     ax, 00B0h
                        jb      i08_lcmp_b_and_lt
                        jge     i08_lcmp_done
                        inc     bx
                        jmp     i08_lcmp_done
i08_lcmp_b_and_lt:      dec     bx
i08_lcmp_done:          pop     bx
                        jb      int08_store_ticks    ;; there has been a midnight rollover at this point
                        xor     ax, ax               ;; zero out counter
                        xor     bx, bx
                        inc     BYTE PTR ds:0470h     ;; increment rollover flag
int08_store_ticks:      mov     WORD PTR ds:046ch, ax           ;; store new ticks dword
                        mov     WORD PTR ds:046eh, bx
                        int     01ch
                        cli
                        pop     ds        ;; call eoi_master_pic
                        pop     bx
                        pop     ax
                        iret

;;--------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;; Interrupt Vector -  Vector table
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0fef3h - startofrom) 
Vectors:                dw      int08_handler       ; Timer tick
                        dw      int09_handler       ; Key attention
                        dw      ignore_handler      ; Reserved
                        dw      ignore_handler      ; Reserved for COM2 serial i/o
                        dw      ignore_handler      ; Reserved for COM1 serial i/o
                        dw      ignore_handler      ; Reserved for hard disk attn.
                        dw      int0E_handler       ; Floppy disk attention
                        dw      ignore_handler      ; Reserved for parallel printer
                        dw      int10_handler       ; Video bios services
                        dw      int11_handler       ; Equipment present
                        dw      int12_handler       ; Memories  present
                        dw      int13_handler       ; Disk bios services
                        dw      int14_handler       ; Serial com. services
                        dw      int15_handler       ; Cassette bios services
                        dw      int16_handler       ; Keyboard bios services
                        dw      int17_handler       ; Parallel printer services
                        dw      ignore_handler      ; rom Basic (setup later)
                        dw      int19_handler       ; Bootstrap
                        dw      int1a_handler       ; Timer bios services
                        dw      dummy_iret_handler  ; Keyboard break user service
                        dw      dummy_iret_handler  ; System tick user service
                        dw      int1d_Table         ; Video parameter table
                        dw      int1E_table         ; Disk  parameter table
                        dw      0                   ; Graphic charactr table ptr


;;--------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;; Interrupt Vector -  IBM entry, nonsense interrupt
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0ff23h - startofrom) 
ignore_handler:         push    ds               ; Unexpected interrupts go here
                        push    ax
                        mov     ax, 040h
                        mov     ds, ax
                        mov     ah, 00h          ; What IRQ caused this?
                        mov     ds:006Bh, ah     ; Save last nonsense interrupt
                        pop     ax
                        pop     ds
                        iret    ;; IRET Instruction for Interrupt Handler

;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; IRET Instruction for Dummy Interrupt Handler -
;; Also INT1Ch - User Timer Tick and INT1B
;;--------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0ff53h - startofrom)
int1c_handler:                                  ; Stub for later
int1b_handler:                                  ; Stub for later
dummy_iret_handler:     iret    ;; IRET Instruction for Dummy Interrupt Handler

;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
;; MAIN BIOS Entry Point:  
;; on Reset - Processor starts at this location. This is the first instruction
;; that gets executed on start up. So we just immediately jump to the entry
;; Point for the Bios which is the POST (which stands for Power On Self Test).
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
                        org     (0fff0h - startofrom)          ;; Power-up Entry Point
                        jmp     far ptr post

;;--------------------------------------------------------------------------
;;---------------------------------------------------------------------------
;; BIOS Strings
;;---------------------------------------------------------------------------
;;--------------------------------------------------------------------------
BIOS_COPYRIGHT_STRING   equ     "(c) 2009, 2010 Zeus Gomez Marmolejo and (c) 2002 MandrakeSoft S.A."
BIOS_BUILD_DATE         equ     "04/5/10\n"
                                org     (0ff00h - startofrom)
MSG1:                   db      BIOS_COPYRIGHT_STRING

;;---------------------------------------------------------------------------
                                org     (0fff5h - startofrom)     ;; ASCII Date ROM was built - 8 characters in MM/DD/YY
MSG2:                   db      BIOS_BUILD_DATE
                                org     (0fffeh -startofrom)    ;; Put the 
SYS_MODEL_ID                    equ     0FCh                    ;; System Model ID 
                                db      SYS_MODEL_ID            ;; here
                                db      0

;;--------------------------------------------------------------------------
_BIOSSEG                ends
                        end             biosrom
;;---------------------------------------------------------------------------


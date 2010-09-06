
;;--------------------------------------------------------------------------
shadowcopy:             mov     dx, SPIFLASH_PORT       ;; Set DX reg to SPI FLASH IO port
                        mov     ax, VGABIOSSEGMENT      ;; Load with the segment of the vga bios rom area
                        mov     es, ax                  ;; BIOS area segment
                        xor     bx, bx                  ;; Bios starts at offset address 0
                        mov     cx, VGABIOSLENGTH       ;; VGA Bios is <32K long
                        mov     ax, 0x0003              ;; Starting Read Commad and /CS
                        out     dx, ax                  ;; brings /CS low and loads MSB address byte
                        xor     al, al                  ;; MSB address byte is 0x00
                        out     dx, al                  ;; Load MSB address byte
                        out     dx, al                  ;; Load 2nd address byte
                        out     dx, al                  ;; Load LSB address byte
vgabios1:               in      al, dx                  ;; Get input byte into al register
                        mov     byte ptr es:[bx], al    ;; Save that byte to next place in RAM
                        inc     bx                      ;; Increment to next address location
                        loop    vgabios1                ;; Loop until vga bios is loaded up
                        mov     ax, 0FFFFh              ;; NOP plus make /CS high
                        out     dx, ax                  ;; brings /CS low and loads MSB address byte
                        
;;--------------------------------------------------------------------------
                        mov     ax, ROMBIOSSEGMENT      ;; Load with the segment of the extra bios rom area
                        mov     es, ax                  ;; BIOS area segment
                        xor     bx, bx                  ;; Bios starts at offset address 0
                        mov     cx, ROMBIOSLENGTH       ;; Bios is 64K long - Showdow rom
                        mov     ax, 0x0003              ;; Starting Read Commad and /CS
                        out     dx, ax                  ;; brings /CS low and loads MSB address byte
                        mov     ax, 0x01                ;; MSB address byte is 0x01
                        out     dx, al                  ;; Load MSB address byte
                        xor     al, al                  ;; low bytes of flash address are 0x00
                        out     dx, al                  ;; and loads 2nd address byte
                        out     dx, al                  ;; and loads 3rd address byte
extrabios1:             in      al, dx                  ;; Get input byte into al register
                        mov     byte ptr es:[bx], al    ;; Save that byte to next place in RAM
                        inc     bx                      ;; Increment to next
                        loop    extrabios1              ;; Loop until vga bios is loaded up
                        mov     ax, 0FFFFh              ;; NOP plus make /CS high
                        out     dx, ax                  ;; brings /CS low and loads MSB address byte
;;--------------------------------------------------------------------------


;;-----------------------------------------------------------------------
;; Send 1 byte
;;-----------------------------------------------------------------------
sendbyte:               push    ax
                        push    cx
                        push    dx
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx:                in      al, dx
                        test    al, 0x40    
                        jz      wait_tx
                        mov     cx, 256
delay:                  nop
                        loop    delay
                        pop     dx
                        pop     cx
                        pop     ax
                        ret                        
;;-----------------------------------------------------------------------
    
;;-----------------------------------------------------------------------
;;    BIOS Check
;;-----------------------------------------------------------------------
bioscheck:              xor     bx, bx
tx_dram:                mov     al, byte ptr es:bx
                        call    sendbyte
                        inc     bx 
                        cmp     bx, 0x0200
                        jb      tx_dram                        
                        ret                        
;;-----------------------------------------------------------------------
 

                    
;;-----------------------------------------------------------------------
                        mov     ax, VGABIOSSEGMENT      ;; Load with the segment of the vga bios rom area
                        mov     es, ax                  ;; BIOS area segment
                        call    bioscheck
;;-----------------------------------------------------------------------
  

;;-----------------------------------------------------------------------
;; delay a little bit of time
;;-----------------------------------------------------------------------
delay:                  push    cx
                        mov     cx, 500
delay1:                 nop
                        loop    delay1
                        pop     cx
                        ret    
                
;;-----------------------------------------------------------------------
;; Send 1 byte
;;-----------------------------------------------------------------------
sendbyte:               push    ax
                        push    dx
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx:                in      al, dx
                        test    al, 0x40    
                        jz      wait_tx
                        call    delay
                        pop     dx
                        pop     ax
                        ret                        
;;-----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;;    Memory Check
;;-----------------------------------------------------------------------
memcheck:               mov     ax, VGABIOSSEGMENT
                        mov     es, ax   
                        xor     bx, bx
                        xor     al, al 
                        mov     cx, 0x200
a_count1:               mov     byte ptr es:[bx], al 
                        inc     bx                   
                        inc     al
                        loop    a_count1        

                        mov     cx, 1000
long_delay:             call    delay
                        loop    long_delay

                        xor     bx, bx
                        mov     cx, 0x200
a_count2:               mov     al, byte ptr es:[bx]
                        call    sendbyte
                        inc     bx                   
                        loop    a_count2
                        ret                        
;;-----------------------------------------------------------------------
   


;;-----------------------------------------------------------------------
;; Send 1 byte
;;-----------------------------------------------------------------------
sendbyte:               push    ax
                        push    cx
                        push    dx
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx:                in      al, dx
                        test    al, 0x40    
                        jz      wait_tx
                        mov     cx, 512
delay:                  nop
                        loop    delay
                        pop     dx
                        pop     cx
                        pop     ax
                        ret                        
;;-----------------------------------------------------------------------
    
;;-----------------------------------------------------------------------
;;    BIOS Check
;;-----------------------------------------------------------------------
bioscheck:              xor     bx, bx
tx_dram:                mov     al, byte ptr es:bx
                        call    sendbyte
                        inc     bx 
                        cmp     bx, 0x0200
                        jb      tx_dram                        
                        ret                        
;;-----------------------------------------------------------------------
 

                    


    ;;-----------------------------------------------------------------------
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        mov     al, byte ptr es:[bx]
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx2:               in      al, dx
                        test    al, 0x40    
                        jz      wait_tx2

                        push    cx
                        mov     cx, 256
delay3:                 nop
                        loop    delay3
                        pop     cx

                        mov     dx, SPIFLASH_PORT       ;; Set DX reg to SPI FLASH IO port
    ;;-----------------------------------------------------------------------
 
                        

;;-----------------------------------------------------------------------
;;    BIOS Check
;;-----------------------------------------------------------------------
bioscheck:              mov     ax, VGABIOSSEGMENT
                        mov     es, ax   
                        xor     bx, bx
tx_dram:                mov     al, byte ptr es:[bx]
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx:                in      al, dx
                        test    al, 0x40    
                        jz      wait_tx

                        mov     cx, 256
delay:                  nop
                        loop    delay
                       
                        inc     bx
                        
                        cmp     bx, 0x1000
                        jb      tx_dram                        
                        ret                        
             
                        

                        
;;-----------------------------------------------------------------------
;;    Memory Check
;;-----------------------------------------------------------------------
memcheck:       mov     ax, VGABIOSSEGMENT
                mov     es, ax   
                xor     bx, bx
                xor     al, al            
                mov     cx, 256
a_count1:       mov     byte ptr es:[bx], al 
                inc     bx                   
                inc     al
                loop    a_count1        

                mov     cx, 10000
delay1:         call    delay
                loop    delay1

                xor     bx, bx
a_count2:       mov     al, byte ptr es:[bx]
                mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                out     dx, al
                mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx1:       in      al, dx
                test    al, 0x40    
                jz      wait_tx1
                call    delay
                inc     bx                   


                cmp     bx, 256
                jb      a_count2
                ret                        

delay:          push    cx
                mov     cx, 256
delay2:         nop
                loop    delay2
                pop     cx
                ret                        
                        
 
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        

    ;;-----------------------------------------------------------------------
    ;;-----------------------------------------------------------------------
;                        mov     dx, 0f102h
;                        mov     al, 0x03
;                        out     dx, al
;                        mov     ax, VGABIOSSEGMENT      ;; Load with the segment of the vga bios rom area
;                        mov     es, ax                  ;; BIOS area segment
;                        mov     bx, 0x01                  ;; Bios starts at offset address 0
;                        mov     al, byte ptr es:[bx]
;                        cmp     al, 0xAA 
;                        jne     testdram1
;                        mov     al, 0x01
;                        out     dx, al
;testdram1:
    ;;-----------------------------------------------------------------------
    ;;-----------------------------------------------------------------------





    ;;-----------------------------------------------------------------------
    ;; memory check
    ;;-----------------------------------------------------------------------
;                        mov     dx, 0f102h
;                        mov     al, 0x03
;                        out     dx, al
;                        mov     ax, VGABIOSSEGMENT
;                        mov     es, ax   
;                        xor     bx, bx
;                        xor     al, al            
;                        mov     cx, 64
;a_count1:               mov     byte ptr es:[bx], al 
;                        inc     bx                   
;                        inc     al
;                        loop    a_count1        
;                        xor     bx, bx
;                        xor     al, al            
;                        mov     cx, 64
;a_count2:               cmp     al, byte ptr es:[bx]
;                        jne     testdram1
;                        inc     bx                   
;                        inc     al
;                        loop    a_count2
;                        mov     al, 0x01
;                        out     dx, al
;testdram1:
;                        jmp     testdram1
    ;;-----------------------------------------------------------------------
    ;;-----------------------------------------------------------------------




    
;;-----------------------------------------------------------------------
                        mov     ax, VGABIOSSEGMENT      ;; Load with the segment of the vga bios rom area
                        mov     es, ax                  ;; BIOS area segment
                        xor     bx, bx                  ;; Bios starts at offset address 0
                        mov     cx, VGABIOSLENGTH       ;; VGA Bios is <32K long

tx_dram:                mov     al, byte ptr es:[bx]
                        mov     dx, 0x03F8              ;; Set DX reg to RS232 IO port
                        out     dx, al
                        mov     dx, 0x03FD              ;; Set DX reg to RS232 IO port
wait_tx:                in      al, dx
                        test    al, 0x40    
                        je      wait_tx
                        
                        inc     bx
                        loop    tx_dram

;;-----------------------------------------------------------------------
    

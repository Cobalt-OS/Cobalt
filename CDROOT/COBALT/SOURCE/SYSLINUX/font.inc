;; $Id: font.inc,v 1.3 2004/02/03 06:16:03 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 1994-2004 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; font.inc
;;
;; VGA font handling code
;;

;
; loadfont:	Load a .psf font file and install it onto the VGA console
;		(if we're not on a VGA screen then ignore.)  It is called with
;		SI and DX:AX set by routine searchdir
;
loadfont:
		mov bx,trackbuf			; The trackbuf is >= 16K; the part
		mov cx,[BufSafe]		; of a PSF file we care about is no
		call getfssec			; more than 8K+4 bytes

		mov ax,[trackbuf]		; Magic number
		cmp ax,0436h
		jne lf_ret

		mov al,[trackbuf+2]		; File mode
		cmp al,5			; Font modes 0-5 supported
		ja lf_ret

		mov bh,byte [trackbuf+3]	; Height of font
		cmp bh,2			; VGA minimum
		jb lf_ret
		cmp bh,32			; VGA maximum
		ja lf_ret

		; Copy to font buffer
		mov si,trackbuf+4		; Start of font data
		mov [VGAFontSize],bh
		mov di,vgafontbuf
		mov cx,(32*256) >> 2		; Maximum size
		rep movsd

		mov [UserFont], byte 1		; Set font flag

		; Fall through to use_font

;
; use_font:
; 	This routine activates whatever font happens to be in the
;	vgafontbuf, and updates the adjust_screen data.
;       Must be called with CS = DS = ES
;
use_font:
		test [UserFont], byte 1		; Are we using a user-specified font?
		jz adjust_screen		; If not, just do the normal stuff

		mov bp,vgafontbuf
		mov bh,[VGAFontSize]

		xor bl,bl			; Needed by both INT 10h calls
		cmp [UsingVGA], byte 1		; Are we in graphics mode?
		jne .text

.graphics:
		xor cx,cx
		mov cl,bh			; CX = bytes/character
		mov ax,480
		div cl				; Compute char rows per screen
		mov dl,al
		dec al
		mov [VidRows],al
		mov ax,1121h			; Set user character table
		int 10h
		mov [VidCols], byte 79		; Always 80 bytes/line
.lf_ret:	ret				; No need to call adjust_screen

.text:
		mov cx,256
		xor dx,dx
		mov ax,1110h
		int 10h				; Load into VGA RAM

		xor bl,bl
		mov ax,1103h			; Select page 0
		int 10h

		; Fall through to adjust_screen

lf_ret		equ use_font.lf_ret

;
; adjust_screen: Set the internal variables associated with the screen size.
;		This is a subroutine in case we're loading a custom font.
;
adjust_screen:
		pusha
                mov al,[BIOS_vidrows]
                and al,al
                jnz vidrows_ok
                mov al,24                       ; No vidrows in BIOS, assume 25
						; (Remember: vidrows == rows-1)
vidrows_ok:	mov [VidRows],al
                mov ah,0fh
                int 10h                         ; Read video state
                dec ah                          ; Store count-1 (same as rows)
                mov [VidCols],ah
		popa
		ret


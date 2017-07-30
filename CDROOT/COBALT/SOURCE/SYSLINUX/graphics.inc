;; $Id: graphics.inc,v 1.2 2002/06/10 18:32:42 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 1994-2002 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

; ----------------------------------------------------------------------------
;  VGA splash screen code
; ----------------------------------------------------------------------------

;
; vgadisplayfile:
;	Display a graphical splash screen.
;
; Input:
;
; SI	= cluster/socket pointer
;
vgadisplayfile:
		mov [VGACluster],si
		push es

		; This is a cheap and easy way to make sure the screen is
		; cleared in case we were in graphics mode already
		call vgaclearmode
		call vgasetmode
		jnz .error_nz

.graphalready:
		mov ax,xfer_buf_seg		; Use as temporary storage
		mov es,ax
		mov fs,ax

		call vgagetchunk		; Get the first chunk

		; The header WILL be in the first chunk.
		cmp dword [es:xbs_vgabuf],0x1413f33d	; Magic number
.error_nz:	jne .error
		mov ax,[es:xbs_vgabuf+4]
		mov [GraphXSize],ax

		mov dx,xbs_vgabuf+8		; Color map offset
		mov ax,1012h			; Set RGB registers
		xor bx,bx			; First register number
		mov cx,16			; 16 registers
		int 10h
	
.movecursor:
		mov ax,[es:xbs_vgabuf+6]	; Number of pixel rows
		mov dx,[VGAFontSize]
		add ax,dx
		dec ax
		div dl
		xor dx,dx			; Set column to 0
		cmp al,[VidRows]
		jb .rowsok
		mov al,[VidRows]
		dec al
.rowsok:
		mov dh,al
		mov ah,2
		xor bx,bx
		int 10h				; Set cursor below image

		mov cx,[es:xbs_vgabuf+6]	; Number of graphics rows

		mov si,xbs_vgabuf+8+3*16	; Beginning of pixel data
		mov word [VGAPos],0

.drawpixelrow:
		push cx
		mov cx,[GraphXSize]
		mov di,xbs_vgatmpbuf		; Row buffer
		call rledecode			; Decode one row
		push si
		mov si,xbs_vgatmpbuf
		mov di,si
		add di,[GraphXSize]
		mov cx,640/4
		xor eax,eax
		rep stosd			; Clear rest of row
		mov di,0A000h			; VGA segment
		mov es,di
		mov di,[VGAPos]
		mov bp,640
		call packedpixel2vga
		add word [VGAPos],byte 80	; Advance to next pixel row
		push fs
		pop es
		pop si
		pop cx
		loop .drawpixelrow

.error:
		pop es
		ret

;
; rledecode:
;	Decode a pixel row in RLE16 format.
;
; FS:SI	-> input
; CX -> pixel count
; ES:DI -> output (packed pixel)
;
rledecode:
		shl esi,1		; Nybble pointer
		xor dl,dl		; Last pixel
.loop:
		call .getnybble
		cmp al,dl
		je .run			; Start of run sequence
		stosb
		mov dl,al
		dec cx
		jnz .loop
.done:
		shr esi,1
		adc si,byte 0
		ret
.run:
		xor bx,bx
		call .getnybble
		and al,al
		jz .longrun
		mov bl,al
.dorun:
		push cx
		mov cx,bx
		mov al,dl
		rep stosb
		pop cx
		sub cx,bx
		ja .loop
		jmp short .done
.longrun:
		call .getnybble
		mov ah,al
		call .getnybble
		shl al,4
		or al,ah
		mov bl,al
		add bx,16
		jmp short .dorun
.getnybble:
		shr esi,1
		fs lodsb
		jc .high
		dec si
		and al,0Fh
		stc
		rcl esi,1
		ret
.high:
		shr al,4
		cmp si,xbs_vgabuf+trackbufsize	; Chunk overrun
		jb .nonewchunk
		call vgagetchunk
		mov si,xbs_vgabuf		; Start at beginning of buffer
.nonewchunk:
		shl esi,1
		ret

;
; vgagetchunk:
;	Get a new trackbufsize chunk of VGA image data
;
; On input, ES is assumed to point to the buffer segment.
;
vgagetchunk:
		pushad
		mov si,[VGACluster]
		and si,si
		jz .eof				; EOF overrun, not much to do...

		mov cx,[BufSafe]		; One trackbuf worth of data
		mov bx,xbs_vgabuf
		call getfssec

		jnc .noteof
		xor si,si
.noteof:	mov [VGACluster],si

.eof:		popad
		ret

;
; packedpixel2vga:
;	Convert packed-pixel to VGA bitplanes
;
; FS:SI -> packed pixel string
; BP    -> pixel count (multiple of 8)
; ES:DI -> output
;
packedpixel2vga:
		mov dx,3C4h	; VGA Sequencer Register select port
		mov al,2	; Sequencer mask
		out dx,al	; Select the sequencer mask
		inc dx		; VGA Sequencer Register data port
		mov al,1
		mov bl,al
.planeloop:
		pusha
		out dx,al
.loop1:
		mov cx,8
.loop2:
		xchg cx,bx
		fs lodsb
		shr al,cl
		rcl ch,1	; VGA is bigendian.  Sigh.
		xchg cx,bx
		loop .loop2
		mov al,bh
		stosb
		sub bp,byte 8
		ja .loop1
		popa
		inc bl
		shl al,1
		cmp bl,4
		jbe .planeloop
		ret

;
; vgasetmode:
;	Enable VGA graphics, if possible; return ZF=1 on success
;	DS must be set to the base segment; ES is set to DS.
;
vgasetmode:
		push ds
		pop es
		mov ax,1A00h		; Get video card and monitor
		xor bx,bx
		int 10h
		sub bl, 7		; BL=07h and BL=08h OK
		cmp bl, 1
		ja .error		; ZF=0
;		mov bx,TextColorReg
;		mov dx,1009h		; Read color registers
;		int 10h
		mov ax,0012h		; Set mode = 640x480 VGA 16 colors
		int 10h
		mov dx,linear_color
		mov ax,1002h		; Write color registers
		int 10h
		mov [UsingVGA], byte 1

		call use_font		; Set graphics font/data
		mov byte [ScrollAttribute], 00h

		xor ax,ax		; Set ZF
.error:
		ret

;
; vgaclearmode:
;	Disable VGA graphics.  It is not safe to assume any value
;	for DS or ES.
;
vgaclearmode:
		push ds
		push es
		pushad
		mov ax,cs
		mov ds,ax
		mov es,ax
		cmp [UsingVGA], byte 1
		jne .done
		mov ax,0003h		; Return to normal video mode
		int 10h
;		mov dx,TextColorReg	; Restore color registers
;		mov ax,1002h
;		int 10h
		mov [UsingVGA], byte 0

		call use_font		; Restore text font/data
		mov byte [ScrollAttribute], 07h
.done:
		popad
		pop es
		pop ds
		ret

;
; vgashowcursor/vgahidecursor:
;	If VGA graphics is enabled, draw a cursor/clear a cursor
;
vgashowcursor:
		pushad
		mov al,'_'
		jmp short vgacursorcommon
vgahidecursor:
		pushad
		mov al,' '
vgacursorcommon:
		cmp [UsingVGA], byte 1
		jne .done
		mov ah,09h
		mov bx,0007h
		mov cx,1
		int 10h
.done:
		popad
		ret


		; Map colors to consecutive DAC registers
linear_color	db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0
UsingVGA	db 0


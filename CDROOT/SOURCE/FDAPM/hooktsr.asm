; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2003-2007.
; FDAPM is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published
; by the Free Software Foundation; either version 2 of the License,
; or (at your option) any later version.
; ### FDAPM is distributed in the hope that it will be useful, but
; ### WITHOUT ANY WARRANTY; without even the implied warranty of
; ### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; ### GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with FDAPM; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
; (or try http://www.gnu.org/licenses/ at http://www.gnu.org).

; This file: hookhandlers - call to hook interrupt handlers.
; Returns CY set if POWER is already resident.
; To be used together with handlers.asm, of course. Like
; handlers, this is influenced by the UNIDLECHECK define.
; FUTURE extension: removetsr - call to remove existing TSR

removetsr:
	; *** ***
	; maybe add some "unhook / unload TSR" option here?
	; *** ***
	stc
	ret

; ---------------

hookhandlers:
	mov ax,5400h
	xor bx,bx
	int 2fh	; POWER install check
	cmp bx,504dh
	jnz goingTSR
	stc
	ret	; already TSR

goingTSR:
	push ds	; initialize statistics / timestamps
	mov ax,40h
	mov ds,ax
	mov ax,[ds:6ch]		; low word of timer tick
	pop ds
	mov [cs:lastIdleTick],ax	; avoid duplicate counts
	mov [cs:lastOnTick],ax		; not checked every tick
	mov byte [cs:onTicks],1	; make sure that this dd is not 0

	call findVga		; no VESA if not at least VGA
	jc noBochs
	push es
	mov di,cs
	mov es,di
	mov di,acpistuff	; offset: we can overwrite all ACPI stuff
	add di,4
	and di,0xfffc		; 512 byte buffer at es:di
	mov word [es:di],'V'+(0x100*'B')
	mov word [es:di+2],'E'+(0x100*'2')
	mov ax,0x4f00		; test for VESA
	int 0x10
	pop es
	cmp ax,0x4f		; okay?
	jnz noBochs
	push es
	les di,[cs:di+0x06]	; OEM name string pointer
	mov ax,[es:di+2]
	cmp word [es:di],'B'+(0x100*'o')
	pop es
	jnz noBochs
	cmp ax,'c'+(0x100*'h')
	jnz noBochs
isBochs:
	mov ah,9
	mov dx,bochsmsg
	int 0x21	; show warning string
	and byte [cs:savingstrat],0xfd	; "and not 2", disable APM idling
	and byte [cs:maxSavingstrat],0xfd	; disallow APM idling
noBochs:

	push es
	mov es,[cs:2ch]	; environment segment
			; see Ralf Browns IntList tables 1378-1379
	mov ah,49h
	int 0x21	; free the environment! (saves some RAM)
	pop es		; (jc "could not free... bla")

	cld
	mov si,hooktable
hookloop:
	lodsb	; int number
	or al,al
	jz hookdone
	mov dx,ax
	push es
	mov ah,35h
	int 21h	; get vector to ES:BX
	mov cx,es
	pop es
	lodsw	; storage pointer
	mov di,ax
	mov ax,bx
	stosw	; store old vector, offset
	mov ax,cx
	stosw	; store old vector, segment
	mov ax,dx
	mov ah,25h
	push ax
	lodsw	; handler offset
	mov dx,ax
	pop ax
	int 21h	; set vector to DS:DX
	jmp short hookloop
hookdone:
	clc
	ret

hooktable:
	db 16h
	dw oldi16,i16
	db 28h
	dw oldi28,i28
	db 2ah
	dw oldi2a,i2a
	db 2fh
	dw oldi2f,i2f
%ifdef UNIDLECHECK
	db 08h
	dw oldi08,i08
	db 10h
	dw oldi10,i10
	db 13h
	dw oldi13,i13
	db 14h
	dw oldi14,i14
	db 17h
	dw oldi17,i17
	db 21h
	dw oldi21,i21
%endif
	db 0

bochsmsg	db "VESA VBE OEM is 'Bochs' - APM CPU idle disabled."
		db 13,10,"$"	; avoid Bochs 2.3 "panic on HLT in BIOS"

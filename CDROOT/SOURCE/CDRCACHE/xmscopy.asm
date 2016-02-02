 ; This file is part of CDRcache, the 386/XMS DOS CD-ROM cache by
 ; Eric Auer (eric@coli.uni-sb.de), based on LBAcache, 2001-2003.

 ; CDRcache is free software; you can redistribute it and/or modify
 ; it under the terms of the GNU General Public License as published
 ; by the Free Software Foundation; either version 2 of the License,
 ; or (at your option) any later version.

 ; CDRcache is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.

 ; You should have received a copy of the GNU General Public License
 ; along with CDRcache; if not, write to the Free Software Foundation,
 ; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 ; (or try http://www.gnu.org/licenses/licenses.html at www.gnu.org).

 ; -------------------------------------------------------------------


	; XMS helper functions
	; copytoxms copies one sector from es:bx to XMS slot AX
	; copytodos copies one sector from XMS slot AX to es:bx

%ifdef DBGx
toxmsmsg	db 13,10,'esbx->xms.',0
todosmsg	db 13,10,'esbx<-xms.',0
%endif



			; new Jan 2002 - why did nobody abandon
			; the possibility to turn the A20 off
			; as soon as the 386 came up?????

A20KLUDGE:		; hangs if nothing reads the waiting data!
%ifndef SANE_A20
	pushf		; yes, that crappy thing finally got me:
	push ax		; the BOCHS(.com) ROMBIOS fails to check,
	sti		; but in 64 does not only need test 2/Z
a20k:	in al,0x64	; (can accept commands) BUT(!) also has to be
	test al,3	; test 1/Z (has -no- -other- data waiting on
	jnz a20k	; port 60) for command 0xd0 (read A20/...
	pop ax		; state), so iodev/keyboard.cc and
	popf		; bios/rombios.c are at war (in BOCHS) !
			; (which may crash, because int 15.87, the
			; copy function, uses set_enable_a20 ...)
%endif
	ret




	; XMS copy structure (0x10 in size):
	; D size (*2048*), W source handle (0 for DOS), D source
	; (linear or pointer for DOS), W dest handle (...), D dest

copytoxms:	; copy 1 sector from es:bx to xms bin AX
	pushf

%ifdef DBGx
		push word toxmsmsg	; DBG
		call meep		; DBG
%endif

	pusha	; do not trust XMS
	push eax

	movzx eax,ax		; clear high 16 bits
	shl eax,11		; assume *2048* byte sectors!
	push eax		; C bin nr -> dest offs
	push word [cs:xmshandle]	; A dest: xms
	push es
	push bx			; 6 esbx source pointer
	push word 0		; 4 source: dos
	push dword 2048		; 0 size: *2048* (1 sector)
	mov si,sp		; our XMS copy structure on stack
	push ds		; (save)
	push ss
	pop ds		; ... stack again ...

		call A20KLUDGE	; Jan 2002
	mov ah,0x0b	; XMS copy command
		call far [cs:xmsvec]

	pop ds		; (restore)
	mov bp,ax		; (save)
	pop eax		; remove...
	pop eax		;   XMS copy...
	pop eax		;     structure from...
	pop eax		;       stack!
	mov ax,bp		; (restore)

	or ax,ax
	jnz wrxmsok

	; *** whoops: XMS copy returned an error in BL, BL trashed !
		mov al,bl	; for debugging
		mov ah,0x0b	; same
		push word xmserr
		call meep

wrxmsok:
	pop eax
	popa
	popf
	ret

; ---------------------------------------------------------------

copytodos:	; copy 1 sector from xms bin AX to es:bx
	pushf

%ifdef DBGx
		push word todosmsg	; DBG
		call meep		; DBG
%endif

	pusha	; do not trust XMS
	push eax

	push es
	push bx			; C esbx dest pointer
	push word 0		; A dest: dos
	movzx eax,ax		; clear high 16 bits
	shl eax,11		; assume *2048* byte sectors!
	push eax		; 6 bin nr -> source offs
	push word [cs:xmshandle]	; 4 source: xms
	push dword 2048		; 0 size: *2048 (1 sector)
	mov si,sp		; our XMS copy structure on stack
	push ds		; (save)
	push ss
	pop ds		; ... stack again ...

		call A20KLUDGE	; Jan 2002
	mov ah,0x0b	; XMS copy command
		call far [cs:xmsvec]

	pop ds		; (restore)
	mov bp,ax		; (save)
	pop eax		; remove...
	pop eax		;   XMS copy...
	pop eax		;     structure from...
	pop eax		;       stack!
	mov ax,bp		; (restore)

	or ax,ax
	jnz rdxmsok

	; *** whoops: XMS copy returned an error in BL, BL trashed !
		mov al,bl	; for debugging
		mov ah,0x0b	; same
		push word xmserr
		call meep

rdxmsok:
	pop eax
	popa
	popf
	ret

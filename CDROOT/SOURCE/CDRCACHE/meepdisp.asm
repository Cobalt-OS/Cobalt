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


	; more helper functions (using int 0x10):
	; meep: makes a beep and shows AX and the message pointed
	;       to by a word found on stack (if not zero).
	;       Takes the word from stack on return and saves ALL
	;       registers and flags. The empty message defaults
	;	to a single beep (char 07).
	; showal: shows AL as hex, trashing many registers.

	; *** NEW 11/2002: uses a flag to use int 21 during
	; *** initialization, so that you can redirect its output
	; *** DOSEMU define to copy the output of meep to the DOSEMU
	; *** log facility

%ifdef DOSEMU
meepbuf		db "01234567890012345678900123456789001234567890"
		db "01234567890012345678900123456789001234567890",0
meepbufp	dw meepbuf
%imacro DOSEMUTTY 0
	push si
	mov si,[cs:meepbufp]
	mov [cs:si],al			; print char to buffer
	inc si
	cmp si,meepbufp-1		; carry set if in range
	cmc				; invert carry flag
	sbb si,0			; decrement if carry
	mov [cs:meepbufp],si		; update buffer pointer
	pop si
%endm
%imacro DOSEMUPRINT 0
	push ax
	push dx
	push es
	mov ax,cs
	mov es,ax
	mov al,0
	DOSEMUTTY			; print trailing \0
	mov dx,meepbuf
	mov word [cs:meepbufp],dx	; reset buffer pointer
	mov ax,0x13	; print to DOSEMU log (seems not to work?)
	int 0xe6	; DOSEMU API
	pop es
	pop dx
	pop ax
%endm
%else
%define DOSEMUTTY	; do nothing
%define DOSEMUPRINT	; do nothing
%endif

; -------------

meep:	push bp	; to have some feedback: BEEP/show message and
	push si	; then the value in AX
	mov bp,sp	; <- stooopid: forgot to init bp here...
	mov si,[ss:bp+6]	; the pushed word, skipping si bp ip
	pushf
	push ax
	push bx

	push ax	; save for later
meepmsgloop:
	mov al,[cs:si]
	or al,al
	jz short nomeepmeep	; so only beep if no message
	inc si

	cmp word [cs:running],1	; still in init? Use int 21!
	jb meepINT21

meepINT10:

	DOSEMUTTY		; *** NEW 11/2002

	mov bx,7
	mov ah,0x0e
		int 0x10	; show char
	jmp short meepmsgloop

meepINT21:
	push dx
	mov ah,2
	mov dl,al
%ifdef REDIRBUG
	call BUGTTY
%else
		int 0x21	; show char
%endif

	pop dx
	jmp short meepmsgloop

nomeepmeep:
	pop ax
	push ax
	mov al,ah
		call showal	; show AH
	pop ax
		call showal	; show AL
	cmp word [cs:running],1	; only wait if init already done
	jb nomeepwait		; wait a while if we had a message
	mov bx,10		; *** adjust if you want
%ifdef DBG
	mov bx,222		; *** adjust if you want
%endif
mpwt2:	mov si,-1
%ifdef FASTMEEP
	mov si,42
%endif
mpwt1:	
	; in al,0x80		; I/O is slow, good...
	dec si
	or si,si
	jnz mpwt1
	dec bx
	jnz short mpwt2
nomeepwait:

	DOSEMUPRINT	; *** NEW 11/2002

	pop bx
	pop ax
	popf
	pop si
	pop bp
	ret +2		; take the message offset from stack!


showal:			; helper func to show AL as hex
	cmp word [cs:running],1
	jb showalINT21	; *** NEW 11/2002
showalINT10:		; label needed for REGDUMP
	push ax
	mov ah,0
	mov bx,ax
	shr bx,4
	mov al,[cs:hexa+bx]

	DOSEMUTTY		; *** NEW 11/2002

	mov bx,7
	mov ah,0x0e
		int 0x10	; high hex digit
	pop ax
	and ax,0x000f
	mov bx,ax
	mov al,[cs:hexa+bx]

	DOSEMUTTY		; *** NEW 11/2002

	mov bx,7
	mov ah,0x0e
		int 0x10	; low hex digit
	ret

showalINT21:			; *** new 11/2002, allows redirection
	push dx
	push ax
	mov ah,0
	mov bx,ax
	shr bx,4
	mov dl,[cs:hexa+bx]
	mov ah,2		; print high hex digit
%ifdef REDIRBUG
	call BUGTTY
%else
		int 0x21	; show char
%endif
	pop ax
	and ax,0x000f
	mov bx,ax
	mov dl,[cs:hexa+bx]
	mov ah,2		; print low hex digit
%ifdef REDIRBUG
	call BUGTTY
%else
		int 0x21	; show char
%endif
	pop dx
	ret

hexa	db '0123456789abcdef'

	; some useful default messages:
crlfmsg		db 13,10,0
colonmsg	db ':',0
spacemsg	db ' ',0
meepmsg		db 7,0

; ---------------------------------------------------------------

	; this is new Jan 2002: Detailled DEBUGGING register dump.
	; *** Using int 10, NOT using int 21 in any case.

REGDUMP:
%ifndef USEREGDUMP
	ret
%else
	pushf
	push eax
	push bx
	push si
		push bx
	mov si,rdmsg1
	ror eax,16
	call onereg		; EAX hi
	mov si,colonmsg
	ror eax,16
	call onereg		; EAX lo
	mov si,rdmsg2
		pop bx		; original BX
	mov ax,bx
	call onereg
	mov si,rdmsg3
	mov ax,cx
	call onereg
	mov si,rdmsg4
	mov ax,dx
	call onereg
	pop ax			; saved SI
	push ax
	mov si,rdmsg5
	call onereg
	mov si,rdmsg6
	mov ax,di
	call onereg
	mov si,rdmsg7
	mov ax,bp
	call onereg
	mov si,rdmsg8
	mov ax,sp
	add ax,12		; what we used on stack
	call onereg
	mov si,sp
	mov ax,[ss:si+10]	; the IP to which we return
	mov si,rdmsg9
	call onereg

	DOSEMUPRINT	; *** NEW 11/2002

	pop si
	pop bx
	pop eax
	popf
	ret

onereg:	push ax
onestr:	mov bx,7
	mov ah,0x0e
	mov al,[cs:si]
	or al,al
	jz short noonestr
	inc si

	DOSEMUTTY		; *** NEW 11/2002

		int 0x10	; show char
	jmp short onestr
noonestr:
	pop ax
	push ax
	mov al,ah
	call showalINT10
	pop ax
	push ax
	call showalINT10
	pop ax
	ret

rdmsg1	db 13,10,'CDRcache: EAX=',0
rdmsg2	db ' BX=',0
rdmsg3	db ' CX=',0
rdmsg4  db ' DX=',0
rdmsg5	db ' SI=',0
rdmsg6  db ' DI=',0
rdmsg7	db ' BP=',0
rdmsg8	db ' SP=',0
rdmsg9	db ' IP=',0
%endif


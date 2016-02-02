; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2003.
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

; This file: Generic display functions.
; crlf (write cr lf to stdout),
; hex2dec (convert AX from hex to bcd, with saturation),
; showal / showax (write AL / AX to stdout, in hex, strip leading 0s),
; showalFull / showaxFull (write AL / AX ... do not strip leading 0s),
; showtty (write AL to stdout, as char),
; showlist (write DOS string pointed to by [si+(2*AL)] to stdout).

crlf:	push ax
	push dx
	mov dl,13	; CR
	mov ah,2	; char to stdout
	int 21h
	mov dl,10	; LF
	mov ah,2	; char to stdout
	int 21h
	pop dx
	pop ax
	ret

hex2dec:		; take a value AX and convert to packed BCD
        push bx		; modified, taken from LBAcache.
        push cx
        push dx
        push di
        xor dx,dx
        xor cx,cx	; shift counter
        xor di,di	; result
        mov bx,10
h2dlp:  div bx		; remainder in dx, 0..9
        shl dx,cl	; move to position
        or di,dx	; store digit
        xor dx,dx	; make dx:ax a proper value again
        or ax,ax	; done?
        jz h2dend
        add cl,4        ; next digit
        cmp cl,16	; digit possible?
        jb h2dlp	; otherwise overflow
h2doverflow:
	mov di,9999h	; overflow
h2dend: mov ax,di	; return packed BCD
        pop di
        pop dx
        pop cx
        pop bx
        ret

showax:			; show AX in HEX, strip at most 3 leading 0s
	xchg al,ah
	or al,al
	jz showaxshort	; skip leading zeroes
	call showal	; DO strip here
	xchg al,ah
	call showalFull	; do NOT strip here
	ret

showaxFull:		; show AX in HEX, do not strip leading 0s
	xchg al,ah
	call showalFull	; do NOT strip here
	xchg al,ah
	call showalFull	; do NOT strip here
	ret

showaxshort:
	xchg al,ah
	call showal	; DO strip here
	ret

showalFull:		; show AL in HEX, do NOT strip leading 0s
	push ax
	push cx
	xor cx,cx	; strip nothing
	jmp short showal2

showal:			; show AL in HEX, strip at most 1 leading 0
	push ax
	push cx
	mov ch,'0'	; strip if '0'
showal2:
	push dx
	mov dl,al
	mov cl,4
	shr dl,cl
	add dl,'0'
	cmp dl,'9'
	jbe showAL1
	add dl,7	; 'A'-('0'+10)	; ... which is 7
showAL1:
	push ax
	cmp dl,ch	; strip this?
	jz showshortAL	; strip leading zero / leading "impossible"
	mov ah,2	; char DL to stdout
	int 21h
showshortAL:
	pop ax
	and al,15
	mov dl,al
	add dl,'0'
	cmp dl,'9'
	jbe showAL2
	add dl,7	; 'A'-('0'+10)	; ... which is 7
showAL2:
	mov ah,2	; char DL to stdout
	int 21h
	pop dx
	pop cx
	pop ax
	ret

showtty:		; char AL to stdout
	push ax
	push dx
	mov dl,al
	mov ah,2	; char DL to stdout
	int 21h
	pop dx
	pop ax
	ret

showlist:		; list SI, entry AL: show a string
	push ax
	push si
	push dx
	mov ah,0
	add ax,ax
	add si,ax
	mov dx,[si]
	mov ah,9	; string output
	int 21h
	pop dx
	pop si
	pop ax
	ret


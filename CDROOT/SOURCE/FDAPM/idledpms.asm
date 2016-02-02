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

; This file: DPMS screen saver TSR, separate binary from FDAPM
; To compile: "nasm -o idledpms.com idledpms.asm"

%define testmode 0	; either 1 or 0

	org 100h
start:	jmp setup

	; to save RAM, we use the PSP to store variables
%define old1c	50h
; old1c	dd 0	; old handler address
%define old9	54h
; old9	dd 0	; old handler address
%define count	58h
; count	dw 0	; count of idle ticks
%define limit	5ah
; limit	dw 1000	; ticks until screen is switched off
%define idle	5ch
; idle	db 0	; 1 if screen is already off
%define what	5dh
; what	db 0	; 1 to sleep, 0 to wake up
	; 2 bytes left free before the relocated code
%define reloc	60h

i1c:	; timer tick handler
	pushf
	push ax
	cmp byte [cs:idle],1	; screen already off?
	jnz waiting
	jmp done
waiting:
	inc word [cs:count]
	mov ax,[cs:count]
	or al,al		; every once in a while, check win/keyb
	jnz norm		; otherwise just do timeout check
	push ds
	push bx
	xor ax,ax
	mov ds,ax
	lds bx,[ds:(9+9+9+9)]	; int 9 vector
	mov ax,ds
	cmp bx,i9+reloc-start	; compare to relocated self
	pop bx
	pop ds
	jnz gone		; somebody else hooked int 9
	push bx
	mov bx,cs
	cmp ax,bx
	pop bx
	jnz gone
	mov ax,1600h		; Win3.x activity check
	int 2fh
	cmp al,0		; 0 means Win3.x not active
	jnz gone		; else Win3.x is active
	jmp short done		; enough work for now

norm:	cmp ax,[cs:limit]
	jb done			; limit not reached
	call sleep
	mov byte [cs:idle],1	; screen is now off
gone:	mov word [cs:count],0	; block timeout
done:	pop ax
	popf
	jmp far [cs:old1c]


i9:	; keyboard handler
	cmp byte [cs:idle],1
	jnz notidle
	call wakeup
	mov byte [cs:idle],0	; no longer idle
notidle:
	mov word [cs:count],0
	jmp far [cs:old9]


wakeup:	mov byte [cs:what],0
	jmp short godpms
sleep:	mov byte [cs:what],1
godpms:
	push ax
	push bx
	push cx
	push dx
	mov al,[cs:what]
%if testmode
	push ds
	mov ah,1ch
	xor al,1
	add al,'0'	; char to display: "1 for on, 0 for off"
	mov bx,0b800h
	mov ds,bx
	mov [ds:0],ax
	pop ds
%else
	call dodpms	; switch screen state
%endif
	pop dx
	pop cx
	pop bx
	pop ax
	ret

%include "dpmsfunc.asm"

setup:	push es
	mov es,[cs:2ch]	; environment segment
			; see Ralf Browns IntList tables 1378-1379
	mov ah,49h
	int 0x21	; free the environment! (saves some RAM)
	pop es		; (jc "could not free... bla")
	call findEga	; EGA, VGA and VESA VBE supported
	jnc egaok
	mov dx,egamsg
	jmp quit

egaok:	cld
	mov si,81h
cmd:	lodsb
	cmp al,' '
	jz cmd
	cmp al,'/'
	jz cmd
	cmp al,'-'
	jz cmd
	cmp al,'0'
	jb help
	cmp al,'9'
	ja help
	mov ah,al
	lodsb	; optional 2nd digit
	cmp al,13
	jz onedigit
	cmp al,' '
	jz onedigit
	cmp al,'0'
	jb help
	cmp al,'9'
	ja help
	push ax
	lodsb
	mov bl,al
	pop ax
	cmp bl,13
	jz twodigits
	cmp bl,' '
	jz twodigits

help:	mov dx,helpmsg
	jmp quit

twodigits:
	push ax
	mov al,ah	; high digit
	sub al,'0'
	mov ah,0
	mov bl,10
	mul bl
	mov bl,al	; ah will be 0
	pop ax
	sub al,'0'
	add al,bl	; low digit
	jmp short calc
onedigit:
	mov al,ah	; only digit
	sub al,'0'
calc:	mov ah,0
	cmp al,0
	jz badval	; too short
	cmp al,60
	ja badval	; too long
%if testmode
	mov bx,109	; 1092 ticks per minute
%else
	mov bx,1092	; 1092 ticks per minute
%endif
	mul bx
	mov [limit],ax	; dx will be 0 if at most 60 minutes
	xor ax,ax
	mov [count],ax	; init the counter to 0
	mov [idle],al	; not yet idle

	push es
	mov ax,3509h	; get int 9
	int 21h
	mov [cs:old9],bx
	mov [cs:old9+2],es
	mov ax,351ch	; get int 1c
	int 21h
	mov [cs:old1c],bx
	mov [cs:old1c+2],es
	mov di,bx
	mov si,i1c		; not relocated yet
	mov cx,16		; compare a bit...
	repz cmpsb
	jz alreadytsr

notsryet:
	pop es
	cld
	mov si,start		; offset
	mov di,reloc		; offset
	mov cx,setup-start	; size
	rep movsb		; copy own resident code up
	mov ax,2509h		; set int 9
	mov dx,i9+reloc-start	; offset
	int 21h
	mov ax,251ch		; set int 1c
	mov dx,i1c+reloc-start	; offset
	int 21h
	mov dx,loadmsg
	mov ah,9
	int 21h
	mov ax,3100h		; stay TSR
	mov dx,setup+15+reloc-start	; setup itself is not TSR
	mov cl,4
	shr dx,cl	; convert to paras
	int 21h

badval:	mov dx,timemsg
	jmp short quit

alreadytsr:		; current int1c handler starts like ours...
	mov word [es:count],0
	mov ax,[cs:limit]
	mov [es:limit],ax
	pop es
	mov dx,twomsg

quit:	mov ah,9
	int 21h
	mov ax,4c01h
	int 21h

egamsg	db "EGA, VGA or newer needed.",13,10,"$"
timemsg	db "Time must be 1 to 60 min.",13,10,"$"
twomsg	db "Set new timeout.",13,10,"$"
loadmsg	db "Loaded, will switch off your DPMS screen while no",13,10
	db "keys are pressed for more than the selected time.",13,10,"$"

helpmsg	db "Usage: IDLEDPMS time",13,10
	db "Switches off the screen after [time] minutes without keypress, unless",13,10
	db "Windows 3.x or a game with own keyboard IRQ driver is active.",13,10
	db "This is free open source GPL 2 software,",13,10
	db "Written and conceived by Eric Auer 2007.",13,10,"$"


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

; This file: DPMS screen functions. dodpms, findEga, findVga, vbeSet
; VBE/PM support added 6/2007
; Must only use relative references to be relocateable for IDLEDPMS ;-)

dodpms:	mov ah,0x12	; input: AL 1 for DPMS off, 0 for DPMS on (!)
	call findEga	; preserves AX, tests for VGA, sets CY if not found
	jnc wxVga2
	stc
	ret
wxVga2:	call findVga
	mov bx,ax	; to remember the on/off decision: al 1 off 0 on!
	jc wxVga3	; skip if not VGA
	call vbeSet	; try VBE first
	; Mateusz has VBE that claims to work but does not, so always go on
;	jnz wxV2b	; if VBE failed, fall back to classic VGA
;	clc
;	ret
wxV2b:	mov bl,0x36	; AX 1201 / 1200 here to disable / enable refresh
	push ax
	int 0x10	; INT 10.12.BL=36 VGA refresh control
	; returns AL=12 if function supported, but the user will
	; notice anyway... (whole screen will show DAC[0] color)
	pop bx		; BL is used to remember the on/off decision
wxVga3:	; int 10.1012.bl=36 was VGA only, everything else works on EGA+
	mov dx,0x3c4
	mov al,1	; EGA/VGA sequencer: clocking
	out dx,al
	inc dx
	in al,dx
	and al,0xdf	; VGA screen refresh on (21jul2007: df, not d0!)
	cmp bl,0	; 0 means "enable"
	jz wxVgaOn1	; if enabled, skip "refresh off"
	or al,0x20	; VGA screen refresh off
wxVgaOn1:
	out dx,al
	;
	push ds
	mov ax,0x40
	mov ds,ax
	mov dx,[ds:0x63]	; CRT controller base I/O port address
	pop ds		; (3b4 for mono, but usually 3d4 for color)
	;
	mov al,0x17	; EGA/VGA: mode control
	out dx,al
	inc dx
	in al,dx
	and al,0x7f	; reset/stop CRTC
	cmp bl,1	; 1 means "disable"
	jz wxVgaOff1	; if disabled, stay in stop mode
	or al,0x80	; CRTC resume from stop
wxVgaOff1:
	out dx,al
	ret

findVga:		; check if VGA is present, set CY otherwise (5/2005)
	call findEga
	jc noVga
	push ax
	push bx
	mov ax,0x1a00	; get VGA display combo BX / "install check"
	int 0x10
	cmp al,0x1a	; confirmation received? then VGA found.
	pop bx
	pop ax
	stc
	jnz noVga
isVga:	clc
noVga:	ret

findEga:		; check if EGA is present, set CY otherwise (5/2005)
	push ax
	push bx
	push cx
	mov ah,0x12
	mov bx,0x0ff10	; int 10.12 bl=10 get EGA info / "install check"
	mov cx,0x0ffff
	int 10h
	cmp cx,0x0ffff	; nothing happened? then no EGA found.
	pop cx
	pop bx
	pop ax
	stc
	jz noEga
isEga:	clc
noEga:	ret

vbeSet:	push cx		; set DPMS off/on if al odd/even, NZ if error
	push bx
	push ax
	mov cx,ax
	push es
	push di
	xor di,di
	mov es,di
	mov ax,0x4f10	; VBE/PM
	xor bx,bx	; function bl=0 is "install check"
	int 10h
	pop di
	pop es
	cmp ax,4fh	; okay?
	jnz noVBEx
	mov ax,1	; set state bh: 0 on 1 stdby 2 suspend 4 off 8 dim
	test cx,1
	jz knowV	; if setting to on... else find a nice state next
	test bh,7	; state 1, 2 or 4 supported?
	jz badV		; else return with NZ
	mov ah,4	; try off
	test bh,ah	; is off supported?
	jnz knowV
	shr ah,1	; else try suspend
	test bh,ah
	jnz knowV
	mov ah,1	; else use standby
knowV:	mov bx,ax	; do set that state
	mov ax,0x4f10	; VBE/PM, function bl=1 is "set mode"
	int 10h
badV:	cmp ax,4fh	; okay?
noVBEx:	pop ax
	pop bx
	pop cx
	ret		; NZ if error, Z if okay


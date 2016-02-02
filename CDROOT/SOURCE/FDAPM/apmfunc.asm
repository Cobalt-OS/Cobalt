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

; This file: connectAPM - call to connect to APM if not already
; done, and return APM version in AH.AL and cs:apmversion ...
; at most, version 1.2 is reported. CY set if no APM found.
; enableAPM - call to enable APM and - if APM 1.1 or better - CPU
; management. AX BX CX destroyed.
; disableAPM - call to disable APM and if enabled CPU management.
; AX BX CX destroyed.

connectAPM:	; connect to APM. Return APM version in AX.
	mov ax,[cs:apmversion]
	cmp ax,-1
	jnz connectedAPM	; tried that before?
	push bx
	push cx
	mov ax,5300h
	xor bx,bx
	int 15h		; APM install check
	pop cx
	jc nAPM1
	cmp bx,504dh	; "PM"
	jz yAPM
nAPM1:	xor ax,ax	; no APM found
	jmp nAPM2
yAPM:	cmp ah,1
	jb nAPM1	; require at least APM 1.0
	jz v1APM
v12APM:	mov ax,102h	; remember APM 2.x or newer as APM 1.2
v1APM:	cmp al,2
	ja v12APM	; clamp to 1.2 if 1.x above 1.2
	push ax	; version
	;
	mov ax,5301h
	xor bx,bx	; "systemwide"
	int 15h		; real mode connect (makes version go back to 1.0!)
	jnc connAPMok
	cmp ah,2
	jnz nAPM1	; error but not simply "already connected"
connAPMok:
	pop cx		; version
	push cx
	mov ax,530eh	; set version
	xor bx,bx	; "systemwide"
	cmp cx,100h	; only 1.0 ? (does not support int 15.530e)
	jz connAPMold
	int 15h		; otherwise, reactivate newer interface
connAPMold:
	pop ax		; version
nAPM2:	pop bx
	mov [cs:apmversion],ax
connectedAPM:
	or ax,ax
	jz noAPM
	clc
	ret
noAPM:	stc
	ret

enableAPM:		; enable BIOS APM and possibly CPU management
	call connectAPM
	jc noAPM
	cmp ax,0100h
	ja enNewAPM
	mov ax,5308h
	mov cx,1	; enable
	mov bx,0ffffh
	int 15h		; enable APM 1.0
	ret
enNewAPM:
	mov ax,5308h
	mov bx,1
	mov cx,1	; enable
	int 15h		; enable standard power management
	mov ax,530fh
	mov bx,1
	mov cx,1
	int 15h		; engage power management, coop., APM 1.1+
	mov ax,530dh
	mov bx,1
	mov cx,1
	int 15h		; enable APM 1.1+ CPU management
	ret

disableAPM:		; disable BIOS APM and if possible CPU mgmt.
	call connectAPM
	jc noAPM
	cmp ax,0100h
	ja disNewAPM
	mov ax,5308h
	xor cx,cx	; disable
	mov bx,0ffffh
	int 15h		; disable APM 1.0
	ret
disNewAPM:
	mov ax,5308h
	mov bx,1
	xor cx,cx	; disable
	int 15h		; disable standard power management
	mov ax,530fh
	mov bx,1
	xor cx,cx
	int 15h		; disengage power management APM 1.1+
	mov ax,530dh
	mov bx,1
	xor cx,cx
	int 15h		; disable APM 1.1+ CPU management
	ret


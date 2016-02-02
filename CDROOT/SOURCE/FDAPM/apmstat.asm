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

; This file: statusAPM (destroys registers)
; Writes APM status to stdout, or nothing if no APM installed.

apmvermsg	db "APM detected, version compatible to APM $"
battmsg		db " found",13,10,"AC power status: $"
battlist	dw bloffline, blonline, blups, blunk
bloffline	db "mains offline$"
blonline	db "mains online$"
blups		db "running on backup power$"
blunk		db "power supply unknown$"

bchargelist	dw bchigh, bclow, bccrit, bccharge, bcunk
bchigh		db "Battery quite full$"
bclow		db "Battery quite empty$"
bccrit		db "Battery almost empty!!!$"
bccharge	db "Battery now charging$"
bcunk		db "Battery charge status unknown$"

battpercentmsg	db "Battery fill percentage: $"
unknownmsg	db "unknown$"	; if battery fill percentage is -1
battremmsg	db "Remaining battery time: $"
hoursmsg	db " hours $"
minutesmsg	db " minutes $"
secondsmsg	db " seconds $"
hourmsg		db " hour $"
minutemsg	db " minute $"
secondmsg	db " second $"

; ---------------

statusAPM:
	call connectAPM	; returns APM version in AX, carry if none
	jnc yAPMSD
	jmp nAPMSD
yAPMSD:	push ax		; ... version
	mov dx,apmvermsg
	mov ah,9
	int 21h
	pop ax		; ... version
	push ax		; ... version
	xchg al,ah
	call showal	; major version (w/o leading 0s)
	mov al,'.'
	call showtty
	xchg al,ah
	call showal	; minor version (w/o leading 0s)
	;		; (use showalFull to show leading 0s)
	mov dx,battmsg
	mov ah,9
	int 21h
	xor dx,dx	; time 0
	mov ax,530ah	; get battery status
	mov bx,1
	int 15h
	pop ax		; ... version
	mov di,ax	; remember version
	push dx		; remember time <<<
	;
	mov si,battlist
	mov al,bh	; battery status 1
	cmp al,2
	jna battBHsane
	mov al,3
battBHsane:
	call showlist	; battery status: offline, online, ups, unk.
	call crlf
	mov al,bl	; battery status 2
	cmp al,3
	jna battBLsane
	mov al,4
battBLsane:
	mov si,bchargelist
	call showlist
	call crlf
	;
	mov dx,battpercentmsg
	mov ah,9
	int 21h
	xor ax,ax
	mov al,cl	; battery fill percentage
	cmp al,255	; unknown?
	jnz battCLsane
	mov dx,unknownmsg
	mov ah,9
	int 21h
	jmp short battCLdone
battCLsane:
	call hex2dec
	call showax
battCLdone:
	call crlf
	;
	pop bx		; recall time <<<
		; or do we have to do int 15.530a.bx=8001 to
		; explicitly query battery 1 if APM 1.2 ?
%ifndef DEBUG
	cmp bx,-1	; unknown remaining battery time?
	jz nbtime1	; then skip info completely
	test bx,07fffh	; zero remaining battery time?
	jz nbtime1	; then skip info completely
%endif
	mov ax,di	; recall version
	cmp ah,1
	jb nbtimX
	ja ybtime
	cmp al,1
nbtimX:	jb nbtime
ybtime:			; if at least APM 1.1...
	mov dx,battremmsg
	mov ah,9
	int 21h
	mov di,bx	; remember for minute / seconds decision
	mov ax,bx	; * BUGFIX
	and ax,07fffh	; remove minute / second unit flag
	xor dx,dx	; division will be 32:16 bits
	mov bx,60	; make hour:minute or minute:second format
	div bx		; * RESULT: AX   REMAINDER: DX
	test ax,ax	; * can be up to 3 digits (32767/60)
	jz ytinytime	; do not tell about high part if zero
	;
	push dx		; * ... remember low part
	call hex2dec
	call showax	; high part (div. by 60 part)
	;
	cmp ax,0001	; singular or plural ?
	jnz yhrplur
	mov dx,hourmsg
	test di,8000h	; hours or minutes?
	jnz yhrsmsg
	mov dx,minutemsg
	jmp short yhrsmsg
nbtime1:		; just because jump would be out of range
	jmp short nbtime
yhrplur:
	mov dx,hoursmsg
	test di,8000h	; hours or minutes?
	jnz yhrsmsg
	mov dx,minutesmsg
yhrsmsg:
	mov ah,9
	int 21h
	;
	pop ax		; ... recall low part
	;
ytinytime:
	test al,al	; low part (modulo 60 part)
	jz nbtime	; skip low part if zero
	xor ah,ah
	call hex2dec
	; call showalFull	; low part (always 2 digits)
	call showal	; low part (w/o leading 0s)
	;
	cmp al,01	; singular or plural ?
	jnz yminplur
	mov dx,minutemsg
	test di,8000h	; minutes or seconds?
	jnz yminmsg
	mov dx,secondmsg
	jmp short yminmsg
yminplur:
	mov dx,minutesmsg
	test di,8000h	; minutes or seconds?
	jnz yminmsg
	mov dx,secondsmsg
yminmsg:
	mov ah,9
	int 21h
	call crlf
nbtime:
nAPMSD:
	ret


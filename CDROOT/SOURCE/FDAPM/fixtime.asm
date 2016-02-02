; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2005.
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


%define VERBOSEFIXTIME 1	; set to 1 to get messages about skew


	; call this function to adjust the timer tick count by
	; reading the real time clock (RTC) time. Shows some
	; messages, and can change all general purpose registers.
	; Used after a SUSPEND (e.g. from reboot.asm).
fixTime:
	mov ah,2	; get RTC time
	clc		; some BIOSes forget to update this flag
	int 1ah		; clock services
	jnc rtcokay	; carry set? Maybe RTC was busy!

	mov ah,0
	int 1ah
	mov bx,dx
	inc bx
	inc bx		; we will wait 1.1 ... 2 ticks now
rtcwait:
	mov ah,0
	int 1ah
	cmp bx,dx
	jnz rtcwait
	mov ah,2	; get RTC time (2nd try)
	clc
	int 1ah		; clock services
	jnc rtcokay	; now it should really have worked!
rtcfail:
	mov dx,rtcfailmsg
	mov ah,9	; show string
	int 21h
	ret

rtcokay:		; now time is in CH:CL:DH as BCD hh:mm:ss
	; (DL is 1 if daylight saving time, ignoring that for now)
	; MS DOS would assume "no RTC present" if time is 00:00:00
	; and stuck. But we have no suspend on pre-286 anyway.
	xor si,si	; will store high part of ticks
	xor di,di	; will store low part of ticks
	; cmp cl,59h	; (BCD!) minutes > 59 would overflow...
	; cmp ch,34h	; (BCD!) hours > 34 would overflow...

	mov al,dh	; seconds (last access to the int 1a.02 DX)
	call bcd2bin	; AL -> AX
	mov dh,91	; 91/5 is 18.2, good enough approximation
	mul dh		; AX = AL * 91
	push cx
	mov cx,5
	xor dx,dx
	div cx		; AX = (DX):AX / 5, remainder in DX ignored
	pop cx
	mov di,ax	; first component of converted tick value

	push cx
	mov al,cl	; minutes
	call bcd2bin	; AL -> AX
	mov dx,14201	; 14201/13 is more exact than 18.2*60
	mul dx		; DX:AX = AX * 14201
	mov cx,13
	div cx		; AX = DX:AX / 13, remainder DX ignored
	pop cx
	add di,ax	; add second component...
	adc si,0	; ...to our 32 bit value!

	mov al,ch	; hours (are 65536+7.4 ticks each)
			; (CX from int 1a.02 is no longer needed)
	call bcd2bin	; AL -> AX
	add si,ax	; add 65536 ticks per hour
	mov dl,22	; 22/3 = 7.33 is a sufficient approx of 7.4
	mul dl		; AX = AL * 22
	xor dx,dx
	mov cx,3
	div cx		; AX = (DX):AX / 3, remainder DX ignored

	add di,ax	; add third component...
	adc si,0	; ...to our 32 bit value.

	; --------------

%if VERBOSEFIXTIME
	push si
	push di
	mov ah,0	; get tick time
	int 1ah		; clock services
	; also returns AL, the "midnight crossed" flag
	sub dx,di	; do 32 bit sub to get...
	sbb cx,si	; ... "timer_tick - expected_based_on_RTC"
	; negative: timer was behind RTC (normal for SUSPEND mode)
	; positive: timer was ahead, maybe a midnight crossing?
	js normalskew
	;
%if 1			; ***
	or cx,cx
	jnz aheadskew
	; timer was < 1h or > 23h ahead
	cmp dx,91
	jnb aheadskew
	jmp funnyskew	; timer was < 5 seconds AHEAD, show no message
			; (-23.9h is less plausible than +2s here)
aheadskew:
%endif			; ***
	;
	sub dx,00b0h	; 1800b0h ticks per day
	sbb cx,24	; 18h is 24
	js normalskew
wrongskew:
	mov dx,timefailmsg	; time difference > 1 day??
	mov ah,9	; show string
	int 21h
	jmp funnyskew

normalskew:
	xor ax,ax
	xor bx,bx
	sub ax,dx
	sbb bx,cx	; bx:ax is now the skew value!!!
	mov dx,bx	; DX:AX is nicer to process
	cmp dx,24	; more than 24,9 hours? should not happen.
	ja wrongskew
	cmp dx,1
	ja hourskew	; 2 or more hours
	jz minuteskew	; 60..119 minutes
	cmp ax,2190	; at least 2 minutes?
	ja minuteskew

secondskew:		; at most 119.9 seconds
	mov word [cs:skewunit], skewsec		; offset
	jmp scaledskew

minuteskew:		; at most 119.9 minutes
	mov bx,60
	div bx		; AX = DX:AX / 60
	xor dx,dx	; expand to 32 bit again
	mov word [cs:skewunit], skewmin		; offset
	jmp scaledskew

hourskew:		; at most 23.9 hours
	mov bx,3600
	div bx		; AX = DX:AX / (60*60)
	xor dx,dx	; expand to 32 bit again
	mov word [cs:skewunit], skewhour	; offset

scaledskew:
	test dx,dx	; zero or not?
	pushf
	mov bx,5	; *5, /91 to get /18.2
	mul bx		; DX:AX = AX * 5
	popf
	jz dxzeroskew
	add dx,5	; do not forget the 10000h*5 part...
dxzeroskew:
	mov bx,91
	div bx		; AX = DX:AX / 91, remainder in DX (max. 90)
	push ax		; *** max. 3 digits
	mov ax,dx
	mov dl,10
	mul dl		; AX = AL * 10 (max. 900)
	mov dl,91
	div dl		; AL = AX / 91, we just got one ".digit"
	add al,'0'	; convert to ASCII
	mov [cs:skewdig],al
	pop ax
	mov byte [cs:skewmain],' '
	cmp al,100	; 3rd digit?
	jb al99skew
	mov byte [cs:skewmain],'1'
	sub al,100
al99skew:
	aam		; convert AL to BCD (AH, AL)
	xchg al,ah	; make "high part first" in "RAM"
	add ax,"00"	; convert to ASCII
	cmp al,'0'	; leading 0 possible?
	jnz twoskew
	cmp byte [cs:skewmain],'1'
	jz twoskew	; 1xx, so no actually leading 0
	mov al,' '	; suppress one leading 0
twoskew:
	mov [cs:skewmain+1],ax
	mov ah,9
	mov dx,skewmsg	; show time update message
	int 21h
	mov ah,9
	mov dx,[cs:skewunit]	; fetch appropriate string offset
	int 21h

funnyskew:
	pop di
	pop si

%endif	; VERBOSEFIXTIME

	; --------------

	mov ah,1	; set tick time
	mov cx,si	; high part
	mov dx,di	; low part
	int 1ah		; clock services
	ret

bcd2bin:	; convert BCD in AL to binary in AX
	push cx
	mov cl,4
	mov ah,al
	shr ah,cl	; high digit
	and al,0fh	; low digit
	aad		; convert to binary
	pop cx
	ret

rtcfailmsg	dd "RTC error?",13,10,"$"
timefailmsg	dd "Timer error?",13,10,"$"

%if VERBOSEFIXTIME
skewmsg		db "Updated timer from clock: Difference was "
skewmain	db "199."
skewdig		db "0 $"
skewunit	dw skewmin
skewsec		db "seconds.",13,10,"$"
skewmin		db "minutes.",13,10,"$"
skewhour	db "hours.",13,10,"$"
%endif	; VERBOSEFIXTIME


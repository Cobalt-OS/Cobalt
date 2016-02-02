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

; This file: statusPOWER (destroys registers)
; Writes POWER TSR status to stdout, or just tells if no TSR found.
; Change in 2007: Just edited sources to fit into 80 columns :-)

foundmsg	db "Found resident FDAPM / POWER driver version $"
nfoundmsg	db "No FDAPM / POWER driver resident yet.",13,10,"$"

status1msg	db "Savings mode: $"
status1list	dw status1off, status1inv, status1std, status1adv
status1off	db "off (no savings)$"
status1inv	db "only hooks, but no savings!?$"
status1std	db "BIOS APM enabled$"
status1adv	db "BIOS APM plus interrupt hooks $"

idledetmsg	db "Idle detection strategy bitmask: $"

advlist		dw advminmsg, advminmsg, advminmsg, advminmsg
		dw advminmsg, advminmsg, advregmsg, advmaxmsg
advminmsg	db "(MIN setting)$"
advregmsg	db "(REG setting)$"
advmaxmsg	db "(MAX setting)$"

pollfreqmsg	db "APM polling frequency: $"

idlePercMsg1	db " CPU was idle $"
idlePercMsg2    db "% of the time$"
idlePercWarn	db " in APMDOS mode (not on now!)$"

; ---------------

statusPOWER:
	mov ax,5400h
	xor bx,bx
	int 2fh
	cmp bx,504dh
	jz tsrStatus
	mov dx,nfoundmsg
	mov ah,9
	int 21h
	ret
tsrStatus:
	push ax		; store version
	mov dx,foundmsg
	mov ah,9
	int 21h
	pop ax
	;
	xchg al,ah	; AH first
	call showal	; major version (w/o leading 0s)
	mov al,'.'
	call showtty
	xchg al,ah	; AL now
	call showal	; minor version (w/o leading 0s)
			; use showalFull for with leading 0s
	call crlf
	;
	mov dx,status1msg
	mov ah,9
	int 21h
	mov ax,5401h
	mov bh,0	; get status
	int 2fh
	and bl,3	; 0 off 2 std 3 adv
	mov al,bl
	mov si,status1list
	call showlist	; OFF STD or ADV
	cmp al,3
	jz yAdvStatus
	jmp nAdvStatus
	;
yAdvStatus:
	mov ax,5403h
	xor bx,bx	; get ADV intensity
	int 2fh
	mov al,bl
	cmp bx,7
	jbe advSaneStatus
	mov al,7	; unknown intensity -> assume MAX
advSaneStatus:
	mov si,advlist
	call showlist
	call crlf
	;
	mov ax,5482h		; get/set APM polling frequency
	xor bx,bx	; get APM polling frequency (bx 0 for "get")
	int 2fh
	or ax,ax		; success? indicated by ax being 0.
	jnz advNoAPMPollFreq	; else skip this display completely.
	or bx,bx		; frequency zero is not displayed.
	jz advNoAPMPollFreq
	push bx			; remember frequency BX
	mov dx,pollfreqmsg
	mov ah,9
	int 21h
	pop ax			; restore into AX
	call hex2dec	; convert AX to BCD (saturates at 9999)
	call showax
advNoAPMPollFreq:
	;
	; *** you MAY want to display the idleness and APM counters
	; *** which would mean 8 32bit values, probably far too much
	; *** info for a normal user. However, the cpu on ticks vs
	; *** cpu idle ticks ratio might be interesting, e.g. as
	; *** percent display...
	;
	; *** MS POWER apparently displays "100 * idleTicks/onTicks"
	; *** which overflows if onTicks is 0 or the result > 32bit.
	; *** We also display that, but with a 16 bit approach.
	;
	mov dx,idlePercMsg1
	mov ah,9
	int 21h
	mov ax,5481h	; get interrupt call based idle/work tick counts
	mov bx,0	; not BIOS but DOS counters
	mov cx,7*4		; size of buffer (in bytes)
	mov si,IdleStatBuf	; simply overwrite our local buffer!
	int 2fh
	mov ax,[ds:idleTicks]
	mov dx,[ds:idleTicks+2]	; 16bit code...
	mov bx,[ds:onTicks]
	mov cx,[ds:onTicks+2]
iPercScale:
	or cx,cx		; > 16bit?
	jnz iPercScale2		; FIXED 3/2005: BOTH cx and dx must be 0!
	or dx,dx		; > 16bit?
	jz iPercOkay
iPercScale2:		; SCALE values until CX and DX are both 0
	shr cx,1		; lowest bit -> carry
	rcr bx,1		; -> becomes highest bit
	shr dx,1		; same again... now, dx:ax
	rcr ax,1		; and cx:bx are 1/2 of before.
	jmp short iPercScale
iPercOkay:		; scaled to 16 bit values: AX of BX units idle
	cmp bx,ax		; more idle time than uptime?
	jb iPercInfinite	; show "100%" instead of impossible value
	mov cx,100		; turn into %
	mul cx			; scaled idleTicks * 100 -> dx:ax
; ---	cmp bx,100		; would division overflow?
; ---	jb iPercInfinite	; no uptime, but idle time...
; ---	; (cx unused)		; worst case: 100*64k / 100
	div bx			; calculate busy percentage
	jmp short iPercKnown
iPercInfinite:
	mov ax,100		; default
iPercKnown:
	call hex2dec	; convert AX to BCD (saturates at 9999)
	call showax
	mov dx,idlePercMsg2
	mov ah,9
	int 21h
	mov ax,5401h	; get current mode (off, ?, bios, dos)
	xor bx,bx
	int 2fh
	and bl,3
	cmp bl,3	; is it the DOS hooking power saving mode?
	jz idlePercFresh	; else warn about unreliable stats
	mov dx,idlePercWarn
	mov ah,9
	int 21h
idlePercFresh:
	;

nAdvStatus:
	call crlf
	;
	mov ax,5402h
	xor bx,bx
	int 2fh
	mov al,bl
	cmp al,-1	; no value? Then skip display!
	jz nAdvIdleStrat
	cmp al,0fh	; default value? Skip display, too!
	jz nAdvIdleStrat
	push ax		; remember that other value (can be 00..0E)
	mov dx,idledetmsg
	mov ah,9
	int 21h
	pop ax		; restore to-be-displayed value
	call showal	; idle detection strategy bitmask
	call crlf
nAdvIdleStrat:
	;
	ret


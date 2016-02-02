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

; This file: All interrupt handlers and related things that
; belong to the TSR part of the tool.
; If UNIDLECHECK is %define-d, more handlers are enabled.
; Main configuration variables are in main file:
; dw apmversion, db savingstrat, dw MainStatus,
; db IdleStrategy, dw IdleHardness, dw APMPollFreq.
; New 6/2007: maxSavingstrat.

; idlestrategy bits 0..3: IDLE16*, IDLE28, IDLE2F*, IDLE2A
; idle stat buf order: 2f*, 28, 16*, 2a
%define IDLE2f 1	; index value into counters and for IdleStrategy
%define IDLE28 2
%define IDLE16 3
%define IDLE2a 4

i16:	cmp ah,1	; 0: 84 key "check"  /  1: 84 key "get"
	jbe ii16
	cmp ah,10h	; 102 key style "get"
	jz ii16
	cmp ah,11h	; 102 key style "check"
	jz ii16
	cmp ah,20h	; 122 key style "get"
	jz ii16
	cmp ah,21h	; 122 key style "check"
	jnz ix16
ii16:	test ah,1	; GET key or CHECK for key?
	jz ii16i	; 0, 10h... are "get key". Those are "idling"
			; 1, 11h... are only "check for key". Work!
ii16w:	
	call working	; sign of WORK state: checking for key
	jmp short ix16
	;
ii16L:	mov byte [cs:whichIdling],IDLE16	; which counter update
	call idling	; sign of IDLE state: waiting for key
ii16i:	push ax		; only "go idling" if no key in queue!
	or ah,1		; 0->1, 10h->11h, 20h->21h: check for key
	pushf			; simulate interrupt
	call far [cs:oldi16]	; original handler
	pop ax
	jz ii16L	; if no key available yet, KEEP IDLING.
	;
ix16:	jmp far [cs:oldi16]

i2a:	cmp ah,84h
	jnz ix2a
	mov byte [cs:whichIdling],IDLE2a	; which counter update
	call idling
ix2a:	jmp far [cs:oldi2a]

i28:	cmp word [cs:IdleHardness],7		; ADV:MAX active?
	jb ix28					; else don't idle here
	mov byte [cs:whichIdling],IDLE28	; which counter update
	call idling
ix28:	jmp far [cs:oldi28]

; ---------------

i2f:	cmp ah,54h	; a call for POWER (int 2f.54xx)?
	jz ini2f
	jmp ii2f	; skip POWER API
ini2f:
	; *** the POWER API ***
	cmp al,0	; version check: 2f.5400
	jnz i2fa
	mov ax,0100h	; "version 1.0"
	mov bx,504dh	; detection magic "PM"
	iret
i2fa:	cmp al,1	; status: 2f.5401
	jnz i2fb
	or bh,bh
	jnz setstat
	mov bl,[cs:MainStatus]
i2fret:	xor ax,ax
	iret
setstat:			; ***
	mov [cs:MainStatus],bl	; 2LSB: 0 off 2 std 3 adv
	call useMainStatus
	jmp short i2fret
i2fb:	cmp al,2	; strategy: 2f.5402
	jnz i2fc
	or bh,bh
	jnz setstrat
	mov bl,[cs:IdleStrategy]
	jmp short i2fret
setstrat:			; ***
	mov [cs:IdleStrategy],bh	; bit field
	jmp short i2fret
i2fc:	cmp al,3	; intensity: 2f.5403
	jnz i2fd
	or bx,bx
	jnz sethard
	mov bx,[cs:IdleHardness]	; get adv:___ level (</=/> 6)
	jmp short i2fret
sethard:			; ***
	mov [cs:IdleHardness],bx	; set adv:___ (min/reg/max)
i2fd:	cmp al,81h	; get stats: 2f.5481 get stats
	jnz i2fe	; ** 2f.5480 not implemented! **
	or bx,bx	; (because not documented in RBIL)
	jnz getAPMstat
getIDLEstat:
	cmp cx,28
	jb i2ferr
	mov ax,IdleStatBuf
	call getStatBuf
	jmp short i2fret
getAPMstat:
	cmp bx,-1	; NEW 2/2005 *** use bx=-1 to reset all stats ***
	jz zapAllStats
	cmp cx,4
	jb i2ferr
	mov ax,APMStatBuf
	call getStatBuf
	jmp short i2fret
i2ferr:	mov ax,87h
	iret
i2fe:	cmp al,82h	; pollfreq: 2f.5482 (dummy)
	jnz ii2f	; ** 2f.548f not implemented! **
	or bx,bx	; (because not documented in RBIL)
	jnz setPollFreq
	mov bx,[cs:APMPollFreq]	; dummy for now
	jmp short i2fret
setPollFreq:			; ***
	mov [cs:APMPollFreq],bx	; dummy for now
	jmp short i2fret
ii2f:	cmp ah,0aeh	; NOT POWER API, but DOS 3.3/newer command.com
	jnz ix2f	; "installable command" API, called by the shell
	call working	; whenever a line of shell input is processed
ix2f:	cmp ax,1680h	; "release time slice" (multitasker idle call)
	jnz iy2f
	mov byte [cs:whichIdling],IDLE2f	; which counter update
	call idling
iy2f:	jmp far [cs:oldi2f]


zapAllStats:
	push ax
	call getTime
	mov [cs:lastOnTick],ax
	mov [cs:lastIdleTick],ax
	xor ax,ax
	mov [cs:resumeCount],ax
	mov [cs:resumeCount+2],ax
	pop ax
	push di
	mov di,IdleStatBuf
idleStatZap:
	mov byte [cs:di],0
	inc di
	cmp di,IdleStatBuf+28
	jb idleStatZap
	pop di
	mov bx,55aah	; confirm zapping
	clc
	jmp i2fret


getStatBuf:	; copy cs:ax to ds:si length cx. Don't change regs.
	pushf
	push ax
	push bx
	push cx
	push si
	mov bx,ax
gSBlp:	mov al,[cs:bx]
	mov [ds:si],al
	inc si
	inc bx
	loop gSBlp
	pop si
	pop cx
	pop bx
	pop ax
	popf
	ret

useMainStatus:	; update settings to reach OFF, REG or ADV mode
	push ax
	mov ah,[cs:MainStatus]	; 0 off 2 std 3 adv
	and ah,3
	mov al,[cs:savingstrat]
	and al,0fch	; no HLT nor APM IDLE
			; or al,ah is probably too simplistic, so...
	cmp ah,3	; only in ADV mode use int hooks!?
	jnz noMainADV
	or al,[cs:maxSavingstrat]	; enable HLT and/or APM IDLE
		; (idling will ignore APM IDLE if no APM available)
noMainADV:
	mov [cs:savingstrat],al
	; *** is this all that we have to do??? ***
	pop ax
	ret

; ---------------

idling:		; called whenever the system is idle!
	call uptime_update	; NEW 2/2005
	pushf
	;
	push ax
	xor ax,ax		; must use add, not inc: fixed 7/2007
	add word [cs:idleCalls], byte 1	; maintain the statistics
	adc [cs:idleCalls+2],ax	; ... which are 32 bit
	call getTime		; get timer tick time of now
	cmp ax,[cs:lastIdleTick]
	jz sameIdle		; still the same, do not count
	mov [cs:lastIdleTick],ax
	xor ax,ax
	add word [cs:idleTicks], byte 1	; the global counter
	adc word [cs:idleTicks+2],ax	; 16bit code ;-)
	push bx
	; idle stat buf order and whichIdling order: 2f!, 28, 16!, 2a
	; (according to int 2f.5481 documentation in RBIL version 61)
	mov bx,[cs:whichIdling]	; increment which special counter?
	dec bx		; make 0 based
	and bx,3	; limit to sane values
	add bx,bx	; multiply...
	add bx,bx	; ...to get offset in dword array
	add bx,idle2fCalls	; offset of array itself
	inc word [cs:bx]		; "idle for reason x counter"
	adc word [cs:bx+2],ax		; 16bit code ;-)
	pop bx
sameIdle:
	pop ax
	push cx
	; idlestrategy bits 0..3: IDLE16!, IDLE28, IDLE2F!, IDLE2A
	; but whichIdling for them: 2+1      1+1     0+1      3+1
	mov cx,[cs:whichIdling]	; get category of this idle event
	dec cx
	and cx,3
	test cl,1		; even value (16 / 2f)?
	jnz oddIdle
	xor cl,2		; swap order to get right bit offset
oddIdle:			; (according to PC DOS documentation)
	mov ch,1
	shl ch,cl		; find the flag bit for THIS category
	test [cs:IdleStrategy],ch
	pop cx
	jz nHLTing		; if category disabled, do not sleep now.
	;
	test byte [cs:savingstrat],2
	jz nAPMidling
	push ax
	mov ax,[cs:apmversion]
	cmp ax,0100h
	jb nAPMid
	mov ax,5305h	; tell "CPU idle"
	int 15h
nAPMid:	pop ax
nAPMidling:
	test byte [cs:savingstrat],1
	jz nHLTing
	sti
	hlt	; HLT alone can save energy already!
nHLTing:
	popf
	ret


uptime_update:			; guess what (preserves flags / regs)
	pushf
	push ax
	call getTime		; current time?
	push ax
	sub ax,[cs:lastOnTick]	; after the "last checked on..." tick?
	jc wrapWorkTime		; to be safe, ignore if wrapping
	jz wrapWorkTime		; we would add 0 - skip that
	add [cs:onTicks],ax		; add to accumulated uptime
	adc word [cs:onTicks+2],0	; ... which is a 32bit value
wrapWorkTime:
	pop ax
	mov [cs:lastOnTick],ax	; update in any case
	pop ax
	popf
	ret

working:			; for now, we only update the uptime!
	call uptime_update
%ifdef UNIDLECHECK
	pushf
	; *** we would increment a busy counter here...    ***
	; *** a timer tick handler might decrement it and  ***
	; *** trigger "idling" when the counter reaches 0  ***
	; ***  (the increment must be with "saturation")   ***
	; *** increment STATISTICS based on CALLER IP, too ***

	test byte [cs:savingstrat],2	; APM in use?
	jz nwAPM
	push ax
	mov ax,[cs:apmversion]
	cmp ax,100h
	jb nwAPM2
	mov ax,5306h	; tell "CPU busy"
	int 15h
nwAPM2:	pop ax
nwAPM:	popf
%endif	; UNIDLECHECK
	ret


getTime:	; get low word of timer tick count in AX
	push ds
	mov ax,40h
	mov ds,ax
	mov ax,[ds:6ch]	; current time
	pop ds
	ret

; ---------------

%ifdef UNIDLECHECK
i08:	pushf
	; *** ***
	; *** count down "working" count and call "idling" if 0
	; *** ***
	popf
ix08:	jmp far [cs:oldi08]

i10:	cmp ah,3
	jbe ix10
	call working
ix10:	jmp far [cs:oldi10]

i13:	call working
	jmp far [cs:oldi13]

i14:	cmp ah,3
	jz ix14
	call working
ix14:	jmp far [cs:oldi14]

i17:	cmp ah,2
	jz ix17
	call working
ix17:	jmp far [cs:oldi17]

i21:	cmp ah,4bh
	jnz ix21
	call working
ix21:	jmp far [cs:oldi21]

oldi08	dd -1
oldi10:	dd -1
oldi13:	dd -1
oldi14:	dd -1
oldi17:	dd -1
oldi21:	dd -1
%endif	; UNIDLECHECK

; not handling int 6c ...
; IRQ already handled by BIOS, by the way ...

; ---------------

	align 4
	db "int>"
oldi16:	dd -1
oldi28:	dd -1
oldi2a:	dd -1
oldi2f:	dd -1
	db "<int"

lastIdleTick	dw 0	; avoid counting one idle moment twice
lastOnTick	dw 0	; helps to count busy time
whichIdling	dw 0	; (byte) index to "what called idling?"

		db ">>"		; following buffer can be fetched through API
IdleStatBuf:
onTicks		dd 1	; timer ticks with CPU on (avoid div by 0)
idleTicks	dd 0	; timer ticks with CPU idle
idleCalls	dd 0	; calls to "idling" in total
idle2fCalls	dd 0	; calls to "idling" by int 2f handler
idle28Calls	dd 0	; calls to "idling" by int 28 handler
idle16Calls	dd 0	; calls to "idling" by int 16 handler
idle2aCalls	dd 0	; calls to "idling" by int 2a handler
		db "<<"

APMStatBuf:
resumeCount	dd 0	; count of resumes (found by APM polling?)


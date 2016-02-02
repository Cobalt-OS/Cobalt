 ; This file is part of CDRcache, the 386/XMS DOS CD-ROM cache by
 ; Eric Auer (eric@coli.uni-sb.de), based on LBAcache, 2001-2004.

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

	; Userint offers an user interface by supporting read
	; and write calls to our device. As CD-ROM drivers do
	; not support those calls, there is no conflict.
	; Functions are to be called from DISPATCH with valid
	; es:bx value. They are "readUI" and "writeUI".

	; *** The initial version simply writes feedback to
	; *** SCREEN, not to a buffer provided by the user.

; -------------------------------------------------------------

flushmsg	db 13,10,"Flushing this CDRcache.",13,10,0
clearmsg	db 13,10,"Resetting statistics of this CDRcache.",13,10,0
sleepmsg	db 13,10,"Putting this CDRcache to sleep.",13,10,0
wakeupmsg	db 13,10,"Waking this CDRcache up.",13,10,0

uihelpmsg	db 13,10,"CDRcache user commands:",13,10
		db "F = flush cache   C = clear statistics",13,10
		db "S = show stats    I = show tech. info   ? = show help",13,10
		db "Q = quiet mode    V = verbose mode      N = normal mode",13,10
		db "0 = stop caching  1 = continue caching",13,10,0

verbositymsg	db 13,10,"Setting CDRcache verbosity level to ",0
quietverbmsg	db "QUIET",13,10,0
normalverbmsg	db "NORMAL",13,10,0
verboseverbmsg	db "VERBOSE",13,10,0


	; special: have to show a string as argument (2 pmessage calls)
clientdevmsg	db 13,10,"CDRcache: Chained device is "
clientdevmsg2	db "????????",0

	; things to show in DECIMAL
infolist1	dw rdhit,rhitmsg+0x8000
		dw   rdmiss,rmissmsg+0x8000
		dw xmssize, xmsinfomsg+0x8000
		dw 0,0

rhitmsg		db "read  hits   in sectors:    ",0
rmissmsg	db "read  misses in sectors:    ",0
xmsinfomsg	db "cache size   in kiloBytes:  ",0

	; we get log2 of this information in 2a: hi(tabsz) 2b: lo(tabsz)
tabinf2a	db "sectors per bin in table:       ",0
tabinf2b	db "bytes   per bin in table:       ",0

	; things to show in HEXADECIMAL
infolist2	dw   hint, tabinf1+0x4000
		dw xmshandle, xmshandmsg+0x4000
		dw   xmsvec, xmsptrmsg+0x8000
		dw oldintr, clientdevimsg+0x8000
		dw   oldstra, clientdevsmsg+0x8000
		dw 0,0
		
tabinf1 	db "table offset in driver:       0x",0
xmshandmsg	db "Used XMS handle number:       0x",0
xmsptrmsg	db "Used XMS handler    is at 0x",0
clientdevimsg	db "Chained dev. intr.  is at 0x",0
clientdevsmsg	db "Chained dev. strat. is at 0x",0

	; special: extra processing for more human readable output
hrdmsg		db "MegaBytes  read in total:       ",0
hhitmsg		db "percentage of cache hits:       ",0


twocolumns	db 0	; *** 11/2002 start with one column
		; low bit: 0=crlf 1=tab after message
		; bit 1: 0=nothing 2=toggle low bit after message
showtech	db 0	; *** 5/2004: set to show tech info in stats

; -------------------------------------------------------------

copyclientname:
	push ax
	push si
	push bx
	mov si,clientname
	mov bx,clientdevmsg2
copynameloop:
	mov al,[cs:si]
	mov [cs:bx],al
	inc bx
	inc si
	or al,al	; we copy the trailing 0, too!
	jnz copynameloop
	pop bx
	pop si
	pop ax
	ret

; -------------------------------------------------------------

writeUI:		; currently allowed commands are:
			; F for FLUSH
			; S for STATISTICS (less verbose 5/2004)
			; I for INFO (detailled statistics - new 5/2004)
			; C to  CLEAR STATISTICS
			; 0 to  disable caching (sleep)
			; 1 to  enable caching (wakeup)
			; Q for QUIET LOGGING
			; N for NORMAL LOGGING
			; V for VERBOSE LOGGING
			; ? for HELP
			
	mov byte [cs:twocolumns],0	; one column mode
	call copyclientname

	push si		; we have: data pointer [...+0x0e], byte count
	push ax		; word [...+0x12], count also used for feedback.

	xor ax,ax		; AH nonzero means found any
	mov si,[es:bx+0x12]	; the length
	push es
	push bx
	les bx,[es:bx+0x0e]

wrUIscan:
	mov al,[es:bx]
	cmp al,'a'		; lower case?
	jb wrUIisupper
	sub al,0x20		; make upper case if lower case
wrUIisupper:
	or si,si
	jnz wrUIscanon		; still chars in buffer, scan on
	jmp wrUIscanned

wrUIscanon:
	cmp al,'F'		; F for FLUSH
	jnz wrUInoFfound
	call flush			; FLUSH all units
	mov ax,flushmsg			; message
	jmp wrUIfoundany

wrUInoFfound:
	cmp al,'S'		; S for STATISTICS
	jnz wrUInoSfound
	clc
	call readUI			; SHOW STATISTICS
					; NO extra message
	jmp wrUIendloop			; do NOT show client dev name

wrUInoSfound:
	cmp al,'I'		; I for INFORMATION and STATISTICS
	jnz wrUInoIfound
	stc
	call readUI			; SHOW INFORMATION / STATISTICS
					; NO extra message
	jmp wrUIendloop			; do NOT show client dev name

wrUInoIfound:
	cmp al,'C'		; C for CLEAR STATISTICS
	jnz wrUInoCfound
	push eax
	xor eax,eax
	mov [cs:rdhit], eax		; clear HIT count
	mov [cs:rdmiss], eax		; clear MISS count
	pop eax
	mov ax,clearmsg			; message
	jmp wrUIfoundany

wrUInoCfound:
	cmp al,'0'		; 0 for SLEEP
	jnz wrUIno0found
	or word [cs:sleeping],2		; GO TO deep SLEEP
	mov ax,sleepmsg			; message
	jmp wrUIfoundany

wrUIno0found:
	cmp al,'1'		; 1 for WAKE UP
	jnz wrUIno1found
	and word [cs:sleeping],0xfffc	; STOP all SLEEPING
	mov ax,wakeupmsg		; message
	jmp wrUIfoundany

wrUIno1found:
	cmp al,'Q'		; Q for QUIET
	jnz wrUInoQfound
	mov word [cs:verbosity],0	; MINIMUM verbosity
	mov ax,quietverbmsg+0x8000	; message with extra FLAG
	jmp wrUIfoundany

wrUInoQfound:
	cmp al,'N'		; N for NORMAL verbosity
	jnz wrUInoNfound
	mov word [cs:verbosity],1	; MEDIUM verbosity
	mov ax,normalverbmsg+0x8000	; message with extra FLAG
	jmp wrUIfoundany

wrUInoNfound:
	cmp al,'V'		; V for VERBOSE
	jnz wrUInoVfound
	mov word [cs:verbosity],2	; MAXIMUM verbosity
	mov ax,verboseverbmsg+0x8000	; message with extra FLAG
	jmp wrUIfoundany

wrUInoVfound:
	cmp al,'?'		; ? for HELP
	jnz wrUIendloop
	mov ax,uihelpmsg		; message

wrUIfoundany:
	push si
	mov si,ax
	test si,0x8000			; extra FLAG set?
	jz wrUInormalmsg
	push si
	mov si,verbositymsg		; then show extra message!
		call pmessage
	pop si
	and si,0x7fff			; strip the FLAG
wrUInormalmsg:
		call pmessage
	pop si
	or ah,1

wrUIendloop:
	inc bx
	dec si
	jmp wrUIscan		; loop around

wrUIscanned:
	pop bx
	pop es

	or ah,ah		; any valid command encountered?
	jz wrUInonefound	; if not, no extra message
	mov si,clientdevmsg	; tell which client driver we serve
		call pmessage
wrUInonefound:
	pop ax
	pop si
	ret

; -------------------------------------------------------------

readUI:			; show information about cache status
			; up to word [...+0x12] bytes can be returned
			; to the user buffer, pointer is at [...+0x0e].

	mov al,0	; *** verbose if called with carry flag set (5/2004)
	adc al,0		; set if carry is set
	mov [cs:showtech],al	; whether to show tech info

	call copyclientname

	pusha			; (currently only on screen using BIOS TTY)
	push eax
	push es
	push ds

	cmp word [es:bx+0x12],3	; at least 3 bytes space?
	jb lazyreadUI		; if not, user reads 0-2 bytes trash.
	mov word [es:bx+0x12],2	; give the user 2 bytes...
	les bx,[es:bx+0x0e]	; get buffer pointer
	mov word [es:bx],"Ok"	; give the user some "okay" :-)
	mov byte [es:bx+2],0	; for stupid users
lazyreadUI:
	

	mov bx,cs
	mov ds,bx
	mov es,bx		; if ES unequal CS, show info
				; about OTHER driver instance

	; the device call ES:BX is now no longer visible in readUI,
	; but [cs:pb] still has it.

; -------------------------------------------------------------

; ***	mov byte [cs:twocolumns],3	; start with 2 column mode ***

	mov si,clientdevmsg	; (copyclientname has copied the name there)
		call pmessage	; announce tell client name

%if 1
	mov ax,[es:sectors]	; number of sectors
		movzx eax,ax	; clear high half <<<
	shl eax,1		; turn into kB: *2048* by/sector
	mov [es:xmssize],eax	; update value, just in case...!
%endif

; -------------------------------------------------------------

	mov bx,infolist1	; a list of what we want to show in dec
infoloop1:
	mov si,[cs:bx]		; where to read
	or si,si
	jz infodone1
	mov eax,[es:si]		; read a value from driver
	inc bx
	inc bx
	mov si,[cs:bx]		; select a message
	test si,0x8000		; 32 bit?
	jnz info32list1
	movzx eax,ax		; mask to 16 bit otherwise
	test si,0x4000		; 16 bit?
	jnz info16list1
	xor ah,ah		; mask to 8 bit otherwise
info16list1:
info32list1:
		call hex2dec	; convert EAX to packed BCD
		call pmessage	; show the message and AX/EAX
	inc bx
	inc bx
	jmp short infoloop1	; go on with next info

infodone1:

; ----------------------

tablehints:			; decode table property description bits
	mov al,[cs:showtech]	; *** verbose mode active? (5/2004)
	or al,al
	jz infodone2		; *** else skip table info and tech info

	mov ax,[es:tabsz]	; READ VALUE FROM DRIVER
	and ax,0x0f0f		; ignore undefined bits
	push cx
	push ax
	mov cl,ah		; log2 of sectors per bin
	xor eax,eax
	inc eax
	shl eax,cl
		call hex2dec	; convert to packed BCD
	mov si,tabinf2a+0x4000	; show a 4 digit value
		call pmessage	; show the message and AX
	pop ax
	mov cl,al		; log2 of bytes per bin
	xor eax,eax
	inc eax
	mov ax,1
	shl eax,cl
		call hex2dec	; convert to packed BCD
	mov si,tabinf2b+0x4000	; show a 4 digit value
		call pmessage	; show the message and AX
	pop cx

; ----------------------

	mov bx,infolist2	; a list of what we want to show in hex
infoloop2:
	mov si,[cs:bx]		; where to read
	or si,si
	jz infodone2
	mov eax,[es:si]		; read a value from driver
	inc bx
	inc bx
	mov si,[cs:bx]		; select a message
		call pmessage	; show the message and AX/EAX
	inc bx
	inc bx
	jmp short infoloop2	; go on with next info

infodone2:

; ----------------------

; *** 8/02: added human readable display

	push edx
	push ebx

humanreadable:
	mov eax,[es:rdhit]	; read hits
		push eax	; * save hits for percent
	xor edx,edx
	add eax,[es:rdmiss]	; read misses added
	adc edx,edx
		push eax	; * save (low part of) sum for percent

	mov ebx,512		; 512 CD-ROM sectors are 1 MegaByte
	div ebx			; transform (sum!) to MBytes
	xor edx,edx
		call hex2dec	; transform to packed BCD
	mov si,hrdmsg+0x4000	; human readable read message, 16bit
	test eax,0xffff0000	; need 32bit?
	jz hrd16
hrd32:	xor si,0xc000		; 16->32bit display size
				; (flags 0x4000 instead of 0x8000)
hrd16:		call pmessage

		pop eax		; * total
	mov ebx,eax
		pop eax		; * hits
		push ebx	; * total (>= hits)
	mov ebx,100		; for percent *** BUG fixed
	xor edx,edx
	mul ebx			; *** BUG was: did use edx=100
		pop ebx		; * total
; ???	xor edx,edx		; brute overflow blocking !!!

; ???	ror ebx,1		; rounding (no overflow check)
; ???	add eax,ebx		; add total/2 to 100*hits
; ???	stc
; ???	rcl ebx,1		; restore total count, do "or 1"
				; now ebx is an access count > 0
	or ebx,1		; *** instead of the above round-add

				; EDAX=100*hits EBX=(hits+misses)|1
	div ebx			; calculate percent

	cmp eax,100		; clip to 100%
	jb hrd99
hrd100:	mov eax,100
	jmp short hrdmax
hrd99:		aam		; AAM: ah=al div 10, al=al mod 10
		shl ah,4
		or al,ah	; now we have packed BCD
		xor ah,ah	; clear high byte
hrdmax:
	mov si,hhitmsg+0x4000	; percentage message, 16bit <<<
		call pmessage

	pop ebx
	pop edx

; ----------------------

leavethis:		; all status info has been shown to the user
	pop ds
	pop es
	pop eax
	popa
	ret

; -------------------------------------------------------------

pmtty:			; shows char AL on screen destroys AH
			; *** TODO: output to user buffer, too!?
	mov ah,0eh	; TTY
	push bp		; >
	push bx		; >
	mov bx,07	; page 0, color 7 if any
	int 10h		; VIDEO BIOS: TTY OUTPUT CHAR
	pop bx		; <
	pop bp		; <
	ret

; ----------------------

pmessage:		; show message at (si and 0x3fff) and maybe
	push dx		; value of ax or eax (ax if si test 0x4000 nz,
	push eax	; eax if si test 0x8000 nz), and then CRLF
	push ds		; *** NEW 11/2002: twocolumns may skip CRLF
	  push cs	; *** if on, every other CRLF will be a TAB
	  pop ds	; *** IF you turn twocolumns OFF (and ~2),
	  push ax	; *** AND the "test ... 1" is ZR, then you
			; *** have to CRLF before the next string.
	  push si
	and si,0x3fff	; mask out our flags (offset is max 16k)
pmloop:	mov al,[si]
	or al,al
	jz pmloopdone
	inc si
	call pmtty	; TTY CHAR
	jmp short pmloop
pmloopdone:
	  pop si

	  pop ax
	test si,0xc000	; any bytes to show? <<<
	jz pmdone	; if none, done
	  mov dl,cl	; save CL
	  push ax
	mov ax,si
	shr ax,12	; how many nibbles to show
	and al,12	; only 4/8/12, which is    <<<
			; AX EAX ? (2, 4, 6 bytes) <<<
	mov dh,al	; as a counter
	mov cl,8	; max 8 nibbles
	sub cl,dh	; how many nibbles not to show
	shl cl,2	; nibbles -> bits
	  pop ax
	shl eax,cl	; make EAX "left-bound"

	push si
	and si,0x3fff	; mask out our flags (offset is max 16k)
scanhx:	inc si		; find end of string
	cmp byte [si],0
	jnz scanhx
	cmp word [si-2],'0x'	; did string end with "0x"?
	pop si
	mov cl,'*'	; *** do not suppress leading zeroes for HEX
	jz showeaxlp
	mov cl,'0'	; *** start suppressing leading zeroes for DEC


showeaxlp:
	rol eax,4	; move digit to show in lowest pos
	  push ax
	and al,0x0f
	add al,'0'	; hex -> ascii
	cmp al,'9'
	jbe nothex
	add al,7	; 'A'-('9'+1)

nothex:	cmp al,cl	; *** leading zero to be suppressed?
	jnz notzero
	mov al,'_'	; *** SUPPRESS leading zero!
	jmp short waszero
notzero:
	mov cl,'*'	; *** stop suppressing leading zeroes
waszero:

	mov dl,al
	call pmtty	; TTY CHAR
	  pop ax

	cmp dh,2	; *** last digit to follow now?
	jnz notend
	mov cl,'*'	; *** do not ever suppress last digit...
notend:

	dec dh
	jnz showeaxlp	; on to the next digit

	mov cl,dl	; restore CL

pmdone:	test byte [cs:twocolumns],1	; TAB instead of CRLF ?
	jz pmdocrlf		; use CRLF
	mov al,9
	call pmtty	; TTY CHAR
	jmp short pmdonecrlf

pmdocrlf:
	mov al,13	; CR
	call pmtty	; TTY CHAR
	mov al,10	; LF
	call pmtty	; TTY CHAR
pmdonecrlf:
	test byte [cs:twocolumns],2	; toggle mode on?
	jz pmnotoggle
	xor byte [cs:twocolumns],1	; toggle TAB <-> CRLF
pmnotoggle:

	pop ds
	pop eax
	pop dx
	ret

; -------------------------------------------------------------

hex2dec:		; take a longint eax and convert to packed BCD
	push ebx	; overflow handled (0xffffffff has 10 digits)
	push ecx
	push edx
	push edi
	xor edx,edx
	xor ecx,ecx	; shift counter
	xor edi,edi	; result
	mov ebx,10
h2dlp:	div ebx		; remainder in edx, 0..9
	shl edx,cl	; move to position
	or edi,edx	; store digit
	xor edx,edx	; make edx:eax a proper 64bit value again
	or eax,eax	; done?
	jz h2dend
	add cl,4	; next digit
	cmp cl,32	; digit possible?
	jae h2doverflow	; otherwise overflow
	jmp short h2dlp
		
h2dend:	mov eax,edi	; return packed BCD
	pop edi
	pop edx
	pop ecx
	pop ebx
	ret

h2doverflow:
	mov edi,0x99999999
	jmp short h2dend

; -------------------------------------------------------------


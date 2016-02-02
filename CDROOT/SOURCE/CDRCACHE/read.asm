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


	; main read handling function:
	; we can ignore the interleave setting, as it only affects
	; the data read style, not which data is read. Parameters:
	; word [es:bx+0x12] sector count
	; word [es:bx+0x0e] data pointer
	; word [es:bx+0x14] sector number
	; byte [es:bx+1]    subunit number (already in DL)


%ifdef DBGcnt
%define TSTCX call tstcx2
cxmsg	db ' cx',0
tstcx2:	pushf
	push ax
	mov ax,cx
		push word cxmsg
		call meep
	pop ax
	popf
	ret
%else
%define TSTCX nop
%endif

%ifdef DBGcnt
%define TSTBX call tstbx2
bxmsg	db ' bx',0
tstbx2:	pushf
	push ax
	mov ax,bx
		push word bxmsg
		call meep
	pop ax
	popf
	ret
%else
%define TSTBX nop
%endif

; ---------------------------------------------------------------

READ:		; the main "READ LONG" thing

	STACKDEBUG
	cmp word [cs:rwbusy],0	; nesting protecion
	jnz lrnested
	inc word [cs:rwbusy]	; nesting protection

	call readmain	; **** PURE READ HANDLER ****
			; (no longer has status returned in AH,
			;  returns status in device style instead)

	dec word [cs:rwbusy]	; <- nesting protection
lrdone:	STACKDEBUG
	ret

lrnested:
	mov byte [es:bx+3],0x0c		; "general failure"
	or word [es:bx+3],0x8200	; error, busy
	mov word [es:bx+0x12],0		; "zero sectors read" (do we
					; ...have to return a value?)
	push word nesterr
		call meep	; warn
	ret

; ---------------------------------------------------------------

align 4

; (we could use the given data block rather than an own one...)

readrequest:	; device request data block to allow us to ask
		; the CD-ROM driver to read things for us.
	db 0x1b	; [00] length - CONST for use with "read long"
rdUnit	db 0	; [01] <<< subunit
	db 0x80	; [02] command - CONST for "read long"
rdStat	dw 0	; [03] <<< status - returned by driver
	db 0,0,0,0, 0,0,0,0	; reserved
	db 0	; [0d] addressing mode - CONST for "linear / HSG"
rdPtr	dd 0	; [0e] <<< buffer pointer
rdCnt	dw 0	; [12] <<< sector count
rdSec	dd 0	; [14] <<< sector number
	db 0	; [18] read mode - CONST for "cooked 2048 byte sectors"
	db 1	; [19] (interleave size)
	db 0	; [1a] (interleave skip)
	db 0	; (pad for alignment)

; ---------------------------------------------------------------

		; The main reading loop, using CD-ROM driver
		; data structures (ES:BX has a buffer, DL the drive).
readmain:	; Inside, it uses a LOCAL data structure. So avoid
		; nesting. Could alternatively use ES:*SI*, but not
		; BX, as BX has other use here inside...!
		; Local fields: rdUnit, rdStat, rdPtr, rdCnt, rdSec...

	; *** Update (Jan. 2002): we count cache misses and
	; *** pool the actual disk reads (no more speed loss...) !

;--	push word [es:bx+0x10]	; store original buffer SEGMENT
;--	push dword [es:bx+0x14]	; store original sector number

		pusha
		push eax

	mov al,[es:bx+1]	; subunit
	mov dl,al		; (init DL)
	mov [cs:rdUnit],al
	;
	mov eax,[es:bx+0x0e]	; pointer
	mov [cs:rdPtr],eax
	;
	mov ax,[es:bx+0x12]	; count
	mov [cs:rdCnt],ax
	;
	mov eax,[es:bx+0x14]	; sector
	mov [cs:rdSec],eax

	mov si,bx

	mov cx,[cs:rdCnt]	; sector count
	TSTCX
	xor bp,bp		; COUNT of read sectors...
				; @@@       @@@
		xor bx,bx	; @@@ COUNT of CACHE MISSES
				; @@@       @@@
	jcxz lrd_done
	mov ah,0		; () status: OK (still in use???)
	mov di,ax		; this will store our AX value!

lrd_lp: mov eax,[cs:rdSec]	; load sector number
		call findbin	; EAX DL found in cache?
	jc short notcached	; if not found, read from disk

cached:		call REGDUMP		; @@@    @@@
		call POOLED_READS	; @@@ do BX actual disk reads
					; @@@    @@@
		jc lrd_done	; @@@
	inc dword [cs:rdhit]	; statistics: cache HIT

		push es
		push bx
	mov bx,[cs:rdPtr]	; buffer offset
	mov es,[cs:rdPtr+2]	; buffer segment
		call copytodos	; *** if found, read data from XMS bin AX
		pop bx
		pop es

		and di,0x00ff	; () status (high half): OK
	jmp short nextlrd	; on to the next sector

notcached:
		inc bx		; @@@ COUNT but do NOT read from disk yet
				; @@@       @@@
	inc dword [cs:rdmiss]	; statistics: cache MISS

; @@@	push cx		; <- SAVE COUNT
; @@@	mov cx,1		; *** do ONE sector at a time!

; @@@		mov ax,di	; ??? have some AL :-)
; @@@		call readfromdisk	; *** CX from [si+8]l.DL to
					; [si+6]w:[si+4]w
; @@@		mov di,ax	; () status: FROM CALL
			; COUNT assumed to be 1 iff NC here
			; - or read from CX !
; @@@	pop cx		; <- RESTORE COUNT
; @@@	jc lrd_done		; the first error ends the call!

; @@@	mov eax,[cs:rdSec]	; load sector number
; @@@		call newbin	; find a space in XMS -> bin AX

; @@@		push es
; @@@		push bx		; @@@
; @@@	mov bx,[cs:rdPtr]	; buffer offset
; @@@	mov es,[cs:rdPtr+2]	; buffer segment
; @@@		call copytoxms	; copy data to XMS bin AX
; @@@		pop bx		; @@@
; @@@		pop es

nextlrd:
	inc bp		; COUNT: one more sector read ok
	add word [cs:rdPtr+2],0x80	; *2048*/16: next buffer seg
					; (depends on sector size)
	inc dword [cs:rdSec]		; next sector
	loop lrd_lp
		call REGDUMP		; @@@    @@@
		call POOLED_READS	; @@@ do BX actual disk reads
					; @@@    @@@
lrd_done:
	mov [cs:rdCnt],bp	; return our sector COUNT
	jnc lrd_success

lrd_failure:
		pop eax
		popa

	test word [es:bx+3],0x8000	; error already described?
	jnz lrd_specificfailure
	mov byte [es:bx+3],0x0b		; read fault
	or word [es:bx+3],0x8200	; error, busy
lrd_specificfailure:
	mov word [es:bx+0x12],0		; "zero sectors" (need this??)
					; (use [cs:rdCnt] instead?)
	jmp short lrd_return

lrd_success:
		pop eax
		popa

	and word [es:bx+3],0x7d00	; clear error codes
					; (could copy [cs:rdCnt]...)

lrd_return:
;--	pop dword [es:bx+0x14]	; restore original sector number
;--	pop word [es:bx+0x10]	; restore original buffer SEGMENT

	ret			; done...

; ---------------------------------------------------------------

	; @@@ This is used for the new (Jan. 2002) multi
	; @@@ sector read pooling feature. Registers:
	; @@@ CS:readrequest -> data structure
	; @@@ (DL -> drive)
	; @@@ DI -> current return value (for AX) (*** ??? ***)
	; @@@ BP -> count of ok sectors (including the sectors
	; @@@  -we- should do, so we must sub them on failure)
	; @@@ (plus CX to do count, EAX sector, ...)

	; @@@ This must handle BX being 0, >>> return CY on
	; @@@ error <<< and do the proper rollback in DS[SI],
	; @@@ containing ptr/[4] to buffer, dword/[8] sector,
	; @@@ and other stuff that we do not use here (like
	; @@@ on word [2]the external given/returned count)...

POOLED_READS:		; no checks here for count or buffer pointer
	clc		; overflows, as those have been done earlier.
	TSTBX
	or bx,bx
	jz near pore_end

pore_real:		; we set DS:SI, DL, CX (count) and get
	push ax		; AX, CX (count), CY/NC back...
	push bx		; unless there is an error, we do ***NOT***
	push cx		; check the count, and if there is, we
			; pretend complete failure. What the heck...

	push word [cs:rdPtr+2]	; buffer position (segment)
	push dword [cs:rdSec]	; sector number

	mov cx,bx
	shl cx,7		; mul (*2048*>>4), thus mul 0x80
				; (depends on sector size!)
	sub [cs:rdPtr+2],cx	; move BUFFER back (seg)
	push eax
	movzx eax,bx
	sub [cs:rdSec],eax	; move SECTOR back
	pop eax
	mov cx,bx		; the COUNT

	mov ax,di		; status
	call readfromdisk	; *** REAL disk read this time
				; CX from [cs:rdSec]l.DL to [cs:rdPtr]p
	mov di,ax		; status (in AH)

	pop dword [cs:rdSec]
	pop word [cs:rdPtr+2]  

	pop cx
	pop bx
	pop ax

	jc pore_err

	; next, we do all the rollback AGAIN to copy all to XMS !!!
	; data which we just fetched from disk is copied to cache.

pore_xms:
	push eax
	push cx

	push word [cs:rdPtr+2]	; buffer position (seg)
	push dword [cs:rdSec]	; sector number

	mov cx,bx
	shl cx,7		; mul (*2048*>>4), thus mul 0x80
				; (depends on sector size!)
	sub [cs:rdPtr+2],cx	; move BUFFER back
	push eax
	movzx eax,bx
	sub [cs:rdSec],eax	; move SECTOR back
	pop eax
	mov cx,bx		; the COUNT

pore_nextxms:
	mov eax,[cs:rdSec]	; load sector number
		call newbin	; find space in XMS -> bin AX

	push es
	push bx
	mov bx,[cs:rdPtr]	; buffer offset
	mov es,[cs:rdPtr+2]	; buffer segment
		call copytoxms	; *** copy data to XMS bin AX
	pop bx
	pop es

	add word [cs:rdPtr+2],0x80	; *2048*/16: next buffer position
	inc dword [cs:rdSec]		; next sector
	loop pore_nextxms

	pop dword [cs:rdSec]
	pop word [cs:rdPtr+2]  

	pop cx
	pop eax
	
pore_end:
	xor bx,bx	; we must RESET the COUNT of postponed reads!
	clc
	ret

pore_err:
	sub bp,bx	; correct count: less sectors were really
	xor bx,bx
	stc		; ok than planned while postponing reads!
	ret
	

; ---------------------------------------------------------------

		; Read from real CD-ROM, uses device driver request
		; data structure: CX sectors from [cs:rdSec].(DL)
readfromdisk:	; to (far) [cs:rdPtr],
		; returns status CF (*** and AX ??? ***)
		; and count CX, plus status in [cs:rdStat]
		; Called in case of a CACHE MISS.

	push word [cs:rdCnt]
	mov [cs:rdCnt],cx	; number of sectors to do

%if 0 ; EXTRA DEBUGGING 10/2003
	mov ax,cx
	push word readDBI
	call meep
%endif

	push es
	push bx
	mov bx,cs
	mov es,bx
	mov bx,readrequest	; ES:BX now points to data structure
	call far [cs:oldstra]	; call "strategy" of CD-ROM driver
	call far [cs:oldintr]	; call "interrupt" of CD-ROM driver
	pop bx
	pop es

	mov cx,[cs:rdCnt]	; return number of sectors read!
	pop word [cs:rdCnt]


%if 0 ; EXTRA DEBUGGING 10/2003
	push ax
	mov ax,cx
	push word readDBX
	call meep
	mov ax,[cs:rdStat]
	push word readDBG
	call meep
	pop ax
%endif

	test word [cs:rdStat],0x8000	; error?
	jz readfromdisk_success

readfromdisk_fail:
	mov ah,[cs:rdStat]	; error code
	xor cx,cx		; "zero sectors successfully read"
	mov al,cl		; *** count? (is this still needed???)
	stc
	ret

readfromdisk_success:
	mov ah,0		; no error
	mov al,cl		; *** count? (is this still needed???)
	clc
	ret

; ---------------------------------------------------------------

rwbusy	dw 0			; nesting counter

nesterr	db "CDRcache: READ nesting: ",0


%if 0 ; EXTRA DEBUGGING 10/2003
readDBI db " calling readfromdisk ",0
readDBG	db " readfromdisk status ",0
readDBX db " readfromdisk count ",0
%endif



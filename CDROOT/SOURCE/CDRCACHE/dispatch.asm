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


 ; The main DISPATCH routine is called with es:bx being a pointer
 ; to the device call parameter block. For READ LONG, the cache
 ; is triggered. The cache also has own handlers for char INPUT,
 ; char OUTPUT (both for the "userint" user interface), init (init
 ; calls install in "setup" directly, not done here), IOCTL INPUT
 ; (traps "get device headers" and "media change"), IOCTL OUTPUT
 ; (traps "eject", "lock/unlock", "reset" and "close tray"),
 ; READ LONG PREFETCH (replaced by no-op to avoid seeks to data
 ; which is cached), and if required others as well. When the
 ; CARRY FLAG IS CLEAR on return from DISPATCH, the request will
 ; be passed through to the CD-ROM driver as is. Otherwise, when
 ; the CARRY FLAG IS SET, the cache returns directly to the caller.

%define TRACE 1		; show all calls (only for debugging!)
%define TAMETRACE 1	; do NOT show calls "read long" and "media check"
%define VERBOSE 1	; show not yet supported calls

; --------------

localsp	dw 0	; is nonzero if we have a local stack
localss	dw 0	; local ss (is nonzero while stack is busy)
othersp dw 0
otherss dw 0

sleeping	dw 0	; *** set to 1 from the first write until the next
			; *** disk change. TODO: use a bit mask for use
			; *** with multiple sub-units!
			; *** bit 2 currently used for user-requested sleep

			; for throttling:
lastread	dw 0	; time at which user interface was last read

verbosity	dw 1	; verbosity level (may change in the future)
			; 0 quiet   1 "normal"   2 "verbose" (trace)

; --------------

DISPATCH:	; At most ES and BX may be changed (check datahead.asm)

	test word [cs:localsp],0xffff	; local stack available?
	jz care_nostack
	test word [cs:localss],0xffff	; local stack already busy?
	jz care_stack
		push ax
		mov ax,[cs:otherss]
		push word staknesterr
		call meep
		pop ax
	clc				; we cannot handle this!
	ret				; back to "chain to CD-ROM"

care_stack:
	mov [cs:localss],cs		; prepare...
	mov [cs:otherss],ss		; ...to use...
	mov [cs:othersp],sp		; ...our local...
	lss sp,[cs:localsp]		; ...stack!

care_nostack:
	push eax			; give us some freedom
	push dx

; --------------

dispatch_main:
	mov dl,[es:bx+1]		; the driver subunit ("drive")
	mov al,[es:bx+2]		; the command code

%if TRACE
	cmp word [cs:verbosity],2	; HIGH verbosity?
	jb untrace1
	mov ah,dl			; (subunit, for debugging message)
	cmp al,3			; hide "ioctl read" - has own message
	jz untrace1
	cmp al,0x0c			; hide "ioctl write" - has own message
	jz untrace1
%if TAMETRACE
	cmp al,0x80			; hide "read long" - occurs often
	jz untrace1
	cmp al,5			; hide "char peek" - before every R/W
	jz untrace1
	cmp al,8			; hide "write char" - for every char
	jz untrace1
%endif
		push word tracemsg
		call meep
untrace1:
%endif

	; word [es:bx+3], status, already set to "done, no error")
	; byte [es:bx+0], request header length, not yet needed.

	cmp al,3			; IOCTL READ (CD-ROM status)
	jz near ioctlread
	cmp al,4			; CHARACTER READ
	jz charread
	cmp al,5			; CHAR PEEK (seems to be EOF indicator)
	jz BE_BUSY
	cmp al,6			; INPUT STATUS
	jz BE_BUSY			; we are always at EOF!?
	cmp al,7			; INPUT FLUSH
	jz inputflush
	cmp al,8			; CHAR OUTPUT
	jz charwrite
	cmp al,0x0a			; OUTPUT STATUS
	jz dispatchdone_jump		; we are always ready for the user
	cmp al,0x0b			; OUTPUT FLUSH
	jz dispatchdone_jump		; we can simply say "done" here
	cmp al,0x0c			; IOCTL WRITE (CD-ROM control)
	jz ioctlwrite

	; we ignore 0x0d / 0x0e "open"/"close" and some other calls

	cmp al,0x80			; READ LONG
	jz near cdromread
	cmp al,0x82			; PREFETCH for READ LONG
	jz dispatchdone_jump		; we swallow this to avoid seeks

	; we do nothing about 0x83 SEEK and 0x84/0x85/0x88 AUDIO
	; seek uses [..+0x0d] and [..+0x14] just like "read long",
	; prefetch works like "read long" but does not copy data...

	cmp al,0x86			; WRITE LONG
	jz cdromwrite
	cmp al,0x87			; WRITE LONG with VERIFY
	jz cdromwrite

	; everything else is passed on as is to the CD-ROM driver
	; so that it can take care for it itself.
	; *** Should we trap int 13h EDD eject/locking? It can be
	; *** triggered through command 13h (and 19h?), generic IOCTL,
	; *** subcommands 48h/49h.

	jmp DISPATCH_CHAIN

; --------------

BE_ERROR:
	or word [es:bx+3],0x820b	; "error: read fault"
BE_BUSY:
	or word [es:bx+3],0x200		; set "busy" bit
dispatchdone_jump:			; keep jumps short
	jmp DISPATCH_DONE


charread:
	call checktime
	jc BE_ERROR			; refuse access if < 2 seconds ago

	call readUI			; let user interface tell things

	jmp DISPATCH_DONE


charwrite:
	call writeUI			; let the user tell us things

	jmp DISPATCH_DONE


inputflush:				; CD-ROM drivers do it like that...
	call flushone			; *** really flush only one subunit?
	jmp DISPATCH_CHAIN		; let CD-ROM flush, too


cdromwrite:				; request header works like for read
					; but there are more raw modes...
%if VERBOSE
	cmp word [cs:verbosity],1	; NORMAL verbosity?
	jb unverbose1
	mov ah,dl			; show which subunit is affected
	push word writemsg		; have to disable cache for a while
	call meep
unverbose1:
%endif
	or word [cs:sleeping],1		; sleep -  *** TODO: one subunit only
	call flushone			; flush - only written subunit.
	jmp DISPATCH_CHAIN

; --------------

ioctlwrite:
	push ds
	push di			; not used: word [es:bx+0x12] size of buffer
	lds di,[es:bx+0x0e]	; buffer with control data
	mov ax,[ds:di]		; the calls that we want to trap
	pop di			; never use more than 2 control bytes
	pop ds			; AL is the command, AH the parameter.
%if TRACE
	cmp word [cs:verbosity],2	; HIGH verbosity?
	jb untrace2
	push word ioctlwrmsg
	call meep
untrace2:
%endif
	cmp al,2		; IOCTL WRITE 2: RESET
	jz ioctlreset
	cmp al,0		; IOCTL WRITE 0: EJECT
	jz ioctleject

	; *** we could also trap the following: 5 close tray
	; *** 1 lock/unlock (parameter: 0 unlock 1 lock).

	jmp DISPATCH_CHAIN	; let CD-ROM driver do the actual thing

ioctlreset:
%if VERBOSE
	cmp word [cs:verbosity],1	; NORMAL verbosity?
	jb unverbose2
	mov ah,dl		; (subunit, for debugging message)
	push word resetmsg
	call meep
unverbose2:
%endif
	jmp short ejectORreset

ioctleject:
%if VERBOSE
	cmp word [cs:verbosity],1	; NORMAL verbosity?
	jb unverbose3
	mov ah,dl		; (subunit, for debugging message)
	push word ejectmsg
	call meep
unverbose3:
%endif
ejectORreset:
	call flushone		; (note that if eject fails, we
				;  would not have had to flush!)
				; (not worth checking, though)
	jmp DISPATCH_CHAIN	; let CD-ROM reset/eject, too



; --------------

ioctlread:
	push ds
	push di			; not used: word [es:bx+0x12] size of buffer
	lds di,[es:bx+0x0e]	; buffer with control data
	mov al,[ds:di]		; Get query type
	pop di
	pop ds

%if TRACE
	cmp word [cs:verbosity],2	; HIGH verbosity?
	jb untrace3
	mov ah,dl		; (subunit, for debugging message)
%if TAMETRACE
	cmp al,9		; hide "media (changed) check" - occurs often
	jz untrace3
%endif
	push word ioctlrdmsg
	call meep
untrace3:
%endif

	cmp al,0		; IOCTL READ: get driver headers
	jz getdriverheaders	; we MUST trap this or *CDEX bypasses us!
	cmp al,9
	jz getmediachanged	; we MUST trap this to be able to wake up.

	; *** We could also trap, among others: 1, get seek pos, 6 get
	; *** status, 7 get sector size, 8 get volume size, 0x0a get
	; *** audio info, 0x0e get UPC code / EAN number...

gochain:
	jmp DISPATCH_CHAIN	; let CD-ROM driver reply for us

getdriverheaders:
	cmp word [es:bx+0x12],5	; buffer big enough?
	jb gochain		; if not, let CD-ROM driver complain for us.
	push es
	push bx
	les bx,[es:bx+0x0e]
	mov word [es:bx+1],0	; our header offset
	mov [es:bx+3],cs	; our header segment
	pop bx
	pop es
	jmp DISPATCH_DONE

getmediachanged:
	cmp word [es:bx+0x12],2	; buffer big enough?
	jb gochain		; if not, let CD-ROM driver complain for us.
	; *******************
	call far [cs:oldstra]	; tell CD-ROM driver the request pointer
	call far [cs:oldintr]	; QUERY the CD-ROM driver
	; *******************
	test word [es:bx+3],0x8200	; did driver return error or busy?
	jnz mediachangederror
	push es
	push bx
	les bx,[es:bx+0x0e]
	mov al,[es:bx+1]	; get the returned media change status
	mov ah,dl		; (subunit, for debugging messages)
	; byte [1] can have values: 0 unknown, 1 unchanged -1 changed
	pop bx
	pop es
	cmp al,1		; Medium unchanged? Fine!
	jz mcdone
				; else unknown or even changed!
				; changed also happens on 1st access!?
%if VERBOSE
	cmp word [cs:verbosity],2	; HIGH (was: NORMAL) verbosity?
	jb unverbose4
	push word changedmsg
	call meep
unverbose4:
%endif
	call flushone		; flushing subunit DL should be enough.
	test word [cs:sleeping],1	; *** TODO: check for subunit
	jz short mcdone			; *** only NORMAL sleep ends here

	and word [cs:sleeping],0xfffe	; if we were sleeping, we wake up.
%if VERBOSE
	cmp word [cs:verbosity],1	; NORMAL verbosity?
	jb unverbose5
	push word unwritemsg
	call meep
unverbose5:
%endif
	
	jmp short mcdone


mediachangederror:		; this can simply mean "no CD-ROM in drive"

; *** %if 0
; ***	push word mcerrmsg
; ***	call meep
; *** %endif

	call flushone		; cannot harm to flush the subunit in that case.

mcdone:	jmp DISPATCH_DONE


; --------------

cdromread:
	test word [cs:sleeping],3	; ANY sleep (normal / user)?
	jnz cdreadsleeping		; if sleeping, bypass cache

cdrealread:
	cmp byte [es:bx+0x0d],0		; linear HSG address?
	jnz readuncached		; mm:ss:ff RedBook not cached
	cmp byte [es:bx+0x18],0		; sector size cooked 2048 bytes?
	jnz readuncached		; raw 2352 byte sectors not cached
	cmp byte [es:bx+0],0x1b		; request header long enough?
	jb readuncached			; do not cache, will fail anyway.

	; We ignore bytes [...+19h] and [...+1ah] interleave setting
	; as they have no influence on WHAT data is read, only on HOW.
	; We DO USE data pointer [...+0eh], sector count word [...+12h]
	; and sector number doubleword [...+14h], however, as well as
	; DL aka. byte [...+1] subunit/drive number.

	call READ		; ************** DO CACHED/NORMAL READ

	jmp DISPATCH_DONE

readuncached:
%if VERBOSE
	cmp word [cs:verbosity],1	; NORMAL verbosity?
	jb unverbose6
	mov ah,dl		; (subunit, for debugging message)
	mov al,[es:bx+0x0d]	; AL low nibble 1 if RedBook is to blame
	shl al,4
	or al,[es:bx+0x18]	; AH high nibble 1 if raw read is to blame
	or ax,ax		; neither to blame?
	jnz readuc2
	mov al,[es:bx+0]	; request header length
	mov ah,0x0ee		; error code
readuc2:
	push word formatmsg	; we cannot serve this from cache
	call meep
unverbose6:
%endif
cdreadsleeping:
	jmp DISPATCH_CHAIN	; let real CD-ROM driver do the read

; --------------

checktime:			; return CY if [cs:ax] < 1 second ago
	push es
	push bx
	mov bx,0x40		; BIOS data segment
	mov es,bx
	mov bx,[es:0x6c]	; low word of timer
	mov ax,[cs:lastread]	; current value
	mov [cs:lastread],bx	; update value to current timer value
	sub bx,ax		; time elapsed since last access
	cmp bx,36		; less than 2 seconds ago? then set CY.
	pop bx
	pop es
	ret

; --------------

%if 0

fishyfishy:	; UNINSTALL when something scary has happened
	mov word [cs:running],2	; SHUT DOWN cache completely
				; (no need to modify any enable masks)
		push word fish2err
		call meep	; complain a bit...
	jmp DISPATCH_CHAIN	; [running] may be reset with a debugger,
				; so XMS and DOS RAM are not released
fish2err	db "CDRcache: Shutting down. Scary event code: ",0

%endif

; --------------

DISPATCH_DONE:				; we already did everything,
	stc				; so do not chain to CD-ROM driver.
	jmp short unstack_dispatch

DISPATCH_CHAIN:
	clc				; chain to CD-ROM driver

unstack_dispatch:			; close the local stack before

	pop dx				; no longer need THAT freedom ;-)
	pop eax

	pushf				; continuing with oldint!!!
	test word [cs:localss],0xffff	; are we using the local stack?
	jz short unstack_leave
	popf				; ***
	mov word [cs:localss],0		; mark local stack as free
	lss sp,[cs:othersp]		; continue with caller stack
	ret				; returned CARRY FLAG is important!

unstack_leave:
	popf				; ***
	ret				; returned CARRY FLAG is important!


; --------------

mcerrmsg	db " CDRcache IOCTL read media change failed ",0
staknesterr	db " CDRcache Oops! Stack nesting ",0

	; even when compiled in, trace and verbose messages are only
	; displayed when the current verbosity level is high enough.
	; It can be configured through the user interface :-).

%if TRACE
tracemsg	db " CDRcache command ",0
ioctlwrmsg	db " CDRcache IOCTL write ",0
ioctlrdmsg	db " CDRcache IOCTL read ",0
%endif

%if VERBOSE
changedmsg	db " CDRcache flush: IOCTL read media change ",0
resetmsg	db " CDRcache flush: IOCTL write reset ",0
ejectmsg	db " CDRcache flush: IOCTL write eject ",0
formatmsg	db " CDRcache: raw/RedBook not cached ",0

	; *** Note: If you have more than one subunit, the cache can
	; *** re-enable too early and disable too early. Does not
	; *** really harm, apart from degrading performance!

writemsg	db " CDRcache disabled while writing ",0
unwritemsg	db " CDRcache re-enabled after disk change ",0
%endif


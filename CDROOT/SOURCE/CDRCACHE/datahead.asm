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


; include the SYS header, SYS basics and global data

next	dw -1		; chain pointer to next device driver
	dw -1
attrib	dw 0c800h	; a CHAR device with IOCTL and OPEN support
stra	dw strat	; strategy  -> prepare, save ES:BX pointer
inr	dw intr		; interrupt -> main procedure

nam	db 'XXXCACH$'	; our name, WARNING: becomes invalid file
			; name, this is why the "$" is included
			; ("CDRCACH$.*" being invalid is less annoying)
reshdr		dw 0	; [12h] must be 0 in CD-ROM drivers
driveletter	db 0	; [14h] set by *CDEX, we must pass this on!!
			; 0 means none, 1 means a:, we initialize to 26.
unitcount	db 1	; [15h] tells how many drives are handled by us,
			; we copy this value from the real CD-ROM driver.

; ---------------------------------------------------------------

; All the stuff up to and including TSRSIZE should not change in
; structure, because external tools rely on this data being there!
; The magic values are the name at offset 0x0a and the xms handle
; right after that being HDCACHE$ and not-zero respectively.
; LBAcache INFO wants havelba and drvselmask to stay at their places, too.

xmshandle	dw 0	; our XMS handle (also for unloading)

pb		dd 0	; buffer for the ES:BX pointer...

xmsvec		dd 0	; call far pointer for XMS services

rdhit	dd 0	; those 2 counters offer some statistics
rdmiss	dd 0

oldstra	dd 0	; far call to CD ROM device "strategy"
oldintr dd 0	; far call to CD ROM device "interrupt"
oldhead	dd 0	; far pointer to CD ROM device, so that we can pass on
		; "driveletter" value updates.

hint	dw table	; pointer to table (hint for debuggers)
tabsz	dw 8	; hint for debuggers: size of table entries
		; high byte is how far sectors must be shifted right
		; to get number of table entries!

sectors	dw 1024	; how many sectors to cache (*2048* bytes
		; XMS each, plus 8 bytes DOS RAM for each
		; "element" (element size default: 8 sectors).
xmssize	dd 0	; used by status display function, must be dd.

running	dw 0		; nonzero means "init already done"
			; odd means "enabled" (dispatcher)
			; "so initialized but disabled" is 2

	db ">>"
clientname:
	db 0,0,0,0,0,0,0,0	; name of client device, 8 chars
	db 0			; name termination
	db "<<"

; ---------------------------------------------------------------

	; .8086 (nasm only has BITS 16 and BITS 32 directives)
	; So we must REMEMBER not to touch 386 code until we
	; have identified the host as "PC with a 80386 or better"

strat:	; in theory, we could even copy the data at es:bx here
	mov word [cs:pb+2],es
	mov word [cs:pb],bx
	RETF

intr:	; the main entry point (called after strat by DOS).
	pushf
        push bx
        push es

	cmp word [cs:oldhead+2],0	; already connected?
	jz nodriveletterupdate
	; We do this every time, as *CDEX
	; will change our drive letter
	; without notifying us about it!
	push ax
	les bx,[cs:oldhead]
	mov al,[cs:driveletter]		; take current CACHE drive letter
	mov [es:bx+0x14],al		; ... and update the REAL one!
	pop ax
nodriveletterupdate:
	
        les bx,[cs:pb]
        and word [es:bx+3],0x7d00	; *** not busy / no error
        or word [es:bx+3],0x100		; *** ack status
        cmp byte [es:bx+2],0	; is the command INIT ?
	jnz othercmd
	cmp byte [cs:running],0	; install already done?
	jnz nix			; only allow init once
        jmp install
nix:    pop es
        pop bx
        popf
        retf

othercmd:
	test word [cs:running],1
	jz passthru		; handler not enabled?
	call DISPATCH		; the dispatcher (386 code...)
	jc nix			; dispatcher already did everything
				; else chain to CD-ROM driver!

passthru:			; if not enabled, just chain.
        les bx,[cs:pb]		; reload (if DISPATCH trashed it)
	call far [cs:oldstra]	; let CD-ROM driver know es:bx
	pop es
	pop bx
	popf
	jmp far [cs:oldintr]	; chain to CD-ROM driver

	; all stuff up to this line has to stay in RAM in case we were
	; loaded as SYS or in case int 13 was changed in the meantime!

eoftsr:	; (following data can be destroyed if handler is disabled)

; ---------------------------------------------------------------


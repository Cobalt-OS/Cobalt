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


%ifdef DBGsp
%imacro STACKDEBUG 0	; 0 parameters (1 would be referred %1...)
	push ax
	mov ax,sp
	inc ax
	inc ax
		push word colonmsg
		call meep	; warn
	pop ax
%endmacro
%else
%define STACKDEBUG nop
%endif

; dat     segment

; WARNING: nasm treats mov ax,label and mov ax,[label] the same in
; some versions, use mov ax,[seg:label] to access memory AT label !?
; (only a problem for sources, not for destinations)

	org  0	; SYS/XMS version of a CD-ROM read cache

	; CDRCACHE written by Eric Auer 10/2003 <eric@coli.uni-sb.de>
        ; Based on HDDCACHE/LBACACHE by Eric Auer 2001-2003 (...).

; currently not planned: any writes to CD-ROM are a tricky thing,
; so do not expect writes to be cached / pooled by the cache. The
; current version even stops caching reads if it detects write
; activity. Read cache is re-activated by disk changes.

; See history.txt for details about what changed when in CDRcache.

; include the SYS header, SYS basics and global data
; this must be FIRST as it contains stuff that will stay in RAM
; if the driver is disabled but complete unloading does not work

%include "datahead.asm"

	; .386 (nasm only has BITS 16 and BITS 32 directives for)
	; (the CS type,  but cannot limit itself to x86 code...!)

; -------------------------------------------------------------------

	; include the main DISPATCH dispatcher call which calls
	; the actual read and write handlers. If it returns with
	; CY set, we can return to the caller, otherwise we chain
	; to the CD-ROM driver.

%include "dispatch.asm"

; -------------------------------------------------------------------

	; Main read and write handling functions. They expect a
	; standard device call parameter block at ES:BX as usual.

%include "read.asm"

; %include "write.asm"	; *** write handling not yet implemented.

; -------------------------------------------------------------------

	; XMS helper functions
	; copytoxms copies one sector from es:bx to XMS slot AX
	; copytodos copies one sector from XMS slot AX to es:bx

%include "xmscopy.asm"

; -------------------------------------------------------------------

	; more helper functions (using int 0x10):
	; meep: makes a beep or shows the message pointed to by
	;       a word on stack (if the word is not 0...),
	;       then shows AX and returns, taking the word from
	;       the stack again. Saves all regs including flags.
	; showal: shows AL as hex. Trashes lots of registers.

%include "meepdisp.asm"

; -------------------------------------------------------------------

	; Userint offers an user interface by supporting read
	; and write calls to our device. As CD-ROM drivers do
	; not support those calls, there is no conflict.
	; Functions are to be called from DISPATCH with valid
	; es:bx value. They are "readUI" and "writeUI".
	; IMPORTANT: Userint is part of the RESIDENT driver now.

%include "userint.asm"

; -------------------------------------------------------------------

	; status table handling functions

	; table format is - per entry - D base sector, B drive,
	; B LRU ("used" flag) W bit flags ("which sectors filled?")

	; Location and drive are passed in EAX and DL.
	; Input is sector number EAX, output (xms) bin AX

	; findbin:  finds a bin for a location (stc if not found),
        ; newbin:   allocates a new bin for a given location,    
        ;           flushing old bins if needed.
        ;           (main bin selection "intelligence" !)  
	; flush:    marks all cache buffers as empty, resets table
        ; flushone: empties all slots for drive DL only 
        ; telltabsize: (returns carry on error)
        ;          tells in AX how big a table for AX sectors will be

%include "binsel.asm"

; -------------------------------------------------------------------

	; end of install code, needs to be resident (because of flush)

resinst:
inittable:

	mov ax,[cs:localsp]
	or ax,ax
	jz noinitstak
	mov di,table 		; *offset*
	mov al,0x20
initstak:
	mov [cs:di],al		; fill local stack and table with " "
	inc di
	cmp di,[cs:localsp]
	jb initstak
noinitstak:

		call flush	; mark cache as empty, initialize table
				; -> cache size must be known!

quitpop:		; this is where a failure ends...
	pop ds		; restore all regs and return
	popa

	; .8086 (nasm cannot limit itself to a certain CPU, so)
	; (we must simply take care of only using 8086 code in)
	; (this section ourselves... nasm DOES have BITS 16 / )
	; (and BITS 32 directives to set the current CS type. )

quitinst:
	mov word [cs:running],1	; mark init as done
        jmp nix

; -------------------------------------------------------------------

xmserr	db 'CDRCACHE: XMS error.',7,0

hello	db 'CD-ROM XMS/386 read cache, '
	db   'E. Auer <eric@coli.uni-sb.de> 2001-2004',13,10
	db 'License: GPL 2. '

align 8,db ' '	; align with spaces :-)

	; ********************************************

table:		; THIS is where our table -starts-, or in other words,
		; THIS is where the resident code -ends- !!!
		; this + table size -> first free byte position.

	; ********************************************

	db      'For standard CD-ROM device drivers.'
	db 13,10,0


	; the rest after <table> will be overwritten by our (aligned)
	; status table...

	; 1/2002 format:  dword sector0, byte drive, byte lru, word
	; bitmask (bitmask: allows ..16 sectors per slot, default _4_)
	; Using default of _8_ sectors per slot since 11/2002.

; -------------------------------------------------------------------

	; main install and setup routine follows (starts with
	; 8086 compatible code to detect 386, then does a check
	; for the existence of XMS, allocates the cache there.
	; install: is the entry point, jumps to resinst: finally.

%include "setup.asm"

; -------------------------------------------------------------------

	; parse command line, display it...
	; "parsecommandline:" is called from setup
	; input: pointer esbx to "that device structure"

        ; recognized syntax: CD-ROM-driver-name our-name size
        ; anything else -> help message, CY.

%include "cmdline.asm"

; -------------------------------------------------------------------

	; .8086 (as said above, nasm does not have this)
	; (directive, so make sure you do not use >8086)
	; ( code  in  the  lines  below  ->  by  hand! )

			; obvious: do NOT use this after it is
			; overwritten by the table - use meep instead

			; *** changed to use int 0x21, because strtty
			; *** is only used during setup and should be
			; *** redirectable (11/2002)
strtty: push ax		; *** Print out a string during init, via DOS
	push dx		; *** was bx
strt2:	; *** mov bx,7	; print out some text string using bios
        mov ah,2	; *** was 0x0e
			; *** int 21.2 is not reliably redirected,
			; *** using int 21.6 (-> dl must not be 0xff)
        mov al,[cs:si]
	inc si		; so we need not care for std/cld as w/ lods!
        or al,al
        jz short sttend
	mov dl,al	; *** for DOS the char must be in DL
%ifdef REDIRBUG
	call BUGTTY
%else
	int 0x21	; *** was 0x10
%endif
        jmp short strt2	; YUCK, was to strtty - stack bomb!
sttend: pop dx		; *** was bx
	pop ax
	ret

%ifdef REDIRBUG
BUGTTY:	push ds		; Workaround a FreeDOS kernel bug 11/2002:
	push dx		; only strings, not chars are ">" and "|"
	mov ax,cs	; redirectable (FreeDOS kernel 2027)
	mov ds,ax
	mov [ds:bugttybuf],dl
	mov dx,bugttybuf
	mov ah,9
	int 0x21	; print (single char) string...
	pop dx
	pop ds
	ret
bugttybuf	db "*$"	; sigh...
%endif

; dat     ENDS
; END

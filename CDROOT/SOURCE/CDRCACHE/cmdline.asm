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

%define SYNDEBUG 1	; show error numbers for syntax errors

	; Some of the status messages use meep: This uses
	; int 21 while running < 1 and int 10 tty otherwise.

	; parse command line and display it, normally returning NC
	; input: pointer to "that device thing"
	; syntax: cdrom-driver-device-name our-device-name size
	;   size unit is 128 sectors. size must have 1-2 digits.
	;   initial ?:\... (up to space) is skipped, as device
	;   name can be part of the device command line.
	;   *** changed: initial anything is skipped, even if no
	;   *** drive letter is given (12.10.2003). Only for .sys!
	;   (fixes bug: "device=c:\file" worked, "device=file" not!)
	; syntax error causes help message display and returns CY.

parsecommandline:
	push es
	push bx
	push eax
	push si
	les bx,[es:bx+0x12]	; here comes our pointer :-)

	; warning: StrTTY trashes registers: BX AX SI...

%ifdef DBGclptr
		mov ax,es
		push word clbufend	; empty silence *offset*
		call meep		; show es
		mov ax,bx
		push word clbufend	; empty silence *offset*
		call meep		; show bx
%endif

; -------------

				; syntax error 1: no arguments
	jmp clignore_initial	; *** completely ignore first word!
				; (will be cdrcache.sys path itself)

clnextword:
	mov ax,[es:bx+1]	; is it X:\...?
	cmp ax,':\'		; ignore those
	jz near clignored2
	mov ax,[es:bx]
	inc bx			; parse on... (skip " " etc.)
	cmp al,9		; tab? then skip.
	jz clnextword
	cmp al,13		; eof? (CR, LF, or 0)
	jbe clhelp		; too few arguments!

	cmp al,' '	; space?
	jz clnextword	; skip over space

	cmp ax,'/?'	; "/?" help request? (must have the / now!)
	jz clhelp

	; *** if no /?, it is the 1st real argument

%if SYNDEBUG
	mov byte [cs:synerr],'A'
%endif
	jmp short clgetfirstname

; --------------

clhelp:	mov si,clhelpmsg
		call strtty	; show help message (trashes AX BX SI)
	pop si
	pop eax
	pop bx
	pop es
	stc			; errors found
	ret

; --------------

clignored2:
	add bx,3	; skip "X:\"
clignore_initial:			; ***
%if SYNDEBUG
	mov byte [cs:synerr],'0'	; 0 is "line ended after x:\"
%endif
clignored:
	mov al,[es:bx]
	cmp al,' '	; skip until whitespace hit
	jz clnextword	; continue with normal parsing
	cmp al,9	; tab?
	jz clnextword
	cmp al,13
	jbe clhelp	; if end of line, too few args!
	inc bx
	jmp clignored	; skip on

; --------------

clgetfirstname:
	mov si,clientname	; 1st arg is copied there
clgfnl:	mov [cs:si],al		;store 1 char
	mov al,[es:bx]		; get next char
	inc bx			; sic!
	inc si			; sic!
	cmp al,' '		; space or eof?
	jbe clgetsecondname
	cmp si,clientname+8
	jz clhelp		; user tried to tell us a 9th char
	jmp short clgfnl

; -------------

clgetsecondname:
%if SYNDEBUG
	mov byte [cs:synerr],'a'
%endif
	cmp al,9		; tab?
	jz clgsnSpace
	cmp al,' '		; space or eof?
	jb clhelp		; eof: user did not tell us 2nd arg
	jnz clgsn		; no space: found 2nd arg
clgsnSpace:
	mov al,[es:bx]
	inc bx			; skip whitespace
	jmp short clgetsecondname

; -------------

clgsn:	mov si,nam		; 2st arg is copied here
%if SYNDEBUG
	mov byte [cs:synerr],'B'
%endif
clgsnl:	mov [cs:si],al		; store 1 char
	mov al,[es:bx]		; get next char
	inc bx			; sic!
	inc si			; sic!
	cmp al,' '		; space or eof?
	jbe clgotsecondname
	cmp si,nam+8
	jz clhelp		; user tried to tell us a 9th char
	jmp short clgsnl

; -------------

clgotsecondname:
%if SYNDEBUG
	mov byte [cs:synerr],'C'
%endif
	mov al,' '		; pad with spaces
	cmp si,nam+8
	jz clwrotesecondname
	mov [cs:si],al		; pad this (RBIL says we have to)
	inc si			; sic!
	jmp short clgotsecondname

; -------------

clwrotesecondname:
	mov al,[es:bx]
	cmp al,9		; tab?
	jz clgdigitsSpace
	cmp al,' '		; space or eof?
	jb clhelp_jump		; eof: user did not tell us 3rd arg
	jnz cldigits		; no space: found 3rd arg
clgdigitsSpace:	
	inc bx			; skip whitespace
	jmp short clwrotesecondname

; --------------

cldigits:		; numeric argument: cache size in kbytes
			; (but 1..99 means 1..99 *128* sectors buffer!)
%if SYNDEBUG
	mov byte [cs:synerr],'#'
%endif
	push cx		; will hold the converted number
	xor cx,cx

cldigloop:
	mov al,[es:bx]
	inc bx
	cmp al,' '	; end of argument or buffer?
	jbe cldigdone
	sub al,'0'	; convert to binary
	cmp al,9	; valid digit?
	jbe cldigok

cldigbug:
%if SYNDEBUG
	mov [cs:synerr+1],al	; store offending digit
%endif
	pop cx		; restore CX!
	jmp clhelp	; show help / error message

cldigover:		; numeric overflow
	mov al,'+'	; error code for that
	jmp short cldigbug

cldigok:
	mov ah,0	; or use CBW ...
	cmp cx,65535/10	; can value get out of 16 bit range?
	jae cldigover
	push ax
	push dx
	mov ax,10	; base of number is 10
	xor dx,dx	; or use CWD ...
	mul cx		; multiply old value by 10
	mov cx,ax	; update intermediate result
	pop dx
	pop ax
	add cx,ax	; add new digit as lowest one
	jmp short cldigloop	; scan for more digits

cldigdone:
	mov ax,cx	; size (in kbytes)
	cmp ax,100	; for compatibility with old syntax: values
	jae cldignorm	; 1..99 mean 1..99 * 256 kbytes (N/4 MB)
	shl ax,8	; multiply with 256
cldignorm:
	inc ax		; round up (overflow check already done above)
	shr ax,1	; sector size is 2 kB, so N kB is N/2 sectors
	jz cldigiszero	; "zero" cache size means "default"
	test ax,127	; multiple of 128 sectors (1/4 MB)?
	jz cldigisround
	or ax,127	; else round UP to multiple of 128 sectors ...
	inc ax
	js cldigover	; oops, rounded up to 32768 sectors, overflow!
cldigisround:
	mov [cs:sectors],ax	; write the selected cache size
cldigiszero:

	pop cx		; restore CX!

	; ignore all following text (can be used for comments)

; --------------

	pop si
	pop eax
	pop bx
	pop es
	clc		; no errors found
	ret

clhelp_jump:		; keep jumps short
	jmp clhelp

clhelpmsg:
%if SYNDEBUG
	db "Syntax error type "
synerr	db "1 ",13,10
%endif
	db "Syntax: CDRCACHE cdromname cachename size [comments]",13,10
	db 13,10
	db "  cdromname Device name, used to find the CD-ROM driver",13,10
	db "  cachename Name that CDRCACHE should give itself. You can",13,10
	db "            then tell SHSUCDX/MSCDEX/... that name instead",13,10
	db "            of the name of the CD-ROM driver to use this cache.",13,10
	db "  size      The cache size in kilobytes of XMS, 128 to 65280.",13,10
	db "  comments  (Everything after the 3rd argument is ignored.)",13,10
	db 13,10
	db "After loading, the cache can be configured by sending text to the",13,10
	db "device: if you loaded it with cachename being CDRCACH$ for example,",13,10
	db "you can do 'echo ? > CDRCACH$' to get help and 'echo S > CDRCACH$'",13,10
	db "to make cache statistics appear on screen (not redirectable).",13,10
	db 0


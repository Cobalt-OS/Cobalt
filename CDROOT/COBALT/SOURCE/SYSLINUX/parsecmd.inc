;; $Id: parsecmd.inc,v 1.5 2002/10/24 17:31:45 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 1994-2002 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; parsecmd.inc
;;
;; Command line parser code
;;

; -------------------------------------------------------------------------
;  getcommand:	Get a keyword from the current "getc" file and match it
;		against a list of keywords (keywd_table).  Each entry in
;		that table should have the following form:
;		<32 bit hash value> <16 bit handler offset>
;
;               The handler is called, and upon return this function
;               returns with CF = 0.  On EOF, this function returns
;		with CF = 1.
; -------------------------------------------------------------------------

getcommand:
.find:
		call skipspace		; Skip leading whitespace
		jz .eof			; End of file
		jc .find		; End of line: try again
		cmp al,'0'		; Skip comment line
		jb .skipline

		or al,20h		; Convert to lower case
		movzx ebx,al		; Hash for a one-char keyword
.read_loop:
		push ebx
		call getc
		pop ebx
		cmp al,' '		; Whitespace
		jbe .done
		or al,20h
		rol ebx,5
		xor bl,al
		jmp short .read_loop
.done:		call ungetc
		call skipspace
		jz .eof
		jc .noparm
		call ungetc		; Return nonwhitespace char to buf
		mov si,keywd_table
		mov cx,keywd_count
.table_search:
		lodsd
		cmp ebx,eax
		je .found_keywd
		lodsd			; Skip entrypoint/argument
		loop .table_search

		; Otherwise unrecognized keyword
		mov si,err_badcfg
		jmp short .error

		; No parameter
.noparm:
		mov si,err_noparm
.error:
		call cwritestr
		jmp short .find

.found_keywd:	lodsw			; Load argument into ax
		call [si]
		clc
		ret

.eof:		stc
		ret

.skipline:	cmp al,10		; Search for LF
		je .find
		call getc
		jc .eof
		jmp short .skipline

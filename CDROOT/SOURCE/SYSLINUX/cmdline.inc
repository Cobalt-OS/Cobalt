;; $Id: cmdline.inc,v 1.1 2003/11/24 02:44:42 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 2003 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; cmdline.inc
;;
;; Common routine to assemble [null-terminated] command line into
;; real_mode_seg:cmd_line_here.
;; Not used by plain kernel due to BOOT_IMAGE= etc.
;;

;
; Assumes DS == CS
make_plain_cmdline:
		push es
		; ui.inc has already copied the actual command line
		mov ax,real_mode_seg
		mov es,ax

		mov si,[CmdOptPtr]
		mov di,[CmdLinePtr]
		
.loop:		lodsb
		stosb
		and al,al
		jnz .loop

		dec di
		mov [CmdLinePtr],di

		pop es
		ret



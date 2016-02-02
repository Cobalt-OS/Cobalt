;; $Id: keywords.inc,v 1.8 2004/05/29 22:11:23 hpa Exp $
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
;; keywords.inc
;;
;; Common header file for the handling of keyword hash and macros
;;

%ifndef DEPEND		; Generated file
%include "kwdhash.gen"
%endif

%macro keyword 2
	dd hash_%1	; Hash value
	dw 0		; No argument
	dw %2		; Entrypoint
%endmacro

%macro keyword 3
	dd hash_%1	; Hash value
	dw %3		; 16-bit argument
	dw %2		; Entrypoint
%endmacro

%macro keyword 4
	dd hash_%1	; Hash value
	db %3, %4	; 2 8-bit arguments
	dw %2		; Entrypoint
%endmacro

keywd_size	equ 8	; Bytes per keyword

		align 4, db 0

keywd_table:
		keyword hash,      pc_comment
		keyword append,    pc_append
		keyword default,   pc_default
		keyword display,   pc_filecmd,  get_msg_file
		keyword font,      pc_filecmd,  loadfont
		keyword implicit,  pc_setint16, AllowImplicit
		keyword kbdmap,    pc_filecmd,  loadkeys
		keyword kernel,    pc_kernel
		keyword label,     pc_label
		keyword prompt,    pc_setint16, ForcePrompt
		keyword say,       pc_say
		keyword serial,    pc_serial
		keyword timeout,   pc_timeout
		keyword ontimeout, pc_ontimeout
		keyword onerror,   pc_onerror
		keyword allowoptions, pc_setint16, AllowOptions
		keyword f1,        pc_fkey,     FKeyName+(0<<FILENAME_MAX_LG2)
		keyword f2,        pc_fkey,     FKeyName+(1<<FILENAME_MAX_LG2)
		keyword f3,        pc_fkey,     FKeyName+(2<<FILENAME_MAX_LG2)
		keyword f4,        pc_fkey,     FKeyName+(3<<FILENAME_MAX_LG2)
		keyword f5,        pc_fkey,     FKeyName+(4<<FILENAME_MAX_LG2)
		keyword f6,        pc_fkey,     FKeyName+(5<<FILENAME_MAX_LG2)
		keyword f7,        pc_fkey,     FKeyName+(6<<FILENAME_MAX_LG2)
		keyword f8,        pc_fkey,     FKeyName+(7<<FILENAME_MAX_LG2)
		keyword f9,        pc_fkey,     FKeyName+(8<<FILENAME_MAX_LG2)
		keyword f10,       pc_fkey,     FKeyName+(9<<FILENAME_MAX_LG2)
		keyword f0,        pc_fkey,     FKeyName+(9<<FILENAME_MAX_LG2)
%if IS_PXELINUX
		keyword ipappend,  pc_ipappend
%endif
%if IS_PXELINUX || IS_ISOLINUX
		keyword localboot, pc_localboot
%endif

keywd_count	equ ($-keywd_table)/keywd_size


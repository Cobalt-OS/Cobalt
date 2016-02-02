;; $Id: macros.inc,v 1.6 2003/04/16 02:49:31 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 1994-2003 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; macros.inc
;; 
;; Convenient macros
;;

%ifndef _MACROS_INC
%define _MACROS_INC

;
; Identify the module we're compiling; the "correct" should be defined
; in the module itself to 1
;
%ifndef IS_SYSLINUX
%define IS_SYSLINUX 0
%endif
%ifndef IS_MDSLINUX
%define IS_MDSLINUX 0
%endif
%ifndef IS_PXELINUX
%define IS_PXELINUX 0
%endif
%ifndef IS_ISOLINUX
%define IS_ISOLINUX 0
%endif

;
; Macros similar to res[bwd], but which works in the code segment (after
; section .text) or the data segment (section .data)
;
%macro	zb	1.nolist
	times %1 db 0
%endmacro

%macro	zw	1.nolist
	times %1 dw 0
%endmacro

%macro	zd	1.nolist
	times %1 dd 0
%endmacro

;
; Macro to emit an unsigned decimal number as a string
;
%macro asciidec	1.nolist
%ifndef DEPEND	; Not safe for "depend"
%if %1 >= 1000000000
	db ((%1/1000000000) % 10) + '0'
%endif
%if %1 >= 100000000
	db ((%1/100000000) % 10) + '0'
%endif
%if %1 >= 10000000
	db ((%1/10000000) % 10) + '0'
%endif
%if %1 >= 1000000
	db ((%1/1000000) % 10) + '0'
%endif
%if %1 >= 100000
	db ((%1/100000) % 10) + '0'
%endif
%if %1 >= 10000
	db ((%1/10000) % 10) + '0'
%endif
%if %1 >= 1000
	db ((%1/1000) % 10) + '0'
%endif
%if %1 >= 100
	db ((%1/100) % 10) + '0'
%endif
%if %1 >= 10
	db ((%1/10) % 10) + '0'
%endif
	db (%1 % 10) + '0'
%endif
%endmacro

;
; Macros for network byte order of constants
;
%define htons(x)  ( ( ((x) & 0FFh) << 8 ) + ( ((x) & 0FF00h) >> 8 ) )
%define ntohs(x) htons(x)
%define htonl(x)  ( ( ((x) & 0FFh) << 24) + ( ((x) & 0FF00h) << 8 ) + ( ((x) & 0FF0000h) >> 8 ) + ( ((x) & 0FF000000h) >> 24) )
%define ntohl(x) htonl(x)

%endif ; _MACROS_INC

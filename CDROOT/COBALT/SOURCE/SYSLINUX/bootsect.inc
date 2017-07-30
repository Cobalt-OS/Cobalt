;; $Id: bootsect.inc,v 1.11 2003/11/26 05:42:33 hpa Exp $
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
;; bootsect.inc
;; 
;; Load a boot sector (or other bootstrap program.)
;;
;; Unlike previous versions of this software, this doesn't require that
;; the length is 512 bytes.  This allows PXE bootstraps and WinNT
;; "CD boot sectors" to be invoked.
;;

;
; Load a boot sector
;
is_bootsector:
%if IS_SYSLINUX || IS_MDSLINUX
		; Transfer zero bytes
		mov byte [CopySuper],0
		jmp short load_bootsec

is_bss_sector:
		; Transfer the superblock
		mov byte [CopySuper],superblock_len
%endif
load_bootsec:
		xchg dx,ax
		shl eax,16
		xchg dx,ax		; Now EAX = file length
		mov edi, 100000h
		push edi		; Save load address
		call load_high
		call crlf

		sub edi,100000h
		push edi		; Save length

%if IS_SYSLINUX || IS_MDSLINUX
		movzx ecx,byte [CopySuper]
		jcxz .not_bss

		; For a BSS boot sector we have to patch.
		mov esi,superblock
		mov edi,100000h+(superblock-bootsec)
		call bcopy

.not_bss:
%endif

		xor edx,edx
		xor esi,esi
%if IS_SYSLINUX || IS_MDSLINUX
		; Restore original FDC table
		mov eax,[OrigFDCTabPtr]
		mov [fdctab],eax

		mov dl,[bsDriveNumber]
		mov si,PartInfo		; Partition info buffer
		mov di,800h-18		; Put partition info here
		push di
		mov cx,8		; 16 bytes
		xor ax,ax
		rep movsw
		pop si			; DS:SI points to partition info
%elif IS_ISOLINUX
		mov dl,[DriveNo]
%elif IS_PXELINUX
		mov byte [KeepPXE],1	; Chainloading another NBP
		call reset_pxe
%endif
		xor bx,bx

; 
; Entrypoint for "shut down and replace bootstrap" -- also invoked by
; the COMBOOT API.  This routine expects two dword on the stack:
; linear address and length.  Additionally, the values of ESI and EDX
; are passed on to the new bootstrap; the value of BX becomes the new
; DS.
; 
replace_bootstrap:

		;
		; Prepare for shutting down
		;
		call vgaclearmode

		;
		; Set up initial stack frame (not used by PXE if keeppxe is set)
		;
		xor ax,ax
		mov ds,ax
		mov es,ax		

%if IS_PXELINUX
		test byte [KeepPXE],01h
		jz .stdstack
		les di,[InitStack]	; Reset stack to PXE original
		jmp .stackok
%endif
.stdstack:
		mov di,7C00h-44
		push di
		mov cx,22		; 44 bytes
		rep stosw
		pop di
.stackok:

		mov [es:di+28],edx
		mov [es:di+12],esi
		mov [es:di+6],bx

		pop ecx			; Byte count to copy
		pop esi			; Copy from...

		cli
		mov ax,es
		mov ss,ax
		movzx esp,di
		mov edi,7C00h		; Copy to...

		jmp bcopy_over_self

%if IS_SYSLINUX || IS_MDSLINUX
		; Nothing
%else
is_bss_sector:
		mov si,err_bssimage
		call cwritestr
		jmp enter_command
%endif

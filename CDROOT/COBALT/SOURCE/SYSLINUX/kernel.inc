;; $Id: kernel.inc,v 1.1 2002/04/26 05:56:24 hpa Exp $
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
;; kernel.inc
;; 
;; Header file for the kernel interface definitions
;;

%ifndef _KERNEL_INC
%define _KERNEL_INC

;;
;; Structure of the real_mode_seg
;;

		struc real_mode_seg_t
		resb 20h-($-$$)		; org 20h
kern_cmd_magic	resw 1			; 0020 Magic # for command line
kern_cmd_offset resw 1			; 0022 Offset for kernel command line
		resb 497-($-$$)		; org 497d
bs_setupsecs	resb 1			; 01F1 Sectors for setup code (0 -> 4)
bs_rootflags	resw 1			; 01F2 Root readonly flag
bs_syssize	resw 1			; 01F4
bs_swapdev	resw 1			; 01F6 Swap device (obsolete)
bs_ramsize	resw 1			; 01F8 Ramdisk flags, formerly ramdisk size
bs_vidmode	resw 1			; 01FA Video mode
bs_rootdev	resw 1			; 01FC Root device
bs_bootsign	resw 1			; 01FE Boot sector signature (0AA55h)
su_jump		resb 1			; 0200 0EBh
su_jump2	resb 1			; 0201 Size of following header
su_header	resd 1			; 0202 New setup code: header
su_version	resw 1			; 0206 See linux/arch/i386/boot/setup.S
su_switch	resw 1			; 0208
su_setupseg	resw 1			; 020A
su_startsys	resw 1			; 020C
su_kver		resw 1			; 020E Kernel version pointer
su_loader	resb 1			; 0210 Loader ID
su_loadflags	resb 1			; 0211 Load high flag
su_movesize	resw 1			; 0212
su_code32start	resd 1			; 0214 Start of code loaded high
su_ramdiskat	resd 1			; 0218 Start of initial ramdisk
su_ramdisklen	equ $			; Length of initial ramdisk
su_ramdisklen1	resw 1			; 021C
su_ramdisklen2	resw 1			; 021E
su_bsklugeoffs	resw 1			; 0220
su_bsklugeseg	resw 1			; 0222
su_heapend	resw 1			; 0224
su_pad1		resw 1			; 0226
su_cmd_line_ptr	resd 1			; 0228
su_ramdisk_max	resd 1			; 022C
		resb (9000h-12)-($-$$)	; Were bootsect.S puts it...
linux_stack	equ $			; 8FF4
linux_fdctab	equ $
		resb 9000h-($-$$)
cmd_line_here	equ $			; 9000 Should be out of the way
		endstruc

;
; Kernel command line signature
;
CMD_MAGIC	equ 0A33Fh		; Command line magic

;
; Magic number of su_header field
;
HEADER_ID       equ 'HdrS'		; HdrS (in littleendian hex)

;
; Flags for the su_loadflags field
;
LOAD_HIGH	equ 01h			; Large kernel, load high
CAN_USE_HEAP    equ 80h                 ; Boot loader reports heap size

;
; ID codes for various modules
;
syslinux_id	equ 031h		; 3 = SYSLINUX family; 1 = SYSLINUX
pxelinux_id	equ 032h		; 3 = SYSLINUX family; 2 = PXELINUX
isolinux_id	equ 033h		; 3 = SYSLINUX family; 3 = PXELINUX

%endif ; _KERNEL_INC

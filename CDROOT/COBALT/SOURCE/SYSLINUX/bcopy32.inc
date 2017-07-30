;; $Id: bcopy32.inc,v 1.7 2003/11/18 23:54:43 hpa Exp $
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
;; bcopy32.inc
;; 
;; 32-bit bcopy routine for real mode
;;

;
; 32-bit bcopy routine for real mode
;
; We enter protected mode, set up a flat 32-bit environment, run rep movsd
; and then exit.  IMPORTANT: This code assumes cs == 0.
;
; This code is probably excessively anal-retentive in its handling of
; segments, but this stuff is painful enough as it is without having to rely
; on everything happening "as it ought to."
;
; IMPORTANT: This code must be capable of operating when copied to the
; trackbuf area (1000h).  The routine bcopy_over_self handles this mode
; of operation, including any necessary adjustments.
;
		align 4
__bcopy_start:

bcopy_gdt:	dw bcopy_gdt_size-1	; Null descriptor - contains GDT
.adj1:		dd bcopy_gdt		; pointer for LGDT instruction
		dw 0
		dd 0000ffffh		; Code segment, use16, readable,
		dd 00009b00h		; present, dpl 0, cover 64K
		dd 0000ffffh		; Data segment, use16, read/write,
		dd 008f9300h		; present, dpl 0, cover all 4G
		dd 0000ffffh		; Data segment, use16, read/write,
		dd 00009300h		; present, dpl 0, cover 64K
		; The rest are used for COM32 only
		dd 0000ffffh		; Code segment, use32, readable,
		dd 00cf9b00h		; present, dpl 0, cover all 4G
		dd 0000ffffh		; Data segment, use32, read/write,
		dd 00cf9300h		; present, dpl 0, cover all 4G
bcopy_gdt_size:	equ $-bcopy_gdt

;
; bcopy:
;	32-bit copy
;
; Inputs:
;	ESI	- source pointer
;	EDI	- target pointer
;	ECX	- byte count
;	DF	- zero
;
; Outputs:
;	ESI	- first byte after source
;	EDI	- first byte after target
;	ECX	- zero
;
bcopy:		push eax
		pushf			; Saves, among others, the IF flag
		push gs
		push fs
		push ds
		push es
		mov [cs:SavedSSSP],sp
		mov [cs:SavedSSSP+2],ss

		cli
		call enable_a20

.adj2:		o32 lgdt [cs:bcopy_gdt]
		mov eax,cr0
		or al,1
		mov cr0,eax		; Enter protected mode
.adj3a:		jmp 08h:.in_pm

.in_pm:		mov ax,10h		; Data segment selector
		mov es,ax
		mov ds,ax

		mov al,18h		; "Real-mode-like" data segment
		mov ss,ax
		mov fs,ax
		mov gs,ax	

		mov al,cl		; Save low bits
		shr ecx,2		; Convert to dwords
		a32 rep movsd		; Do our business
		; At this point ecx == 0

		mov cl,al		; Copy any fractional dword
		and cl,3
		a32 rep movsb

		mov al,18h		; "Real-mode-like" data segment
		mov es,ax
		mov ds,ax

		mov eax,cr0
		and al,~1
		mov cr0,eax		; Disable protected mode
.adj3b:		jmp 0:.in_rm

.in_rm:		; Back in real mode
		lss sp,[cs:SavedSSSP]
		pop es
		pop ds
		pop fs
		pop gs
		call disable_a20

		popf			; Re-enables interrupts
		pop eax
		ret

;
; Routines to enable and disable (yuck) A20.  These routines are gathered
; from tips from a couple of sources, including the Linux kernel and
; http://www.x86.org/.  The need for the delay to be as large as given here
; is indicated by Donnie Barnes of RedHat, the problematic system being an
; IBM ThinkPad 760EL.
;
; We typically toggle A20 twice for every 64K transferred.
; 
%define	io_delay	call _io_delay
%define IO_DELAY_PORT	80h		; Invalid port (we hope!)
%define disable_wait 	32		; How long to wait for a disable

%define A20_DUNNO	0		; A20 type unknown
%define A20_NONE	1		; A20 always on?
%define A20_BIOS	2		; A20 BIOS enable
%define A20_KBC		3		; A20 through KBC
%define A20_FAST	4		; A20 through port 92h

		align 2
A20List		dw a20_dunno, a20_none, a20_bios, a20_kbc, a20_fast
A20DList	dw a20d_dunno, a20d_none, a20d_bios, a20d_kbc, a20d_fast
a20_adjust_cnt	equ ($-A20List)/2

slow_out:	out dx, al		; Fall through

_io_delay:	out IO_DELAY_PORT,al
		out IO_DELAY_PORT,al
		ret

enable_a20:
		pushad
		mov byte [cs:A20Tries],255 ; Times to try to make this work

try_enable_a20:
;
; Flush the caches
;
%if DO_WBINVD
		call try_wbinvd
%endif

;
; If the A20 type is known, jump straight to type
;
		mov bp,[cs:A20Type]
		add bp,bp			; Convert to word offset
.adj4:		jmp word [cs:bp+A20List]

;
; First, see if we are on a system with no A20 gate
;
a20_dunno:
a20_none:
		mov byte [cs:A20Type], A20_NONE
		call a20_test
		jnz a20_done

;
; Next, try the BIOS (INT 15h AX=2401h)
;
a20_bios:
		mov byte [cs:A20Type], A20_BIOS
		mov ax,2401h
		pushf				; Some BIOSes muck with IF
		int 15h
		popf

		call a20_test
		jnz a20_done

;
; Enable the keyboard controller A20 gate
;
a20_kbc:
		mov dl, 1			; Allow early exit
		call empty_8042
		jnz a20_done			; A20 live, no need to use KBC

		mov byte [cs:A20Type], A20_KBC	; Starting KBC command sequence

		mov al,0D1h			; Command write
		out 064h, al
		call empty_8042_uncond

		mov al,0DFh			; A20 on
		out 060h, al
		call empty_8042_uncond

		; Verify that A20 actually is enabled.  Do that by
		; observing a word in low memory and the same word in
		; the HMA until they are no longer coherent.  Note that
		; we don't do the same check in the disable case, because
		; we don't want to *require* A20 masking (SYSLINUX should
		; work fine without it, if the BIOS does.)
.kbc_wait:	push cx
		xor cx,cx
.kbc_wait_loop:
		call a20_test
		jnz a20_done_pop
		loop .kbc_wait_loop

		pop cx
;
; Running out of options here.  Final attempt: enable the "fast A20 gate"
;
a20_fast:
		mov byte [cs:A20Type], A20_FAST	; Haven't used the KBC yet
		in al, 092h
		or al,02h
		and al,~01h			; Don't accidentally reset the machine!
		out 092h, al

.fast_wait:	push cx
		xor cx,cx
.fast_wait_loop:
		call a20_test
		jnz a20_done_pop
		loop .fast_wait_loop

		pop cx

;
; Oh bugger.  A20 is not responding.  Try frobbing it again; eventually give up
; and report failure to the user.
;


		dec byte [cs:A20Tries]
		jnz try_enable_a20

		mov si, err_a20
		jmp abort_load
;
; A20 unmasked, proceed...
;
a20_done_pop:	pop cx
a20_done:	popad
		ret

;
; This routine tests if A20 is enabled (ZF = 0).  This routine
; must not destroy any register contents.
;
a20_test:
		push es
		push cx
		push ax
		mov cx,0FFFFh		; HMA = segment 0FFFFh
		mov es,cx
		mov cx,32		; Loop count
		mov ax,[cs:A20Test]
.a20_wait:	inc ax
		mov [cs:A20Test],ax
		io_delay		; Serialize, and fix delay
		cmp ax,[es:A20Test+10h]
		loopz .a20_wait
.a20_done:	pop ax
		pop cx
		pop es
		ret

disable_a20:
		pushad
;
; Flush the caches
;
%if DO_WBINVD
		call try_wbinvd
%endif

		mov bp,[cs:A20Type]
		add bp,bp			; Convert to word offset
.adj5:		jmp word [cs:bp+A20DList]

a20d_bios:
		mov ax,2400h
		pushf				; Some BIOSes muck with IF
		int 15h
		popf
		jmp short a20d_snooze

;
; Disable the "fast A20 gate"
;
a20d_fast:
		in al, 092h
		and al,~03h
		out 092h, al
		jmp short a20d_snooze

;
; Disable the keyboard controller A20 gate
;
a20d_kbc:
		call empty_8042_uncond
		mov al,0D1h
		out 064h, al		; Command write
		call empty_8042_uncond
		mov al,0DDh		; A20 off
		out 060h, al
		call empty_8042_uncond
		; Wait a bit for it to take effect
a20d_snooze:
		push cx
		mov cx, disable_wait
.delayloop:	call a20_test
		jz .disabled
		loop .delayloop
.disabled:	pop cx
a20d_dunno:
a20d_none:
		popad
		ret

;
; Routine to empty the 8042 KBC controller.  If dl != 0
; then we will test A20 in the loop and exit if A20 is
; suddenly enabled.
;
empty_8042_uncond:
		xor dl,dl
empty_8042:
		call a20_test
		jz .a20_on
		and dl,dl
		jnz .done
.a20_on:	io_delay
		in al, 064h		; Status port
		test al,1
		jz .no_output
		io_delay
		in al, 060h		; Read input
		jmp short empty_8042
.no_output:
		test al,2
		jnz empty_8042
		io_delay
.done:		ret	

;
; Execute a WBINVD instruction if possible on this CPU
;
%if DO_WBINVD
try_wbinvd:
		wbinvd
		ret
%endif

;
; bcopy_over_self:
;
; This routine is used to copy large blocks of code on top of
; conventional memory (to 0:7c00).  We therefore have to move
; necessary code into the trackbuf area before doing the copy,
; and do adjustments to anything except BSS area references.
;
; After performing the copy, this routine resets the stack and
; jumps to 0:7c00.
;
; IMPORTANT: This routine does not canonicalize the stack or the
; SS register.  That is the responsibility of the caller.
;
; Inputs:
;	ESI, EDI, ECX	- same as bcopy
;	On stack	- initial state (fd, ad, ds, es, fs, gs)
;
%define ADJUST	(__bcopy_start - trackbuf)

		align 2
adjlist		dw bcopy_gdt.adj1 - ADJUST
		dw bcopy.adj2 + 5 - ADJUST
		dw bcopy.adj3a + 1 - ADJUST
		dw bcopy.adj3b + 1 - ADJUST
		dw try_enable_a20.adj4 + 3 - ADJUST
		dw disable_a20.adj5 + 3 - ADJUST
adjlist_cnt	equ ($-adjlist)/2

bcopy_over_self:
		push esi	
		push edi
		push ecx

		xor bx,bx
		mov es,bx
		mov ds,bx

		mov si,__bcopy_start
		mov di,trackbuf
		mov cx,(__bcopy_end - __bcopy_start + 3) >> 2
		rep movsd

		mov si,A20List - ADJUST
		mov cx,a20_adjust_cnt
.adjust1:
		sub word [si], ADJUST
		inc si
		inc si
		loop .adjust1

		mov si, adjlist
		mov cx, adjlist_cnt
.adjust2:
		lodsw
		xchg di,ax
		sub word [di], ADJUST
		loop .adjust2

		jmp .next-ADJUST
.next:
		pop ecx
		pop edi
		pop esi
		call bcopy
		
		pop gs
		pop fs
		pop es
		pop ds
		popad
		popfd
		jmp 0:7c00h
__bcopy_end:

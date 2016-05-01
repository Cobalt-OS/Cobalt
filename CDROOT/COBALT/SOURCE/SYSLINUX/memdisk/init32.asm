;; -*- fundamental -*-
;; $Id: init32.asm,v 1.2 2003/07/11 00:02:30 hpa Exp $
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
;; init32.asm
;; 
;; Routine to trampoline into 32-bit protected memory.  This code is
;; derived from bcopy32.inc and com32.inc in the main SYSLINUX distribution.
;;

%define MY_CS 0x0800
%define CS_BASE (MY_CS << 4)

; Low memory bounce buffer
%define BOUNCE_SEG	(MY_CS+0x1000)

%define DO_WBINVD 0

%define STACK_HEAP_SIZE	(128*1024)

		section .rodata align=16
		section .data   align=16
		section .bss    align=16

		global init32
		
;
; We enter protected mode, set up a flat 32-bit environment, run rep movsd
; and then exit.  IMPORTANT: This code assumes cs == MY_CS.
;
; This code is probably excessively anal-retentive in its handling of
; segments, but this stuff is painful enough as it is without having to rely
; on everything happening "as it ought to."
;
		section .rodata

	; desc base, limit, flags
%macro	desc 3
	dd (%2 & 0xffff) | ((%1 & 0xffff) << 16)
	dd (%1 & 0xff000000) | (%2 & 0xf0000) | ((%3 & 0xf0ff) << 8) | ((%1 & 0x00ff0000) >> 16)
%endmacro
	
		align 4
call32_gdt:	dw call32_gdt_size-1	; Null descriptor - contains GDT
.adj1:		dd call32_gdt+CS_BASE	; pointer for LGDT instruction
		dw 0
		
		; 0008: Code segment, use16, readable, dpl 0, base CS_BASE, 64K
		desc CS_BASE, 0xffff, 0x009b

		; 0010: Data segment, use16, read/write, dpl 0, base CS_BASE, 64K
		desc CS_BASE, 0xffff, 0x0093

		; 0018: Data segment, use16, read/write, dpl 0, base 0, 4G
		desc 0, 0xfffff, 0x809b

		; 0020: Code segment, use32, read/write, dpl 0, base 0, 4G
		desc 0, 0xfffff, 0xc09b

		; 0028: Data segment, use32, read/write, dpl 0, base 0, 4G
		desc 0, 0xfffff, 0xc093
	
call32_gdt_size:	equ $-call32_gdt

err_a20:	db 'ERROR: A20 gate not responding!',13,10,0
	
		bits 16
		section .bss
		alignb 4
SavedSSSP	resd 1			; Place to save SS:SP
Return		resd 1			; Return value
A20Test		resw 1			; Space to test A20
A20Tries	resb 1
		
		section .data
		alignb 4
Target		dd 0			; Target address
Target_Seg	dw 20h			; Target CS

A20Type		dw 0			; Default = unknown
		
		section .text
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
		mov byte [A20Tries],255 ; Times to try to make this work

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
		mov bp,[A20Type]
		add bp,bp			; Convert to word offset
.adj4:		jmp word [bp+A20List]

;
; First, see if we are on a system with no A20 gate
;
a20_dunno:
a20_none:
		mov byte [A20Type], A20_NONE
		call a20_test
		jnz a20_done

;
; Next, try the BIOS (INT 15h AX=2401h)
;
a20_bios:
		mov byte [A20Type], A20_BIOS
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

		mov byte [A20Type], A20_KBC	; Starting KBC command sequence

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
		mov byte [A20Type], A20_FAST	; Haven't used the KBC yet
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

		dec byte [A20Tries]
		jnz try_enable_a20


		; Error message time
		mov si,err_a20
print_err:
		lodsb
		and al,al
		jz die
		mov bx,7
		mov ah,0xe
		int 10h
		jmp print_err


die:
		sti
.hlt:		hlt
		jmp short .hlt

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
		mov ax,[A20Test]
.a20_wait:	inc ax
		mov [A20Test],ax
		io_delay		; Serialize, and fix delay
		cmp ax,[es:A20Test+CS_BASE+10h]
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

		mov bp,[A20Type]
		add bp,bp			; Convert to word offset
.adj5:		jmp word [bp+A20DList]

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

		section .bss
		alignb 4
PMESP		resd 1			; Protected mode %esp

		section .idt nobits alloc exec write align=4096
		alignb 4096
pm_idt		resb 4096		; Protected-mode IDT, followed by interrupt stubs




pm_entry:	equ 0x100000

		section .rodata
		align 4, db 0
call32_pmidt:
		dw 8*256		; Limit
		dd pm_idt+CS_BASE	; Address

call32_rmidt:
		dw 0ffffh		; Limit
		dd 0			; Address

		section .text
;
; This is the main entrypoint in this function
;
init32:
		mov ebx,call32_call_start+CS_BASE	; Where to go in PM

call32_enter_pm:
		mov ax,cs
		mov ds,ax
		cli
		mov [SavedSSSP],sp
		mov [SavedSSSP+2],ss
		cld
		call a20_test
		jnz .a20ok
		call enable_a20

.a20ok:
		lgdt [call32_gdt]	; Set up GDT
		lidt [call32_pmidt]	; Set up the IDT
		mov eax,cr0
		or al,1
		mov cr0,eax		; Enter protected mode
		jmp 20h:dword .in_pm+CS_BASE
		
		bits 32
.in_pm:
		xor eax,eax		; Available for future use...
		mov fs,eax
		mov gs,eax

		mov al,28h		; Set up data segments
		mov es,eax
		mov ds,eax
		mov ss,eax

		mov esp,[PMESP+CS_BASE]	; Load protmode %esp if available
		jmp ebx			; Go to where we need to go

;
; This is invoked before first dispatch of the 32-bit code, in 32-bit mode
;
call32_call_start:
		;
		; Point the stack into low memory
		; We have: this segment, bounce buffer, then stack+heap
		;
		mov esp, CS_BASE + 0x20000 + STACK_HEAP_SIZE
		and esp, ~0xf

		;
		; Set up the protmode IDT and the interrupt jump buffers
		;
		mov edi,pm_idt+CS_BASE

		; Form an interrupt gate descriptor
		; WARNING: This is broken if pm_idt crosses a 64K boundary;
		; however, it can't because of the alignment constraints.
		mov ebx,pm_idt+CS_BASE+8*256
		mov eax,0x0020ee00
		xchg ax,bx
		xor ecx,ecx
		inc ch				; ecx <- 256

		push ecx
.make_idt:
		stosd
		add eax,8
		xchg eax,ebx
		stosd
		xchg eax,ebx
		loop .make_idt

		pop ecx

		; Each entry in the interrupt jump buffer contains
		; the following instructions:
		;
		; 00000000 60                pushad
		; 00000001 B0xx              mov al,<interrupt#>
		; 00000003 E9xxxxxxxx        jmp call32_handle_interrupt

		mov eax,0xe900b060
		mov ebx,call32_handle_interrupt+CS_BASE
		sub ebx,edi

.make_ijb:
		stosd
		sub [edi-2],cl			; Interrupt #
		xchg eax,ebx
		sub eax,8
		stosd
		xchg eax,ebx
		loop .make_ijb

		; Now everything is set up for interrupts...

		push dword (BOUNCE_SEG << 4)	; Bounce buffer address
		push dword call32_syscall+CS_BASE ; Syscall entry point
		sti				; Interrupts OK now
		call pm_entry-CS_BASE		; Run the program...

		; ... on return ...
		mov [Return+CS_BASE],eax

		; ... fall through to call32_exit ...

call32_exit:
		mov bx,call32_done	; Return to command loop

call32_enter_rm:
		cli
		cld
		mov [PMESP+CS_BASE],esp	; Save exit %esp
		xor esp,esp		; Make sure the high bits are zero
		jmp 08h:.in_pm16	; Return to 16-bit mode first

		bits 16
.in_pm16:
		mov ax,10h		; Real-mode-like segment
		mov es,ax
		mov ds,ax
		mov ss,ax
		mov fs,ax
		mov gs,ax

		lidt [call32_rmidt]	; Real-mode IDT (rm needs no GDT)
		mov eax,cr0
		and al,~1
		mov cr0,eax
		jmp MY_CS:.in_rm

.in_rm:					; Back in real mode
		mov ax,cs		; Set up sane segments
		mov ds,ax
		mov es,ax
		mov fs,ax
		mov gs,ax
		lss sp,[SavedSSSP]	; Restore stack
		jmp bx			; Go to whereever we need to go...

call32_done:
		call disable_a20
		sti
		mov ax,[Return]
		ret

;
; 16-bit support code
;
		bits 16

;
; 16-bit interrupt-handling code
;
call32_int_rm:
		pushf				; Flags on stack
		push cs				; Return segment
		push word .cont			; Return address
		push dword edx			; Segment:offset of IVT entry
		retf				; Invoke IVT routine
.cont:		; ... on resume ...
		mov ebx,call32_int_resume+CS_BASE
		jmp call32_enter_pm		; Go back to PM

;
; 16-bit system call handling code
;
call32_sys_rm:
		pop gs
		pop fs
		pop es
		pop ds
		popad
		popfd
		retf				; Invoke routine
.return:
		pushfd
		pushad
		push ds
		push es
		push fs
		push gs
		mov ebx,call32_sys_resume+CS_BASE
		jmp call32_enter_pm

;
; 32-bit support code
;
		bits 32

;
; This is invoked on getting an interrupt in protected mode.  At
; this point, we need to context-switch to real mode and invoke
; the interrupt routine.
;
; When this gets invoked, the registers are saved on the stack and
; AL contains the register number.
;
call32_handle_interrupt:
		movzx eax,al
		xor ebx,ebx		; Actually makes the code smaller
		mov edx,[ebx+eax*4]	; Get the segment:offset of the routine
		mov bx,call32_int_rm
		jmp call32_enter_rm	; Go to real mode

call32_int_resume:
		popad
		iret

;
; Syscall invocation.  We manifest a structure on the real-mode stack,
; containing the call32sys_t structure from <call32.h> as well as
; the following entries (from low to high address):
; - Target offset
; - Target segment
; - Return offset
; - Return segment (== real mode cs)
; - Return flags
;
call32_syscall:
		pushfd			; Save IF among other things...
		pushad			; We only need to save some, but...
		cld

		movzx edi,word [SavedSSSP+CS_BASE]
		movzx eax,word [SavedSSSP+CS_BASE+2]
		sub edi,54		; Allocate 54 bytes
		mov [SavedSSSP+CS_BASE],di
		shl eax,4
		add edi,eax		; Create linear address

		mov esi,[esp+11*4]	; Source regs
		xor ecx,ecx
		mov cl,11		; 44 bytes to copy
		rep movsd

		movzx eax,byte [esp+10*4] ; Interrupt number
		; ecx == 0 here; adding it to the EA makes the
		; encoding smaller
		mov eax,[ecx+eax*4]	; Get IVT entry
		stosd			; Save in stack frame
		mov eax,call32_sys_rm.return + (MY_CS << 16) ; Return seg:offs
		stosd			; Save in stack frame
		mov eax,[edi-12]	; Return flags
		and eax,0x200cd7	; Mask (potentially) unsafe flags
		mov [edi-12],eax	; Primary flags entry
		stosw			; Return flags

		mov bx,call32_sys_rm
		jmp call32_enter_rm	; Go to real mode

		; On return, the 44-byte return structure is on the
		; real-mode stack.
call32_sys_resume:
		movzx esi,word [SavedSSSP+CS_BASE]
		movzx eax,word [SavedSSSP+CS_BASE+2]
		mov edi,[esp+12*4]	; Dest regs
		shl eax,4
		add esi,eax		; Create linear address
		and edi,edi		; NULL pointer?
		jnz .do_copy
.no_copy:	mov edi,esi		; Do a dummy copy-to-self
.do_copy:	xor ecx,ecx
		mov cl,11		; 44 bytes
		rep movsd		; Copy register block

		add dword [SavedSSSP+CS_BASE],44	; Remove from stack

		popad
		popfd
		ret			; Return to 32-bit program

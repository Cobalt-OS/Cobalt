;; $Id: comboot.inc,v 1.24 2004/02/03 06:35:00 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 1994-2004 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; comboot.inc
;; 
;; Common code for running a COMBOOT image
;;

; Parameter registers definition; this is the definition
; of the stack frame used by INT 21h and INT 22h.
%define		P_FLAGS		word [bp+44]
%define		P_FLAGSL	byte [bp+44]
%define		P_FLAGSH	byte [bp+45]
%define		P_CS		word [bp+42]
%define		P_IP		word [bp+40]
%define		P_DS		word [bp+38]
%define		P_ES		word [bp+36]
%define		P_FS		word [bp+34]
%define		P_GS		word [bp+32]
%define		P_EAX		dword [bp+28]
%define		P_AX		word [bp+28]
%define		P_HAX		word [bp+30]
%define		P_AL		byte [bp+28]
%define		P_AH		byte [bp+29]
%define		P_ECX		dword [bp+24]
%define		P_CX		word [bp+24]
%define		P_HCX		word [bp+26]
%define		P_CL		byte [bp+24]
%define		P_CH		byte [bp+25]
%define		P_EDX		dword [bp+20]
%define		P_DX		word [bp+20]
%define		P_HDX		word [bp+22]
%define		P_DL		byte [bp+20]
%define		P_DH		byte [bp+21]
%define		P_EBX		dword [bp+16]
%define		P_BX		word [bp+16]
%define		P_HBX		word [bp+18]
%define		P_BL		byte [bp+16]
%define		P_BH		byte [bp+17]
%define		P_EBP		dword [bp+8]
%define		P_BP		word [bp+8]
%define		P_HBP		word [bp+10]
%define		P_ESI		dword [bp+4]
%define		P_SI		word [bp+4]
%define		P_HSI		word [bp+6]
%define		P_EDI		dword [bp]
%define		P_DI		word [bp]
%define		P_HDI		word [bp+2]

; Looks like a COMBOOT image but too large
comboot_too_large:
		mov si,err_comlarge
		call cwritestr
		jmp enter_command

;
; Load a COMBOOT image.  A COMBOOT image is basically a DOS .COM file,
; except that it may, of course, not contain any DOS system calls.  We
; do, however, allow the execution of INT 20h to return to SYSLINUX.
;
is_comboot_image:
		and dx,dx
		jnz comboot_too_large
		cmp ax,0ff00h		; Max size in bytes
		jae comboot_too_large

		push si			; Save file handle

		call make_plain_cmdline

		call comboot_setup_api

		mov cx,comboot_seg
		mov es,cx

		xor di,di
		mov cx,64		; 256 bytes (size of PSP)
		xor eax,eax		; Clear PSP
		rep stosd

		mov word [es:0], 020CDh	; INT 20h instruction
		; First non-free paragraph
		; This is valid because comboot_seg == real_mode_seg
		; == the highest segment used by all derivatives
		int 12h			; Get DOS memory size
		shl ax,6		; Kilobytes -> paragraphs
		mov word [es:02h],ax

%ifndef DEPEND
%if real_mode_seg != comboot_seg
%error "This code assumes real_mode_seg == comboot_seg"
%endif
%endif
		; Copy the command line from high memory
		mov si,cmd_line_here
		mov cx,125		; Max cmdline len (minus space and CR)
		mov di,081h		; Offset in PSP for command line
		mov al,' '		; DOS command lines begin with a space
		stosb

.loop:		es lodsb
		and al,al
		jz .done
		stosb
		loop .loop
.done:

		mov al,0Dh		; CR after last character
		stosb
		mov ax,di
		sub al,82h		; Include space but not CR
		mov [es:80h],al		; Store command line length

		; Now actually load the file...
		pop si			; File handle
		mov bx,100h		; Load at <seg>:0100h
		mov cx,[ClustPerMoby]	; Absolute maximum # of clusters
		call getfssec

		; And invoke the program...
		mov [SavedSSSP],sp
		mov [SavedSSSP+2],ss	; Save away SS:SP

		mov ax,es
		mov ds,ax
		mov ss,ax
		xor sp,sp
		push word 0		; Return to address 0 -> exit

		jmp comboot_seg:100h	; Run it

; Proper return vector
comboot_return:	cli			; Don't trust anyone
		xor ax,ax
		jmp comboot_exit

;
; Set up the COMBOOT API interrupt vectors.  This is also used
; by the COM32 code.
;
comboot_setup_api:
		mov di,4*0x20		; DOS interrupt vectors
		mov eax,comboot_return	; INT 20h = exit
		stosd
		mov ax,comboot_int21	; INT 21h = DOS-compatible syscalls
		stosd
		mov ax,comboot_int22	; INT 22h = proprietary syscalls
		stosd
		mov ax,comboot_bogus
		mov cx,29		; All remaining DOS vectors
		rep stosd
		ret

; INT 21h: generic DOS system call
comboot_int21:	cli
		push ds
		push es
		push fs
		push gs
		pushad
		cld
		mov bp,cs
		mov ds,bp
		mov es,bp
		mov bp,sp			; Set up stack frame

		call adjust_screen		; The COMBOOT program might have changed the screen

		mov cx,int21_count
		mov si,int21_table
.again:		lodsb
		cmp al,P_AH
		lodsw
		loopne .again
		; The last function in the list is the
		; "no such function" function

		call ax			; Call the invoked function
comboot_resume:
		setc P_FLAGSL		; Propagate CF->error
		popad
		pop gs
		pop fs
		pop es
		pop ds
		iret

; Attempted to execute non-21h DOS system call
comboot_bogus:	cli			; Don't trust anyone
		mov ax,err_notdos
;
; Generic COMBOOT return to command line code
;  AX -> message (if any)
;  BX -> where to go next
;
comboot_exit:
		mov bx,enter_command	; Normal return to command prompt
comboot_exit_special:
		xor dx,dx
		mov ds,dx
		mov es,dx
		lss sp,[SavedSSSP]
		sti
		cld
		call adjust_screen	; The COMBOOT program might have changed the screen
		and ax,ax
		je .nomsg
		mov si,KernelCName
		call cwritestr
		xchg si,ax
		call cwritestr
.nomsg:		jmp bx

;
; INT 21h system calls
;
comboot_getkey:				; 01 = get key with echo
		call vgashowcursor
		call comboot_getchar
		call vgahidecursor
		call writechr
		clc
		ret

comboot_writechr:			; 02 = writechr
		mov al,P_DL
		call writechr
		clc
		ret

comboot_writeserial:			; 04 = write serial port
		mov al,P_DL
		call write_serial
		clc
		ret

comboot_getkeynoecho:			; 08 = get key w/o echo
		call comboot_getchar
		clc
		ret

comboot_writestr:			; 09 = write DOS string
		mov es,P_DS
		mov si,P_DX
.loop:		es lodsb
		cmp al,'$'		; End string with $ - bizarre
		je .done
		call writechr
		jmp short .loop
.done:		clc
		ret

comboot_checkkey:			; 0B = check keyboard status
		cmp byte [APIKeyFlag],00h
		jnz .waiting
		call pollchar
.waiting:	setz al
		dec al			; AL = 0FFh if present, 0 if not
		mov P_AL,al
		clc
		ret

comboot_checkver:			; 30 = check DOS version
		; We return 0 in all DOS-compatible version registers,
		; but the high part of eax-ebx-ecx-edx spell "SYSLINUX"
		mov P_EAX,'SY' << 16
		mov P_EBX,'SL' << 16
		mov P_ECX,'IN' << 16
		mov P_EDX,'UX' << 16
		clc
		ret

comboot_getchar:
		cmp byte [APIKeyFlag],00h
		jne .queued
		call getchar		; If not queued get input
		and al,al		; Function key?
		jnz .done
		mov [APIKeyWait],ah	; High part of key
		inc byte [APIKeyFlag]	; Set flag
.done:		mov P_AL,al
		ret
.queued:	mov al,[APIKeyWait]
		dec byte [APIKeyFlag]
		jmp .done

;
; INT 22h - SYSLINUX-specific system calls
;           System call number in ax
;
comboot_int22:
		cli
		push ds
		push es
		push fs
		push gs
		pushad
		cld
		mov bp,cs
		mov ds,bp
		mov es,bp
		mov bp,sp			; Set up stack frame

		call adjust_screen		; The COMBOOT program might have changed the screen

		cmp ax,int22_count
		jb .ok
		xor ax,ax			; Function 0 -> unimplemented
.ok:
		xchg ax,bx
		add bx,bx
		call [bx+int22_table]
		jmp comboot_resume		; On return

;
; INT 22h AX=0000h	Unimplemented call
;
comapi_err:
		stc
		ret

;
; INT 22h AX=0001h	Get SYSLINUX version
;
comapi_get_version:
		; Number of API functions supported
		mov P_AX,int22_count
		; SYSLINUX version
		mov P_CX,(VER_MAJOR << 8)+VER_MINOR
		; SYSLINUX derivative ID byte
		mov P_DX,my_id
		; For future use
		mov P_BX,cs	; cs == 0

		mov P_ES,ds
		; ES:SI -> version banner
		mov P_SI,syslinux_banner
		; ES:DI -> copyright string
		mov P_DI,copyright_str

comapi_nop:
		clc
		ret

;
; INT 22h AX=0002h	Write string
;
; Write null-terminated string in ES:BX
;
comapi_writestr:
		mov ds,P_ES
		mov si,P_BX
		call writestr
		clc
		ret

;
; INT 22h AX=0003h	Run command
;
; Terminates the COMBOOT program and executes the command line in
; ES:BX as if it had been entered by the user.
;
comapi_run:
		mov ds,P_ES
		mov si,P_BX
		mov di,command_line
.copyloop:
		lodsb
		stosb
		and al,al
		jnz .copyloop
		xor ax,ax
		mov bx,load_kernel		; Run a new kernel
		jmp comboot_exit_special	; Terminate task, clean up

;
; INT 22h AX=0004h	Run default command		
;
; Terminates the COMBOOT program and executes the default command line
; as if a timeout had happened or the user pressed <Enter>.
;
comapi_run_default:
		mov bx,auto_boot
		jmp comboot_exit_special

;
; INT 22h AX=0005h	Force text mode
;
; Puts the video in standard text mode
;
comapi_textmode:
		call vgaclearmode
		clc
		ret

;
; INT 22h AX=0006h	Open file
;
comapi_open:
		push ds
		mov ds,P_ES
		mov si,P_SI
		mov di,InitRD
		push di
		call mangle_name
		pop di
		pop ds
		call searchdir
		jz .err
		mov P_AX,ax
		mov P_HAX,dx
		mov ax,[ClustSize]
		mov P_CX,ax
		mov P_SI,si
		clc
		ret
.err:
		stc
		ret


;
; INT 22h AX=0007h	Read file
;
comapi_read:
		mov es,P_ES
		mov bx,P_BX
		mov si,P_SI
		mov cx,P_CX
		call getfssec
		jnc .noteof
		xor si,si		; SI <- 0 on EOF, CF <- 0
.noteof:	mov P_SI,si
		ret

;
; INT 22h AX=0008h	Close file
;
comapi_close:
		; Do nothing for now.  Eventually implement
		; an internal API for this.
		clc
		ret

;
; INT 22h AX=0009h	Call PXE stack
;
%if IS_PXELINUX
comapi_pxecall:
		mov bx,P_BX
		mov es,P_ES
		mov di,P_DI
		call pxenv
		mov P_AX,ax
		clc
		ret
%else
comapi_pxecall	equ comapi_err			; Not available
%endif

;
; INT 22h AX=000Ah	Get Derivative-Specific Info
;
comapi_derinfo:
		mov P_AL,my_id
%if IS_SYSLINUX || IS_MDSLINUX
		mov al,[bsDriveNumber]
		mov P_DL,al
		mov P_ES,cs
		mov P_BX,PartInfo
%elif IS_PXELINUX
		mov ax,[APIVer]
		mov P_DX,ax
		mov ax,[StrucPtr]
		mov P_BX,ax
		mov ax,[StrucPtr+2]
		mov P_ES,ax
		mov ax,[InitStack]
		mov P_SI,ax
		mov ax,[InitStack+2]
		mov P_FS,ax
%elif IS_ISOLINUX
		mov al,[DriveNo]
		mov P_DL,al
		mov P_ES,cs
		mov P_BX,spec_packet
%endif
		clc
		ret

;
; INT 22h AX=000Bh	Get Serial Console Configuration
;
comapi_serialcfg:
		mov ax,[SerialPort]
		mov P_DX,ax
		mov ax,[BaudDivisor]
		mov P_CX,ax
		mov ax,[FlowControl]
		or al,ah
		mov ah,[FlowIgnore]
		shr ah,4
		mov P_BX,ax
		clc
		ret

;
; INT 22h AX=000Ch	Perform final cleanup
;
comapi_cleanup:
%if IS_PXELINUX
		; Unload PXE if requested
		test dl,3
		setnz [KeepPXE]
		sub bp,sp		; unload_pxe may move the stack around
		call unload_pxe
		add bp,sp		; restore frame pointer...
%elif IS_SYSLINUX || IS_MDSLINUX
		; Restore original FDC table
		mov eax,[OrigFDCTabPtr]
		mov [fdctab],eax
%endif
		; Reset the floppy disk subsystem
		xor ax,ax
		xor dx,dx
		int 13h
		clc
		ret


;
; INT 22h AX=000Dh	Clean up then replace bootstrap
;
comapi_chainboot:
		call comapi_cleanup
		mov esi,P_ESI
		mov edx,P_EBX
		mov bx,P_DS
		push P_EDI
		push P_ECX
		jmp replace_bootstrap

;
; This stuff should really be in the data section...
;
%macro 		int21 2
		db %1
		dw %2
%endmacro

int21_table:
		int21	00h, comboot_return
		int21	01h, comboot_getkey
		int21	02h, comboot_writechr
		int21	04h, comboot_writeserial
		int21	08h, comboot_getkeynoecho
		int21	09h, comboot_writestr
		int21	0Bh, comboot_checkkey
		int21	30h, comboot_checkver
		int21	4Ch, comboot_return
		int21	 -1, comboot_bogus
int21_count	equ ($-int21_table)/3

		align 2, db 0
int22_table:
		dw comapi_err			; 0000 unimplemented syscall
		dw comapi_get_version		; 0001 get SYSLINUX version
		dw comapi_writestr		; 0002 write string
		dw comapi_run			; 0003 run specified command
		dw comapi_run_default		; 0004 run default command
		dw comapi_textmode		; 0005 force text mode
		dw comapi_open			; 0006 open file
		dw comapi_read			; 0007 read file
		dw comapi_close			; 0008 close file
		dw comapi_pxecall		; 0009 call PXE stack
		dw comapi_derinfo		; 000A derivative-specific info
		dw comapi_serialcfg		; 000B get serial port config
		dw comapi_cleanup		; 000C perform final cleanup
		dw comapi_chainboot		; 000D clean up then bootstrap
int22_count	equ ($-int22_table)/2

APIKeyWait	db 0
APIKeyFlag	db 0

;; $Id: parseconfig.inc,v 1.11 2003/11/27 05:36:16 hpa Exp $
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
;; parseconfig.inc
;;
;; Configuration file operations
;;

;
; "default" command
; 
pc_default:	mov di,default_cmd
		call getline
		mov byte [di-1],0		; null-terminate
		ret

;
; "ontimeout" command
;
pc_ontimeout:	mov di,Ontimeout
		call getline
		sub di,Ontimeout+1		; Don't need final space
		mov [OntimeoutLen],di
		ret

;
; "onerror" command
;
pc_onerror:	mov di,Onerror
		call getline
		sub di,Onerror
		mov [OnerrorLen],di
		ret

;
; "append" command
;
pc_append:      cmp word [VKernelCtr],byte 0
		ja .vk
                mov di,AppendBuf
		call getline
                sub di,AppendBuf
.app1:        	mov [AppendLen],di
		ret
.vk:		mov di,VKernelBuf+vk_append	; "append" command (vkernel)
		call getline
		sub di,VKernelBuf+vk_append
                cmp di,byte 2
                jne .app2
                cmp byte [VKernelBuf+vk_append],'-'
                jne .app2
                xor di,di			; If "append -" -> null string
.app2:		mov [VKernelBuf+vk_appendlen],di
		ret

;
; "ipappend" command (PXELINUX only)
;
%if IS_PXELINUX
pc_ipappend:	call getint
		jc .err
		cmp word [VKernelCtr], byte 0
		jne .vk
		mov [IPAppend],bl
.err:		ret
.vk:		mov [VKernelBuf+vk_ipappend],bl
		ret
%endif

;
; "localboot" command (PXELINUX, ISOLINUX)
;
%if IS_PXELINUX || IS_ISOLINUX
pc_localboot:	call getint
		cmp word [VKernelCtr],byte 0	; ("label" section only)
		je .err
		mov [VKernelBuf+vk_rname], byte 0	; Null kernel name
		mov [VKernelBuf+vk_rname+1], bx	; Return type
.err:		ret
%endif

;
; "kernel" command
pc_kernel:	cmp word [VKernelCtr],byte 0
		je .err				; ("label" section only)
		call pc_getline
		mov di,VKernelBuf+vk_rname
		call mangle_name
.err:		ret

;
; "timeout" command
;
pc_timeout:	call getint
		jc .err
		mov ax,0D215h			; There are approx 1.D215h
		mul bx				; clock ticks per 1/10 s
		add bx,dx
		mov [KbdTimeOut],bx
.err:		ret

;
; Generic integer variable setting commands:
; "prompt", "implicit"
;
pc_setint16:
		push ax
		call getint
		pop si
		jc .err
		mov [si],bx
.err:		ret

;
; Generic file-processing commands:
; "display", "font", "kbdmap"
;
pc_filecmd:	push ax				; Function to tailcall
		call pc_getline
		mov di,MNameBuf
		push di
		call mangle_name
		pop di
		call searchdir			; tailcall
		jnz .ok
		pop ax				; Drop the successor function
.ok:		ret				; Tailcall if OK, error return

;
; "serial" command
;
pc_serial:	call getint
		jc .err
		push bx				; Serial port #
		call skipspace
		jnc .ok
		pop bx
.err:		ret
.ok:
		call ungetc
		call getint
		mov [FlowControl], word 0	; Default to no flow control
		jc .nobaud
.valid_baud:	
		push ebx
		call skipspace
		jc .no_flow
		call ungetc
		call getint			; Hardware flow control?
		jnc .valid_flow
.no_flow:
		xor bx,bx			; Default -> no flow control
.valid_flow:
		and bh,0Fh			; FlowIgnore
		shl bh,4
		mov [FlowIgnore],bh
		mov bh,bl
		and bx,0F003h			; Valid bits
		mov [FlowControl],bx
		pop ebx				; Baud rate
		jmp short .parse_baud
.nobaud:
		mov ebx,DEFAULT_BAUD		; No baud rate given
.parse_baud:
		pop di				; Serial port #
		cmp ebx,byte 75
		jb .err				; < 75 baud == bogus
		mov eax,BAUD_DIVISOR
		cdq
		div ebx
		mov [BaudDivisor],ax
		push ax				; Baud rate divisor
		cmp di,3
		ja .port_is_io			; If port > 3 then port is I/O addr
		shl di,1
		mov di,[di+serial_base]		; Get the I/O port from the BIOS
.port_is_io:
		mov [SerialPort],di
		lea dx,[di+3]			; DX -> LCR
		mov al,83h			; Enable DLAB
		call slow_out
		pop ax				; Divisor
		mov dx,di			; DX -> LS
		call slow_out
		inc dx				; DX -> MS
		mov al,ah
		call slow_out
		mov al,03h			; Disable DLAB
		add dx,byte 2			; DX -> LCR
		call slow_out
		in al,dx			; Read back LCR (detect missing hw)
		cmp al,03h			; If nothing here we'll read 00 or FF
		jne .serial_port_bad		; Assume serial port busted
		sub dx,byte 2			; DX -> IER
		xor al,al			; IRQ disable
		call slow_out

		add dx,byte 3			; DX -> MCR
		in al,dx
		or al,[FlowOutput]		; Assert bits
		call slow_out

		; Show some life
		mov si,syslinux_banner
		call write_serial_str
		mov si,copyright_str
		call write_serial_str
		ret

.serial_port_bad:
		mov [SerialPort], word 0
		ret

;
; "F"-key command
;
pc_fkey:	push ax
		call pc_getline
		pop di
		call mangle_name		; Mangle file name
		ret

;
; "label" command
;
pc_label:	call commit_vk			; Commit any current vkernel
		call pc_getline
		mov di,VKernelBuf+vk_vname
		call mangle_name		; Mangle virtual name
		inc word [VKernelCtr]		; One more vkernel
		mov si,VKernelBuf+vk_vname 	; By default, rname == vname
		mov di,VKernelBuf+vk_rname
		mov cx,FILENAME_MAX
		rep movsb
                mov si,AppendBuf         	; Default append==global append
                mov di,VKernelBuf+vk_append
                mov cx,[AppendLen]
                mov [VKernelBuf+vk_appendlen],cx
                rep movsb
%if IS_PXELINUX					; PXELINUX only
		mov al,[IPAppend]		; Default ipappend==global ipappend
		mov [VKernelBuf+vk_ipappend],al
%endif
		ret

;
; "say" command
;
pc_say:		call pc_getline			; "say" command
		call writestr
		jmp crlf			; tailcall

;
; Comment line
;
pc_comment	equ pc_getline			; Get a line and discard

;
; Common subroutine: load line into trackbuf; returns with SI -> trackbuf
;
pc_getline:	mov di,trackbuf
		push di
		call getline
		xor al,al
		stosb				; Null-terminate
		pop si
		ret

;
; Main loop for configuration file parsing
;
parse_config:
		call getcommand
                jnc parse_config		; If not EOF do it again
		;
		; The fall through to commit_vk to commit any final
		; VKernel being read
		;
;
; commit_vk: Store the current VKernelBuf into buffer segment
;
commit_vk:
		cmp word [VKernelCtr],byte 0
		je cvk_ret			; No VKernel = return
		cmp word [VKernelCtr],max_vk	; Above limit?
		ja cvk_overflow
		mov di,[VKernelCtr]
		dec di
		shl di,vk_shift
		mov si,VKernelBuf
		mov cx,(vk_size >> 2)
		push es
		push word vk_seg
		pop es
		rep movsd			; Copy to buffer segment
		pop es
cvk_ret:	ret
cvk_overflow:	mov word [VKernelCtr],max_vk	; No more than max_vk, please
		ret

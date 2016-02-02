;; $Id: conio.inc,v 1.6 2004/01/29 06:54:51 hpa Exp $
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
;; conio.inc
;;
;; Console I/O code, except:
;;   writechr, writestr		- module-dependent
;;   cwritestr, crlf		- writestr.inc
;;   writehex*			- writehex.inc
;;

;
; loadkeys:	Load a LILO-style keymap; SI and DX:AX set by searchdir
;
loadkeys:
		and dx,dx			; Should be 256 bytes exactly
		jne loadkeys_ret
		cmp ax,256
		jne loadkeys_ret

		mov bx,trackbuf
		mov cx,1			; 1 cluster should be >= 256 bytes
		call getfssec

		mov si,trackbuf
		mov di,KbdMap
		mov cx,256 >> 2
		rep movsd

loadkeys_ret:	ret
		
;
; get_msg_file: Load a text file and write its contents to the screen,
;               interpreting color codes.  Is called with SI and DX:AX
;               set by routine searchdir
;
get_msg_file:
		push es
		shl edx,16			; EDX <- DX:AX (length of file)
		mov dx,ax
		mov ax,xfer_buf_seg		; Use for temporary storage
		mov es,ax

                mov byte [TextAttribute],07h	; Default grey on white
		mov byte [DisplayMask],07h	; Display text in all modes
		call msg_initvars

get_msg_chunk:  push edx			; EDX = length of file
		xor bx,bx			; == xbs_textbuf
		mov cx,[BufSafe]
		call getfssec
		pop edx
		push si				; Save current cluster
		xor si,si			; == xbs_textbuf
		mov cx,[BufSafeBytes]		; Number of bytes left in chunk
print_msg_file:
		push cx
		push edx
		es lodsb
                cmp al,1Ah                      ; DOS EOF?
		je msg_done_pop
		push si
		mov cl,[UsingVGA]
		inc cl				; 01h = text mode, 02h = graphics
                call [NextCharJump]		; Do what shall be done
		pop si
		pop edx
                pop cx
		dec edx
		jz msg_done
		loop print_msg_file
		pop si
		jmp short get_msg_chunk
msg_done_pop:
                add sp,byte 6			; Drop pushed EDX, CX
msg_done:
		pop si
		pop es
		ret
msg_putchar:                                    ; Normal character
                cmp al,0Fh                      ; ^O = color code follows
                je msg_ctrl_o
                cmp al,0Dh                      ; Ignore <CR>
                je msg_ignore
                cmp al,0Ah                      ; <LF> = newline
                je msg_newline
                cmp al,0Ch                      ; <FF> = clear screen
                je msg_formfeed
		cmp al,19h			; <EM> = return to text mode
		je msg_novga
		cmp al,18h			; <CAN> = VGA filename follows
		je msg_vga
		jnb .not_modectl
		cmp al,10h			; 10h to 17h are mode controls
		jae msg_modectl
.not_modectl:

msg_normal:	call write_serial_displaymask	; Write to serial port
		test [DisplayMask],cl
		jz msg_ignore			; Not screen
                mov bl,[TextAttribute]
		mov bh,[BIOS_page]
                mov ah,09h                      ; Write character/attribute
                mov cx,1                        ; One character only
                int 10h                         ; Write to screen
                mov al,[CursorCol]
                inc ax
                cmp al,[VidCols]
                ja msg_line_wrap		; Screen wraparound
                mov [CursorCol],al

msg_gotoxy:     mov bh,[BIOS_page]
                mov dx,[CursorDX]
                mov ah,02h                      ; Set cursor position
                int 10h
msg_ignore:     ret
msg_ctrl_o:                                     ; ^O = color code follows
                mov word [NextCharJump],msg_setbg
                ret
msg_newline:                                    ; Newline char or end of line
		mov si,crlf_msg
		call write_serial_str_displaymask
msg_line_wrap:					; Screen wraparound
		test [DisplayMask],cl
		jz msg_ignore
                mov byte [CursorCol],0
                mov al,[CursorRow]
                inc ax
                cmp al,[VidRows]
                ja msg_scroll
                mov [CursorRow],al
                jmp short msg_gotoxy
msg_scroll:     xor cx,cx                       ; Upper left hand corner
                mov dx,[ScreenSize]
                mov [CursorRow],dh		; New cursor at the bottom
                mov bh,[ScrollAttribute]
                mov ax,0601h                    ; Scroll up one line
                int 10h
                jmp short msg_gotoxy
msg_formfeed:                                   ; Form feed character
		mov si,crff_msg
		call write_serial_str_displaymask
		test [DisplayMask],cl
		jz msg_ignore
                xor cx,cx
                mov [CursorDX],cx		; Upper lefthand corner
                mov dx,[ScreenSize]
                mov bh,[TextAttribute]
                mov ax,0600h                    ; Clear screen region
                int 10h
                jmp msg_gotoxy
msg_setbg:                                      ; Color background character
                call unhexchar
                jc msg_color_bad
                shl al,4
		test [DisplayMask],cl
		jz .dontset
                mov [TextAttribute],al
.dontset:
                mov word [NextCharJump],msg_setfg
                ret
msg_setfg:                                      ; Color foreground character
                call unhexchar
                jc msg_color_bad
		test [DisplayMask],cl
		jz .dontset
                or [TextAttribute],al		; setbg set foreground to 0
.dontset:
		jmp short msg_putcharnext
msg_vga:
		mov word [NextCharJump],msg_filename
		mov di, VGAFileBuf
		jmp short msg_setvgafileptr

msg_color_bad:
                mov byte [TextAttribute],07h	; Default attribute
msg_putcharnext:
                mov word [NextCharJump],msg_putchar
		ret

msg_filename:					; Getting VGA filename
		cmp al,0Ah			; <LF> = end of filename
		je msg_viewimage
		cmp al,' '
		jbe msg_ret			; Ignore space/control char
		mov di,[VGAFilePtr]
		cmp di,VGAFileBufEnd
		jnb msg_ret
		mov [di],al			; Can't use stosb (DS:)
		inc di
msg_setvgafileptr:
		mov [VGAFilePtr],di
msg_ret:	ret

msg_novga:
		call vgaclearmode
		jmp short msg_initvars

msg_viewimage:
		push es
		push ds
		pop es				; ES <- DS
		mov si,VGAFileBuf
		mov di,VGAFileMBuf
		push di
		call mangle_name
		pop di
		call searchdir
		pop es
		jz msg_putcharnext		; Not there
		call vgadisplayfile
		; Fall through

		; Subroutine to initialize variables, also needed
		; after loading a graphics file
msg_initvars:
                pusha
                mov bh,[BIOS_page]
                mov ah,03h                      ; Read cursor position
                int 10h
                mov [CursorDX],dx
                popa
		jmp short msg_putcharnext	; Initialize state machine

msg_modectl:
		and al,07h
		mov [DisplayMask],al
		jmp short msg_putcharnext

;
; write_serial:	If serial output is enabled, write character on serial port
; write_serial_displaymask: d:o, but ignore if DisplayMask & 04h == 0
;
write_serial_displaymask:
		test byte [DisplayMask], 04h
		jz write_serial.end
write_serial:
		pushfd
		pushad
		mov bx,[SerialPort]
		and bx,bx
		je .noserial
		push ax
		mov ah,[FlowInput]
.waitspace:
		; Wait for space in transmit register
		lea dx,[bx+5]			; DX -> LSR
		in al,dx
		test al,20h
		jz .waitspace

		; Wait for input flow control
		inc dx				; DX -> MSR
		in al,dx
		and al,ah
		cmp al,ah
		jne .waitspace	
.no_flow:		

		xchg dx,bx			; DX -> THR
		pop ax
		call slow_out			; Send data
.noserial:	popad
		popfd
.end:		ret

;
; write_serial_str: write_serial for strings
; write_serial_str_displaymask: d:o, but ignore if DisplayMask & 04h == 0
;
write_serial_str_displaymask:
		test byte [DisplayMask], 04h
		jz write_serial_str.end

write_serial_str:
.loop		lodsb
		and al,al
		jz .end
		call write_serial
		jmp short .loop
.end:		ret

;
; pollchar: check if we have an input character pending (ZF = 0)
;
pollchar:
		pushad
		mov ah,1		; Poll keyboard
		int 16h
		jnz .done		; Keyboard response
		mov dx,[SerialPort]
		and dx,dx
		jz .done		; No serial port -> no input
		add dx,byte 5		; DX -> LSR
		in al,dx
		test al,1		; ZF = 0 if data pending
		jz .done
		inc dx			; DX -> MSR
		mov ah,[FlowIgnore]	; Required status bits
		in al,dx
		and al,ah
		cmp al,ah
		setne al
		dec al			; Set ZF = 0 if equal
.done:		popad
		ret

;
; getchar: Read a character from keyboard or serial port
;
getchar:
		RESET_IDLE
.again:
		DO_IDLE
		mov ah,1		; Poll keyboard
		int 16h
		jnz .kbd		; Keyboard input?
		mov bx,[SerialPort]
		and bx,bx
		jz .again
		lea dx,[bx+5]		; DX -> LSR
		in al,dx
		test al,1
		jz .again
		inc dx			; DX -> MSR
		mov ah,[FlowIgnore]
		in al,dx
		and al,ah
		cmp al,ah
		jne .again
.serial:	xor ah,ah		; Avoid confusion
		xchg dx,bx		; Data port
		in al,dx
		ret
.kbd:		xor ax,ax		; Get keyboard input
		int 16h
		and al,al
		jz .func_key
		mov bx,KbdMap		; Convert character sets
		xlatb
.func_key:	ret

%ifdef DEBUG_TRACERS
;
; debug hack to print a character with minimal code impact
;
debug_tracer:	pushad
		pushfd
		mov bp,sp
		mov bx,[bp+9*4]		; Get return address
		mov al,[cs:bx]		; Get data byte
		inc word [bp+9*4]	; Return to after data byte
		call writechr
		popfd
		popad
		ret
%endif	; DEBUG_TRACERS

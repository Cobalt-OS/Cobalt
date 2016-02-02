; BLACKOUT.EXE (C) Dr. GJC Lokhorst 02/16/1993
; Suppresses messages from CONFIG.SYS and AUTOEXEC.BAT
; Tested with MS-DOS 3.3 - 5.0
; USAGE:
; First line of CONFIG.SYS:
;   device=[drive:][directory]blackout.exe
; Disables int 29h (DOS fast screen output)
; Last line of AUTOEXEC.BAT:
;   [drive:][directory]blackout.exe
; Reenables int 29h (DOS fast screen output)
;
; HOW TO ASSEMBLE
; I used Turbo Assembler, Version 1.0,
; Copyright (c) 1988 by Borland International
; 
; TASM /T/Z BLACKOUT,,NUL,NUL
; TLINK /X BLACKOUT
; DEL BLACKOUT.OBJ

pushall macro
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	push es
	push ds
	push bp
	pushf
endm

popall macro
	popf
	pop bp
	pop ds
	pop es
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
endm

.model small
.stack 80h ; seems enough

.code

assume ds:@code

deviceheader dd -1h
attribute dw 8004h ; char nul
strategy dw offset strategy_init
interrupt dw offset interrupt_init
dev_name:
db 13,"Dr. GJC Lokhorst 02/16/1993",26

endresident:

modify proc near	
	xor ax,ax
	mov ds,ax
	lds bx,dword ptr ds:[29h*4] ; int 29h
	cmp byte ptr [bx],0cfh	;iret
	je toggle
	cmp byte ptr [bx],50h	;push ax
	jne not_ok
	call toggle
; clrscr
; from transient portion of COMMAND.COM 5.0
	mov	ah,0fh
	int	10h
	cmp	al,03h
	jbe	label_112h
	cmp	al,07h
	jnz	endclrscr
label_112h:
	xor	ax,ax
	mov	ds,ax
	mov	dx,word ptr ds:[044ah]  ; dl = cols
	mov	dh,byte ptr ds:[0484h]	; last line, or 0
	or	dh,dh
	jnz	label_127h
	mov	dh,24
label_127h:
	dec	dl
	push	dx
	mov	ah,0bh
	xor	bx,bx
	int	10h
	pop	dx
	xor	ax,ax
	mov	cx,ax
	mov	ah,06h
	mov	bh,07h
	xor	bl,bl
	int	10h
	mov	ah,0fh
	int	10h
	mov	ah,02h
	xor	dx,dx
	int	10h
endclrscr:
not_ok:
	ret
endp

toggle proc near
	xor byte ptr [bx], (50h xor 0cfh)
	ret
endp

strategy_init proc far
	mov word ptr cs:[keep_bx],bx
	mov word ptr cs:[keep_es],es
	ret
endp

keep_bx:
	nop
	nop
keep_es:
	nop
	nop

interrupt_init proc far
	pushall
	call modify
; adapted from EGA.SYS 5.0
	lds bx,dword ptr cs:[keep_bx]
	xor ax,ax
	mov word ptr cs:[attribute],ax ; 
	mov word ptr [bx+10h],cs
	mov word ptr [bx+0eh],ax ; length of resident portion
	mov byte ptr [bx+0dh],al ; nr of units
	mov word ptr [bx+03h],810Ch ; done, general failure
	popall
	ret
endp

start:
	call modify
	mov ax,4c00h ;finish, exit code = OK
	int 21h

end start

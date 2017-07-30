;
; Simple COMBOOT program that just prints out its own command line.
; This also works in DOS.
;

	org 100h

_start:
	xor cx,cx
	mov cl,[80h]			; Command line len
	mov si,81h			; Command line

	mov dl,"<"
	mov ah,02h
	int 21h
	
.writechar:
	lodsb
	mov dl,al
	mov ah,02h
	int 21h
	loop .writechar

	mov dx,end_str
	mov ah,09h
	int 21h

	; Exit with near return, INT 20h, or INT 21h AX=4C00h
	ret
	
		
end_str	db ">", 0Dh, 0Ah, "$"
	
	
	
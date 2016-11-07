;
;	BANNER 1.2 - (C) 1997 Joseph P Morris
;
;	jpmorris@csm.uwe.ac.uk / boff@globalnet.co.uk
;	http://members.tripod.com/~JPMorris
;
;	Do what you will.. just remember that I wrote this originally!
;

;
; To assemble:
; 		nasm -o banner.com banner.asm
;

; History:
;
; 	1.0	Banner.c written.  Font is set to 8x1 characters.
;
;	1.1	Modified font code to do a 40x1 text window.
;
;	1.2	NASM port of banner.c
;
;

[BITS 16]
[ORG 0x100]
[SECTION .text]

	; begin by setting mode 0x13 (320x200x256)

	mov ax,0x13
	int 0x10

	; shrink font to 1 pixel high, 40x1 text window

	mov ax,0x1121
	mov bx,0
	mov cx,1
	mov dx,1
	push bp
	mov bp,data
	int 0x10
	pop bp

	; set palette [black]

	mov ax,0x1012
	mov cx,256
	mov dx,data
	int 0x10

	; open file

	mov ax,0x3d00
	mov dx,filename
	int 0x21
	jc bomb

	mov bx,ax

	; Now read palette

	mov ax,0x3f00
	mov cx,768
	mov dx,data
	int 0x21

	; Now read screen direct to video memory (0xa000:0000)

	push ds
	mov ax,0x3f00
	mov cx,64000

	mov dx,0xa000
	mov ds,dx

	mov dx,0x0000
	int 0x21
	pop ds

	; close file

	mov ax,0x3e00
	int 0x21

	; set palette

	mov ax,0x1012
	mov bx,0
	mov cx,256
	mov dx,data
	int 0x10

	; finish

_out:
	mov ah,0x4c
	int 0x21

	; error routine.. set mode 3 and quit

bomb:
	mov ax,3
	int 0x10
	jmp _out

;[SECTION .data]


data: times 768 db 0
filename: db 'c:\openinit.raw',0

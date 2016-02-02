 ; This file is part of CDRcache, the 386/XMS DOS CD-ROM cache by
 ; Eric Auer (eric@coli.uni-sb.de), based on LBAcache, 2001-2003.

 ; CDRcache is free software; you can redistribute it and/or modify
 ; it under the terms of the GNU General Public License as published
 ; by the Free Software Foundation; either version 2 of the License,
 ; or (at your option) any later version.

 ; CDRcache is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.

 ; You should have received a copy of the GNU General Public License
 ; along with CDRcache; if not, write to the Free Software Foundation,
 ; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 ; (or try http://www.gnu.org/licenses/licenses.html at www.gnu.org).

 ; -------------------------------------------------------------------


	; .8086 (Warning: nasm does not have this directive,)
	; (so we must simply be careful "by hand"... It does)
	; (have BITS 16 and BITS 32 to tell CS type, though.)

	; main install and setup routine follows (starts with
	; 8086 compatible code to detect 386, then does a check
	; for the existence of XMS, allocates the cache there,
	; and stores the drive geometry information for drives
	; 0x80 .. 0x87 for CHS <-> LBA conversion
	; install: is the entry point.
	; jumps to resinst: at the end.
	; FLOPPY: added change line detection -> fddstat word
	; *** NEW 11/2002: lba bit for each drive, 8 drives ..0x87
	; *** NEW 11/2002: honor amount of available RAM if DOS 5+
	; (should also honor it if < DOS 5 in .com mode...)

ramlimit	dw 0xfd00	; *** NEW 11/2002 (default is 64k)
				; *** (- space for .com and stack)
install:
;	cli	; cli not really needed...

getramlimit:		; *** memory calculations are NEW 11/2002
	push ax
		push bx
		push cx
	mov ah,0x30	; get DOS version
	int 0x21	; DOS
		pop cx	; ignore serial number
		pop bx	; ignore serial number and (BH) distro
			; distros: 0 default, 66 PTS, ee DR,
			; ef novell, fd FreeDOS
	cmp al,5	; major version at least 5 ?
	jb noramlimit	; only then [es:bx+0x0e] is valid,
			; see RBIL 61, table 02597, device driver
			; request header, command code 00h ...
	
	mov ax,[es:bx+0x10]	; top of RAM pointer: segment
		push bx
	mov bx,cs
	sub ax,bx		; relative to ours
		pop bx
	cmp ax,1000h
	ja noramlimit		; > 64k, fine
	shl ax,1		; make bytes from the difference
	shl ax,1		; (convert to linear address space)
	shl ax,1
	shl ax,1
	add ax,[es:bx+0x0e]	; top of RAM pointer: offset
	jc noramlimit		; > 64k, fine
	mov [cs:ramlimit],ax	; UPDATE RAM limit (CS end) value
noramlimit:
	pop ax			; *** end of memory calc, NEW 11/2002

; -------------

	mov word [es:bx+0x10],cs	; pointer to first free byte
	mov word [es:bx+0x0e],0		; default: do not use any RAM
		; if load failed... either EOFTSR:
		; (this will leave a minimal "if (!cache enabled)
		;  { goto old int 0x13; }" handler and not much more)
		; OR 0 (do not stay tsr at all, possible only
		; if int 0x13 was not hooked (obviously)

; -------------

; test if we have 386 or better: 
	push ax
	pushf   ; save flags
		xor ax,ax
		push ax
		popf	; try to clear all bits
		pushf
	        pop ax
	and ax,0f000h
	cmp ax,0f000h
	jz noinst1	; 4 msb stuck to 1: 808x or 80186
		mov ax,0f000h
		push ax
		popf	; try to set 4 msb
		pushf
		pop ax
	test ax,0f000h
	jz noinst1	; 4 msb stuck to 0: 80286
	jmp short okinst1
noinst1:	; failed, no 386 found
	push bx
	push si
	push ds
	mov ax,cs
	mov ds,ax
	cld			; flags are saved :-)
	mov si,err386
		call strtty	; complain: no 386 found
	mov si,hello
		call strtty	; show banner
	pop ds
	pop si
	pop bx
	popf			; * swap fixed 8/2003
	pop ax			; * swap fixed 8/2003
	jmp quitinst

	; .386 ( Warning: nasm does not have this directive, )
	; (but remember that you may use 80386 code below :-))

okinst1:
	popf	; good, it is a 386, now restore flags
	pop ax

; -------------

	pusha			; after this point, we must pass
	push ds			; quitpop, not only quitinst...

	mov ax,cs
	mov ds,ax		; save all regs and set DS to our CS
	cld			; flags are saved :-)

	mov si,hello
		call strtty	; show banner

; -------------

	call parsecommandline	; the COMMAND LINE should be parsed as
				; soon as possible, so we do it now.
				; (uses the struct at es:bx)
	jnc go_tsr		; returns carry on syntax error.

	mov si,notsrmsg		; display message and EXIT on error.
		call strtty
	jmp quitpop		; leave this here!

go_tsr:

; -------------

; test if we have XMS. allocate XMS. fail if none/not enough found.

	mov ax,0x4300
		push ax		; for debugging: put "why" code on stack
	mov bl,-1		; clear errorcode value
		int 0x2f	; xms installation check
	cmp al,0x80
	jnz short noxms		; xms not present
	mov ax,0x4310
	push es
		int 0x2f	; get xms call vector (may clobber regs)
	mov [xmsvec],bx
	mov [xmsvec+2],es
	pop es
		pop ax		; for debugging: remove "why" code
	mov ah,8
		push ax		; for debugging: new "why" code
		call far [xmsvec]	; check amount of free XMS
	or ax,ax
	jz short noxms		; errorcode in bl if ax zero
	mov bx,[ds:sectors]	; ds: prefix needed, see above

	; *** use rounding if sector size is < 1k... e.g. "inc bx"
	test bx,0x8000	; *** would need > 64 MB (*2048* byte sectors)?
	jnz short noxms	; *** if yes, bad luck. No XXMS 3.0 supported.

	shl bx,1	; amount of XMS we need (kbytes). *2048* by sectors!
	cmp dx,bx	; DX total, AX biggest chunk (kbyte)
	jb short noxms
	mov [cs:xmssize],bx	; store for the statistics display function
	cmp ax,bx
		pop ax		; for debugging: remove "why" code
		push word 0xfe00	; for debugging: new "why" code
	jb short noxms		; not enough XMS free
		pop ax		; for debugging: remove "why" code
	mov ah,9
		push ax		; for debugging: new "why" code
	mov dx,bx
		call far [xmsvec]	; alloc DX kbyte for us
	or ax,ax
	jz short noxms		; errorcode in bl if ax zero
		pop ax		; for debugging: remove "why" code
	mov [xmshandle],dx	; our handle

	; (to free the memory, we would use function 0x0a and handle in DX)
	; (only other function used is copy, 0x0b)

	jmp short finddevice

noxms:				; ERROR: no or not enough XMS found
		pop ax		; meep displays errorcode from bl...
		mov al,bl	; ...and the "why" code from stack.hi
		push word xmserr
		call meep	; give feedback

	mov si,xmserr2		; no or not enough XMS found
		call strtty	; minimalistic error message

instfailed:
	jmp quitpop		; pop ds, popa, ...

; now we have the XMS we need, after flushing the status table and
; finding the geometry we can hook int 0x13 and return...

; -------------

nofounddevice:
	mov si,errdrv		; error connecting the CD-ROM driver
		call strtty
	mov si,clientname	; ASCIIZ string offset of client device name
		call strtty
	mov si,errdrv2
		call strtty
	mov si,notsrmsg		; not going resident!
		call strtty
	jmp short instfailed

finddevbuf	db 0,0,0,0,0	; (type byte followed by far pointer)
				; type 0 is "get device header address"

finddevice:			; try to connect the CD-ROM driver
	mov dx,clientname	; ASCIIZ string offset
	mov ax,0x3d00		; open for read in compatibility share mode
	int 0x21		; DOS
	jc nofounddevice	; give up if error
	  push bx
	mov bx,ax		; the handle
	mov ax,0x4402		; read char device control channel
	mov cx,5		; size 5: type byte plus far pointer
	mov dx,finddevbuf	; offset of buffer
	int 0x21		; DOS
	mov ax,bx
	  pop bx
	jc nofounddevice	; give up if error
	  push bx
	mov bx,ax
	mov ah,0x3e		; close (only error could be inv. handle)
	int 0x21		; DOS
	    push es
	les bx,[ds:finddevbuf+1]	; read the pointer to the device
	mov [oldstra+2],es	; segment is obvious
	mov [oldintr+2],es	; segment is obvious
	mov [oldhead+2],es	; segment is obvious
	mov [oldhead],bx	; we do need the header address later!
	mov ax,[es:bx+6]	; offset of "strategy" handler
	mov [oldstra],ax
	mov ax,[es:bx+8]	; offset of "interrupt" handler
	mov [oldintr],ax
	mov al,[es:bx+0x15]	; ask how many CD-ROM drives are connected
	mov [unitcount],al	; ... and then cache all of them!

%if 1
	; Read LASTDRIVE value...
	push dx
	mov ah,0x19		; find out on which drive we are
	int 0x21		; DOS
	mov ah,0x0e		; select drive DL (0=a:)
	mov dl,al		; (the current drive letter from func 19h)
	int 0x21		; DOS
	pop dx			; returned AL: number of possible drives
%else
	mov al,26		; 1st drive letter: "Y:" if 1 device,
%endif
	sub al,[ds:unitcount]	; "X:" if 2 devices, etc. (Here 1=a: etc.!)
				; (What if we double-used a letter now???)
	mov [es:bx+0x14],al	; *** Important: *CDEX is not yet loaded,
				; *** so it is OUR task to tell the CD-ROM
				; *** driver which drive letter to use!

	; Some special device header fields for CD-ROM:
	; [0] dd pointer to next driver or ????:0xffff
	; [4] dw device attributes (0xc800 for CD-ROM: char with IOCTL, OPEN)
	; [a] 8 db space-padded name (char devices only, in block device this
	;          would be a byte "number of subunits" followed by a name).
	; [12h] dw 0, [14h] db drive letter (initially 0, 1=a: set by *CDEX!)
	; [15h] db number of drives (drive letters count on...)

	    pop es
	  pop bx

	; *** Here we could display a message that we found the
	; *** CD-ROM device driver with X drives connected and
	; *** which drive letter we have assigned to it.

; -------------

	jmp short malloc

; -------------

smalloc:				; smallify the malloc
	sub word [cs:sectors],16	; use less memory,
					; ... and then TRY AGAIN !
	mov si,dotmsg			; string with a single dot
		call strtty		; print "." each time you shrunk

malloc:				; allocate memory for us and the table

	; ptr[es:bx+0x0e] will tell us a limit, but only for DOS 5+
	; The "ramlimit" variable holds a copy of the relevant data.

	mov ax,[cs:sectors]		; check size requested...
		call telltabsize	; calculate tab size
	jc smalloc			; was far too big


					; *** new 01/2002
mallocstack:
	add ax,table+15+300		; *offset* add 300 byte stack
	  pushf				; *** NEW only 300, 11/2002
	  push ax
	sub ax,4
	and ax,0xfffc
	mov [cs:localsp],ax
	  pop ax
	  popf
; -	jmp short knowstack
; -	
; - nomallocstack:
; -	mov si,stacknomsg		; *** 11/2002
; -		call strtty		; *** 11/2002
; -	mov byte [cs:stacknomsg],'.'	; *** only print ONCE
; -	mov byte [cs:stacknomsg+1],0	; *** then print dots
; -
; -	add ax,table+15	; *offset*	; new eof tsr point

knowstack:

	jbe smalloc			; reduce + TRY AGAIN !

	cmp ax, [cs:ramlimit]		; *** NEW 11/2002
	jae smalloc			; *** Too big for here?

					; (reload in case it got trashed)
	les bx,[cs:pb]			; *** driver data structure

	mov word [es:bx+0x0e],ax	; new end of our TSR, (seg
					; already set to CS above)

; -------------

showxmssize:
	mov ax,[cs:sectors]		; Tell user how big the cache is
	push cx				; %
	mov cl,10-1			; % 2 sectors/kB, 1<<10 kB/MB
					; % (sector size fixed at *2048*!)
	shr ax,cl			; % never more than 64
	aam				; % AAM: ah=al div 10, al=al mod 10
	add ax,'00'			; %
	xchg al,ah			; % make 10s display left of 1s
	cmp al,'0'			; % leading 0 ?
	jnz nosuppxmszero		; % if yes, suppress
	mov al,' '			; %
nosuppxmszero:				; %
	mov [cs:xmsSZmsg2],ax		; % megabyte part (high=1s, low=10s)
	mov ax,[cs:sectors]		; % now figure out the decimals
	mov cl,8-1			; % 2 sectors/kB, units of 256k
					; % (sector size fixed at *2048*!)
	shr ax,cl			; %
	and ax,3			; % mask out multiples of 1024k
	pop cx				; %
	push bx				; %
	add ax,ax			; %
	mov bx,ax			; %
	mov ax,[cs:quartlist+bx]	; % translate to '00' '25' '50' '75'
	mov [cs:xmsSZmsg3],ax		; % decimals part (2 digits)
	pop bx				; %

	mov si,xmsSZmsg
		call strtty		; % show size of XMS alloc, announce
					; % showing of CS (DOS RAM) alloc size
	mov ax,[es:bx+0x0e]		; end of used part of our CS (and DS)

	push dx				; %
	push bx				; %
	mov si,drvSZmsgend+5		; % at most 5 digits
drvszhex2decloop:			; %
	dec si				; % move cursor left for next digit
	xor dx,dx			; % 
	mov bx,10			; % convert to decimal
	div bx				; %
	add dl,"0"			; % remainder becomes low digit
	mov [cs:si],dl			; %
	test ax,ax			; % any digits left?
	jnz short drvszhex2decloop	; %

	pop bx				; %
	pop dx				; %

	; % si pointing to start of decimal number now!
		call strtty		; "????? bytes" + CRLF

; -------------

	; (do other stuff here - table is still unflushed!)
	; (running is still 0, too)

	mov si,connectmsg
		call strtty	; tell that we are connected
	mov si,nam		; luckily there is a 0 after that
		call strtty	; show name of the cache device
	mov si,connectmsg2
		call strtty
	mov si,clientname
		call strtty	; show name of the client device
	mov si,connectmsg3
		call strtty

	jmp resinst	; jump out of the way, rest of inst will be
			; overwritten with the status table!

	; will do: call flush, pop ds, popa, mov word [cs:running],1
	; and jmp nix (to return to the device handler...).

; -------------

notsrmsg	db 'CDRcache will not stay resident.',13,10,13,10,0

dotmsg		db '.',0	; indicator for "reducing memory"
	; *** NEW 11/2002

xmserr2		db ' I need a 386 and enough free XMS memory.'
		db 13,10,0
err386		db ' This software needs at least an 80386 CPU',13,10

errdrv  	db 'Cannot open client CD-ROM device driver "',0
errdrv2		db '".',13,10,0

xmsSZmsg	db 'XMS allocated: '
xmsSZmsg2	db '00.'
xmsSZmsg3	db '00 MB, driver size with tables and stack: ',0
		; *** xmsSZmsg and drvSZmsg are one long string together
quartlist	db '00255075' ; 00 25 50 75 (1/4ths of a megabyte)
drvSZmsgend	db '_____ bytes.',13,10,0

connectmsg	db 'CDRcache loaded as device ',0
connectmsg2	db ' - caching the device(s) of ',0
connectmsg3	db '.',13,10,13,10,0


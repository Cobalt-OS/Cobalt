; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2003-2009.
; FDAPM is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published
; by the Free Software Foundation; either version 2 of the License,
; or (at your option) any later version.
; ### FDAPM is distributed in the hope that it will be useful, but
; ### WITHOUT ANY WARRANTY; without even the implied warranty of
; ### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; ### GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with FDAPM; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
; (or try http://www.gnu.org/licenses/ at http://www.gnu.org).

; This file: flushCaches (destroys registers)
; Call to flush all known caches. Writes status to stdout.
; Update 5/2005: Wait a bit and stop floppy motor at the end.
; Update 9/2009: Fix bug that crashed on SMARTDRV flush attempt,
;   call int 28 before flushes, call int 13.0 only 1x with dx=80

idleflushmsg	db "Calling int28 so MSCLIENT can flush...",13,10,"$"
cdbflushmsg	db "Flushing CD Blitz cache...",13,10,"$"
pccflushmsg	db "Flushing PC-Cache...",13,10,"$"
qcflushmsg	db "Flushing QuickCache...",13,10,"$"
spckflushmsg	db "Flushing Super PC Kwik / PC-Tools / QCache...",13,10,"$"
sdrv1flushmsg	db "Flushing SMARTDRV.EXE...",13,10,"$"
sdrv2flushmsg	db "Flushing SMARTDRV.SYS...",13,10,"$"
genflushmsg	db "Resetting DOS filesystem and BIOS disk handler.$"

flusherrmsg	db 13,10,"Flushing command returned an error.",13,10,"$"
flushokmsg	db " done.",13,10,"$"

twobyte		db 2
smartname	db "SMARTAAR",0


flushErrorMsg:	; print error or done message depending on carry
	push ax
	push dx
	mov dx,flusherrmsg
	jc yFEM
	mov dx,flushokmsg
yFEM:	mov ah,9
	int 21h
	pop dx
	pop ax
	ret

flushCaches:	; flush all known caches - destroys some registers
	mov dx,idleflushmsg
	mov ah,9
	int 21h
	int 28h	; seems to help MSCLIENT and NetWare to flush (9/2009)
	; inspired by Aitors APMLIB
	mov ax,1500h
	mov bx,1234h
	mov ch,90h
	int 2fh	; CDBLITZ install check
	cmp cx,1234h
	jnz nCDblitz
	mov dx,cdbflushmsg
	mov ah,9
	int 21h
	mov ax,1500h
	mov bx,1234h
	mov ch,96h
	int 2fh	; CDBLITZ flush
	call flushErrorMsg
nCDblitz:
	mov ax,0ffa5h
	mov cx,1111h
	int 16h	; PC Cache install check
	or cx,cx
	jnz nPCcache
	mov dx,pccflushmsg
	mov ah,9
	int 21h
	mov ax,0ffa5h
	mov cx,0ffffh
	int 16h	; PC Cache flush
	clc	; always works
	call flushErrorMsg
nPCcache:
	mov ah,27h
	xor bx,bx
	int 13h	; Quick Cache install check
	or ax,ax
	jnz nQuickCache
	or bx,bx
	jnz nQuickCache
	mov ah,9
	mov dx,qcflushmsg
	int 21h
	mov ax,0021h
	xor dx,dx
	int 13h	; Quick Cache flush
	; should CY / NC or AX be used for status?
	call flushErrorMsg
nQuickCache:
	mov cx,4358h
	mov ah,2bh
	int 21h	; Super PC Kwik / PC Tools / QCache install check
	or al,al
	jnz nSPCKwik
	mov dx,spckflushmsg
	mov ah,9
	int 21h
	mov ax,00a1h
	xor dx,dx
	mov si,4358h
	int 13h	; Super PC Kwik flush
	call flushErrorMsg
nSPCKwik:
	mov ax,4a10h
	xor bx,bx
	mov cx,0ebabh
	push ds
	int 2fh	; SMARTDRV.exe install check
	pop ds
	cmp ax,0babeh
	jnz nSdrv1
	mov dx,sdrv1flushmsg
	mov ah,9
	int 21h
	mov ax,4a10h	; Fix 9/2009 (4a01 was wrong)
	mov bx,1
	int 2fh	; SMARTDRV.exe flush
	clc	; always succeeds!?
	call flushErrorMsg
nSdrv1:
	mov ax,3d00h	; open for read: SMARTAAR (device)
	mov dx,smartname
	int 21h		; open handle to device of smartdrv.sys
	jc nSdrv2	; could not open -> no smartdrv.sys
	mov bx,ax	; handle
	push bx
	mov dx,sdrv2flushmsg
	mov ah,9
	int 21h
	pop bx
	mov ax,4403h
	mov cx,0
	mov dx,twobyte
	int 21h	; SMARTDRV.sys flush
	call flushErrorMsg
	; bx still is handle
	mov ah,3eh
	int 21h		; close handle again
nSdrv2:
	mov dx,genflushmsg	; standard flush, always done
	mov ah,9
	int 21h
	mov ah,0dh	; DOS filesystem reset (never fails)
	int 21h
	; Update 9/2009: This fails on modern PCs without floppy
	;	xor dx,dx	; floppies
	;	xor ax,ax	; reset disks
	;	int 13h
	;	jnc nGen	; do not display "done" twice
	;	call flushErrorMsg
nGen:	mov dx,80h	; harddisks (and floppies, if any)
	xor ax,ax	; reset disks
	int 13h
	mov cx,2	; wait 2 seconds
	call countBar	; see reboot.asm
	mov dx,3f2h	; floppy motor control
	xor ax,ax
	out dx,al	; stop all floppy / diskette drive motors
	call flushErrorMsg
	;
	ret


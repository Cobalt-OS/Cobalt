; -*- fundamental -*- (asm-mode sucks)
; $Id: syslinux.asm,v 1.16 2002/06/02 05:12:25 hpa Exp $
; -----------------------------------------------------------------------
;   
;   Copyright 1998-2001 H. Peter Anvin - All Rights Reserved
;
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
;   USA; either version 2 of the License, or (at your option) any later
;   version; incorporated herein by reference.
;
; -----------------------------------------------------------------------

;
; syslinux.asm
;
;	DOS installer for SYSLINUX
;

			absolute 0
pspInt20:		resw 1
pspNextParagraph:	resw 1
			resb 1		; reserved
pspDispatcher:		resb 5
pspTerminateVector:	resd 1
pspControlCVector:	resd 1
pspCritErrorVector:	resd 1
			resw 11		; reserved
pspEnvironment:		resw 1
			resw 23		; reserved
pspFCB_1:		resb 16
pspFCB_2:		resb 16
			resd 1		; reserved
pspCommandLen:		resb 1
pspCommandArg:		resb 127

		section .text
		org 0100h
_start:		
		mov ax,3000h			; Get DOS version
		int 21h
		xchg al,ah
		mov [DOSVersion],ax
		cmp ax,0314h			; DOS 3.20 minimum
		jae dosver_ok
		mov dx,msg_ancient_err
		jmp die

		section .bss
		alignb 2
DOSVersion:	resw 1

		section .text
;
; Scan command line for a drive letter followed by a colon
;
dosver_ok:
		xor cx,cx
		mov si,pspCommandArg
		mov cl,[pspCommandLen]
		
cmdscan1:	jcxz bad_usage			; End of command line?
		lodsb				; Load character
		dec cx
		cmp al,' '			; White space
		jbe cmdscan1
		cmp al,'-'
		je scan_option
		or al,020h			; -> lower case
		cmp al,'a'			; Check for letter
		jb bad_usage
		cmp al,'z'
		ja bad_usage
		sub al,'a'			; Convert to zero-based index
		mov [DriveNo],al		; Save away drive index

		section .bss
DriveNo:	resb 1

		section .text
;
; Got the leading letter, now the next character must be a colon
;
got_letter:	jcxz bad_usage
		lodsb
		dec cx
		cmp al,':'
		jne bad_usage
;
; Got the colon; the rest better be whitespace
;
got_colon:	jcxz got_cmdline
		lodsb
		dec cx
		cmp al,' '
		jbe got_colon
;
; We end up here if the command line doesn't parse
;
bad_usage:	mov dx,msg_unfair
		jmp die

		section .data
msg_unfair:	db 'Usage: syslinux [-s] <drive>:', 0Dh, 0Ah, '$'

		section .text
;
; Scan for options after a - sign.  The only recognized option right now
; is -s.
;
scan_option:	jcxz bad_usage
		lodsb
		dec cx
		cmp al,' '
		jbe cmdscan1
		or al,20h
		cmp al,'s'
		jne bad_usage
		push si			; make_stupid doesn't save these
		push cx
		call make_stupid	; Enable stupid boot sector
		pop cx
		pop si
		jmp short scan_option

;
; Parsed the command line OK.  Check that the drive parameters are acceptable
;
		struc DPB
dpbDrive:	resb 1
dpbUnit:	resb 1
dpbSectorSize:	resw 1
dpbClusterMask:	resb 1
dpbClusterShift: resb 1
dpbFirstFAT:	resw 1
dpbFATCount:	resb 1
dpbRootEntries:	resw 1
dpbFirstSector:	resw 1
dpbMaxCluster:	resw 1
dpbFATSize:	resw 1
dpbDirSector:	resw 1
dpbDriverAddr:	resd 1
dpbMedia:	resb 1
dpbFirstAccess:	resb 1
dpbNextDPB:	resd 1
dpbNextFree:	resw 1
dpbFreeCnt:	resw 1
		endstruc

got_cmdline:
		mov dl,[DriveNo]
		inc dl				; 1-based
		mov ah,32h
		int 21h				; Get Drive Parameter Block
		
		and al,al
		jnz filesystem_error

		cmp word [bx+dpbSectorSize],512	; Sector size = 512 required
		jne sectorsize_error

		cmp byte [bx+dpbClusterShift],5	; Max size = 16K = 2^5 sectors
		jna drive_ok

hugeclust_error:
		mov dx,msg_hugeclust_err
		jmp die
filesystem_error:
		mov dx,msg_filesystem_err
		jmp doserr
sectorsize_error:
		mov dx,msg_sectorsize_err
		jmp die

drive_ok:
		push cs
		pop ds

;
; Writing LDLINUX.SYS
;
		section .data
ldlinux_sys_str:
		db 'A:\LDLINUX.SYS', 0
		section .text

write_file:
		; 0. Set the correct filename

		mov al,[DriveNo]
		add byte [ldlinux_sys_str],al

		; 1. If the file exists, strip its attributes and delete

		xor cx,cx			; Clear attributes
		mov dx,ldlinux_sys_str
		mov ax,4301h			; Set file attributes
		int 21h

		mov dx,ldlinux_sys_str
		mov ah,41h			; Delete file
		int 21h

		; 2. Create LDLINUX.SYS and write data to it

		mov dx,ldlinux_sys_str
		xor cx,cx			; Normal file
		mov ah,3Ch			; Create file
		int 21h
		jc .file_write_error
		mov [FileHandle],ax

		mov bx,ax
		mov cx,ldlinux_size
		mov dx,LDLinuxSYS
		mov ah,40h			; Write data
		int 21h
		jc .file_write_error
		cmp ax,ldlinux_size
		je .no_file_write_error
.file_write_error:
		mov dx, msg_fwrite_err
		jmp doserr
.no_file_write_error:

		mov bx,[FileHandle]
		mov ah,3Eh			; Close file
		int 21h

		section .bss
FileHandle:	resw 1

		section .text

		; 3. Set the readonly flag on LDLINUX.SYS

		mov dx,ldlinux_sys_str
		mov cx,1			; Read only
		mov ax,4301h			; Set attributes
		int 21h

;
; Now, if we're on a recent Windows system we need to lock the device.
; This call should have no effect on plain DOS.
;
lock_drive:
		cmp word [DOSVersion], 0700h	; Win9x/NT?
		jb .plain_dos			; Plain DOS -> no locking

		mov ax,440Dh			; Generic IOCTL
		mov bl,[DriveNo]
		inc bl				; 1-based
		mov bh,1			; Lock level 1
		mov cx,084Ah			; Lock logical volume
		mov dx,01h			; Allow write mappings/allow new mappings
		pusha
		int 21h
		jc .disk_lock_error_nocleanup
		popa

		xor dx,dx
		inc bh				; Lock level 2
		pusha
		int 21h
		jc .disk_lock_error
		popa

		inc bh				; Lock level 3
		pusha
		int 21h
		jnc .done

.disk_lock_error:
		xor cx,cx
		mov cl,bh
		dec cx
.lock_cleanup:
		push cx
		mov ax, 440Dh
		mov bl,[DriveNo]
		inc bl
		mov cx,086Ah
		int 21h
		pop cx
		loop .lock_cleanup

.disk_lock_error_nocleanup:
		popa
		mov dx, msg_lock_err
		jmp doserr

.done:
		popa

.plain_dos:	; Plain DOS -> no locking

;
; Now read the old boot sector and copy the superblock.
;
		section .data
		align 4, db 0
DISKIO		equ $
diStartSector:	dd 0				; Absolute sector 0
diSectors:	dw 1				; One sector
diBuffer:	dw SectorBuffer			; Buffer offset
		dw 0				; Buffer segment

		section .text
read_bootsect:
		mov ax,cs			; Set DS <- CS
		mov ds,ax

		cmp word [DOSVersion],0400h	; DOS 4.00 has a new interface
		jae .new
.old:
		mov bx,SectorBuffer
		mov cx,1			; One sector
		jmp short .common
.new:
		mov bx,DISKIO
		mov [bx+8],ax			; Buffer segment
		mov cx,-1
.common:
		xor dx,dx			; Absolute sector 0
		mov al,[DriveNo]
		int 25h				; DOS absolute disk read
		pop ax				; Remove flags from stack
		jc disk_read_error

		mov si,SectorBuffer+11		; Offset of superblock
		mov di,BootSector+11
		mov cx,51			; Superblock = 51 bytes
		rep movsb			; Copy the superblock
		jmp short write_bootsect
disk_read_error:
		mov dx,msg_read_err
		jmp doserr

;
; Writing boot sector
;
write_bootsect:
		cmp word [DOSVersion],0400h	; DOS 4.00 has a new interface
		jae .new
.old:
		mov bx,BootSector
		mov cx,1			; One sector
		jmp short .common
.new:
		mov bx,DISKIO
		mov word [bx+6],BootSector
		mov cx,-1
.common:
		xor dx,dx			; Absolute sector 0
		mov al,[DriveNo]
		int 26h				; DOS absolute disk write
		pop ax				; Remove flags from stack
		jc disk_write_error

;
; Unlock the disk if we had to lock it
;
unlock_disk:
		cmp word [DOSVersion], 0700h
		jb .plain_dos

		mov cx,	3			; Need to release lock 3 times
.loop:
		push cx
		mov ax,440Dh			; Generic IOCTL
		mov bl,[DriveNo]
		inc bl				; 1-based drive number
		mov cx,086Ah			; Unlock logical drive
		int 21h
		pop cx
		loop .loop

.plain_dos:	; Plain DOS -> no locking

all_done:	mov ax,4C00h			; Exit good status
		int 21h
;
; Error routine jump
;
disk_write_error:
		mov dx,msg_write_err

doserr:
		push cs
		pop ds
		push dx				; Error message
		push ax				; Error code
		mov dx, msg_error_sp
		mov ah,09h
		int 21h
		pop ax
		
		mov cx,4
		mov bx,hexdigits
		mov si,ax
.digit:
		rol si,1
		rol si,1
		rol si,1
		rol si,1
		mov ax,si
		and al,0Fh
		xlatb
		mov ah,02h			; Display character
		mov dl,al
		int 21h
		loop .digit

		mov dx,msg_colon
		mov ah,09h
		int 21h

		jmp short die_common

		section .data
hexdigits:	db '0123456789ABCDEF'

		section .text
die:
		push cs
		pop ds
		push dx
		mov dx, msg_error
		mov ah,09h
		int 21h

die_common:
		pop dx				; Error message

		mov ah,09h			; Write string
		int 21h

		mov ax,4C01h			; Exit error status
		int 21h

;
; Patch the code to make it "stupid"
;
make_stupid:
		; Only access one sector at a time
		mov word [LDLinuxSYS+PATCH_OFFSET],1
		ret

			section .data
msg_error_sp:		db 'ERROR $'
msg_colon:		db ': $'
msg_error:		db 'ERROR: $'
msg_ancient_err:	db 'DOS version 3.20 or later required', 0Dh, 0Ah, '$'
msg_filesystem_err:	db 'Filesystem not found on disk', 0Dh, 0Ah, '$'
msg_sectorsize_err:	db 'Sector sizes other than 512 bytes not supported', 0Dh, 0Ah, '$'
msg_hugeclust_err:	db 'Clusters larger than 16K not supported', 0Dh, 0Ah, '$'
msg_read_err:		db 'Boot sector read failed', 0Dh, 0Ah, '$'
msg_write_err:		db 'Boot sector write failed', 0Dh, 0Ah, '$'
msg_fwrite_err:		db 'LDLINUX.SYS write failed', 0Dh, 0Ah, '$'
msg_lock_err:		db 'Unable to lock drive for exclusive access', 0Dh, 0Ah, '$'

		section .data
		align 16, db 0
BootSector:	incbin "ldlinux.bss"
LDLinuxSYS:	incbin "ldlinux.sys"
ldlinux_size:	equ $-LDLinuxSYS

		section .bss
		alignb 16
SectorBuffer:	resb 512

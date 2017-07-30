; -*- fundamental -*- (asm-mode sucks)
; $Id: memdisk.asm,v 1.22 2004/01/24 21:12:20 hpa Exp $
; ****************************************************************************
;
;  memdisk.asm
;
;  A program to emulate an INT 13h disk BIOS from a "disk" in extended
;  memory.
;
;   Copyright (C) 2001-2003  H. Peter Anvin
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
;  USA; either version 2 of the License, or (at your option) any later
;  version; incorporated herein by reference.
; 
; ****************************************************************************

%ifndef DEPEND
%include	"../version.gen"
%endif

; %define DEBUG_TRACERS			; Uncomment to get debugging tracers

%ifdef DEBUG_TRACERS

%macro TRACER	1
	call debug_tracer
	db %1
%endmacro

%else	; DEBUG_TRACERS

%macro	TRACER	1
%endmacro

%endif	; DEBUG_TRACERS

		org 0h

%define SECTORSIZE_LG2	9		; log2(sector size)
%define	SECTORSIZE	(1 << SECTORSIZE_LG2)

		; Parameter registers definition; this is the definition
		; of the stack frame.
%define		P_DS		word [bp+34]
%define		P_ES		word [bp+32]
%define		P_EAX		dword [bp+28]
%define		P_HAX		word [bp+30]
%define		P_AX		word [bp+28]
%define		P_AL		byte [bp+28]
%define		P_AH		byte [bp+29]
%define		P_ECX		dword [bp+24]
%define		P_HCX		word [bp+26]
%define		P_CX		word [bp+24]
%define		P_CL		byte [bp+24]
%define		P_CH		byte [bp+25]
%define		P_EDX		dword [bp+20]
%define		P_HDX		word [bp+22]
%define		P_DX		word [bp+20]
%define		P_DL		byte [bp+20]
%define		P_DH		byte [bp+21]
%define		P_EBX		dword [bp+16]
%define		P_HBX		word [bp+18]
%define		P_HBXL		byte [bp+18]
%define		P_BX		word [bp+16]
%define		P_BL		byte [bp+16]
%define		P_BH		byte [bp+17]
%define		P_EBP		dword [bp+8]
%define		P_BP		word [bp+8]
%define		P_ESI		dword [bp+4]
%define		P_SI		word [bp+4]
%define		P_EDI		dword [bp]
%define		P_DI		word [bp]

		section .text
		; These pointers are used by the installer and
		; must be first in the binary
Pointers:	dw Int13Start
		dw Int15Start
		dw PatchArea
		dw TotalSize

Int13Start:
		; Swap stack
		mov [cs:Stack],esp
		mov [cs:SavedAX],ax
		mov ax,ss
		mov [cs:Stack+4],ax
		mov ax,cs
		mov ss,ax
		mov sp,[cs:MyStack]

		; See if DL points to our class of device (FD, HD)
		push dx
		push dx
		xor dl,[cs:DriveNo]
		pop dx
		js .nomatch		; If SF=0, we have a class match here
		jz .our_drive		; If ZF=1, we have an exact match
		cmp dl,[cs:DriveNo]
		jb .nomatch		; Drive < Our drive
		dec dl			; Drive > Our drive, adjust drive #
.nomatch:
		mov ax,[cs:SavedAX]
		pushf
		call far [cs:OldInt13]
		pushf
		push bp
		mov bp,sp
		cmp byte [cs:SavedAX+1],08h
		je .norestoredl
		cmp byte [cs:SavedAX+1],15h
		jne .restoredl
		test byte [bp+4],80h	; Hard disk?
		jnz .norestoredl
.restoredl:
		mov dl,[bp+4]
.norestoredl:
		push ax
		push ebx
		push ds
		mov ax,[bp+2]		; Flags
		lds ebx,[cs:Stack]
		mov [bx+4],al		; Arithmetric flags
		pop ds
		pop ebx
		pop ax
		pop bp
		lss esp,[cs:Stack]
		iret

.our_drive:
		; Set up standard entry frame
		push ds
		push es
		mov ds,ax
		mov es,ax
		mov ax,[SavedAX]
		pushad
		mov bp,sp		; Point BP to the entry stack frame
		TRACER 'F'
		; Note: AH == P_AH here
		cmp ah,Int13FuncsMax
		jae Invalid_jump
		xor al,al		; AL = 0 is standard entry condition
		mov di,ax
		shr di,7		; Convert AH to an offset in DI
		call [Int13Funcs+di]

Done:		; Standard routine for return
		mov P_AX,ax
DoneWeird:
		TRACER 'D'
		xor bx,bx
		mov es,bx
		mov bx,[StatusPtr]
		mov [es:bx],ah		; Save status
		and ah,ah

		lds ebx,[Stack]
		; This sets the low byte (the arithmetric flags) of the
		; FLAGS on stack to either 00h (no flags) or 01h (CF)
		; depending on if AH was zero or not.
		setnz [bx+4]		; Set CF iff error
		popad
		pop es
		pop ds
		lss esp,[cs:Stack]
		iret

Reset:
		; Reset affects multiple drives, so we need to pass it on
		TRACER 'R'
		test dl,dl		; Always pass it on if we are resetting HD
		js .pass_on		; Bit 7 set
		; Some BIOSes get very unhappy if we pass a reset floppy
		; command to them and don't actually have any floppies.
		; This is a bug, but we have to deal with it nontheless.
		; Therefore, if we are the *ONLY* floppy drive, and the
		; user didn't request HD reset, then just drop the command.
		xor ax,ax		; Bottom of memory
		mov es,ax
		; BIOS equipment byte, top two bits + 1 == total # of floppies
		test byte [es:0x410],0C0h	
		jz success
		; ... otherwise pass it to the BIOS
.pass_on:
		pop ax			; Drop return address
		popad			; Restore all registers
		pop es
		pop ds
		lss esp,[cs:Stack]	; Restore the stack
		and dl,80h		; Clear all but the type bit
		jmp far [cs:OldInt13]


Invalid:
		pop dx			; Drop return address
Invalid_jump:
		TRACER 'I'
		mov ah,01h		; Unsupported function
		jmp short Done

GetDriveType:
		test byte [DriveNo],80h
		mov bl,02h		; Type 02h = floppy with changeline
		jz .floppy
		; Hard disks only...
		inc bx			; Type = 03h
		mov dx,[DiskSize]	; Return the disk size in sectors
		mov P_DX,dx
		mov cx,[DiskSize+2]
		mov P_CX,cx
.floppy:
		mov P_AH,bl		; 02h floppy, 03h hard disk
		pop ax			; Drop return address
		xor ax,ax		; Success...
		jmp short DoneWeird	; But don't stick it into P_AX

GetStatus:
		xor ax,ax
		mov es,ax
		mov bx,[StatusPtr]
		mov ah,[bx]		; Copy last status
		ret

Read:
		call setup_regs
do_copy:
		call bcopy
		movzx ax,P_AL		; AH = 0, AL = transfer count
		ret

Write:
		call setup_regs
		xchg esi,edi		; Opposite direction of a Read!
		jmp short do_copy

		; Verify integrity; just bounds-check
Verify:
		call setup_regs		; Returns error if appropriate
		; And fall through to success

CheckIfReady:				; These are always-successful noop functions
Recalibrate:
InitWithParms:
DetectChange:
Seek:
success:
		xor ax,ax		; Always successful
		ret

GetParms:
		TRACER 'G'
		mov dl,[DriveCnt]	; Cached data
		mov P_DL,dl
		test byte [DriveNo],80h
		jnz .hd
		mov P_DI,DPT
		mov P_ES,cs
		mov bl,[DriveType]
		mov P_BL,bl
.hd:
		mov ax,[Cylinders]
		dec ax			; We report the highest #, not the count
		xchg al,ah
		shl al,6
		or al,[Sectors]
		mov P_CX,ax
		mov ax,[Heads]
		dec ax
		mov P_DH,al

		;
		; Is this MEMDISK installation check?
		;
		cmp P_HAX,'ME'
		jne .notic
		cmp P_HCX,'MD'
		jne .notic
		cmp P_HDX,'IS'
		jne .notic
		cmp P_HBX,'K?'
		jne .notic

		; MEMDISK installation check...
		mov P_HAX,'!M'
		mov P_HCX,'EM'
		mov P_HDX,'DI'
		mov P_HBX,'SK'
		mov P_ES,cs
		mov P_DI,MemDisk_Info

.notic:
		xor ax,ax
		ret
		
		; Set up registers as for a "Read", and compares against disk size
setup_regs:

		; Convert a CHS address in P_CX/P_DH into an LBA in eax
		; CH = cyl[7:0]
		; CL[0:5] = sector (1-based)  CL[7:6] = cyl[9:8]
		; DH = head
		movzx ecx,P_CX
		movzx ebx,cl		; Sector number
		and bl,3Fh
		dec ebx			; Sector number is 1-based
		cmp bx,[Sectors]
		jae .overrun
		movzx edi,P_DH		; Head number
		movzx eax,word [Heads]
		cmp di,ax
		jae .overrun
		shr cl,6
		xchg cl,ch		; Now (E)CX <- cylinder number
		mul ecx			; eax <- Heads*cyl# (edx <- 0)
		add eax,edi
		mul dword [Sectors]
		add eax,ebx
		; Now eax = LBA, edx = 0

		;
		; setup_regs continues...
		;
		; Note: edi[31:16] and ecx[31:16] = 0 already
		mov di,P_BX		; Get linear address of target buffer
		mov cx,P_ES
		shl ecx,4
		add edi,ecx		; EDI = address to fetch to
		movzx ecx,P_AL		; Sector count
		mov esi,eax
		add eax,ecx		; LBA of final sector + 1
		shl esi,SECTORSIZE_LG2	; LBA -> byte offset
		add esi,[DiskBuf]	; Get address in high memory
		cmp eax,[DiskSize]	; Check the high mark against limit
		ja .overrun
		shl ecx,SECTORSIZE_LG2-1 ; Convert count to 16-bit words
		ret

.overrun:	pop ax			; Drop setup_regs return address
		mov ax,0200h		; Missing address mark
		ret			; Return to Done

int15_e820:
		cmp edx,534D4150h	; "SMAP"
		jne near oldint15
		cmp ecx,20		; Need 20 bytes
		jb err86
		push ds
		push cs
		pop ds
		and ebx,ebx
		jne .renew
		mov ebx,E820Table
.renew:
		add bx,12		; Advance to next
		mov eax,[bx-4]		; Type
		and eax,eax		; Null type?
		jz .renew		; If so advance to next
		mov [es:di+16],eax
		mov eax,[bx-12]		; Start addr (low)
		mov [es:di],eax
		mov ecx,[bx-8]		; Start addr (high)
		mov [es:di+4],ecx
		mov eax,[bx]		; End addr (low)
		mov ecx,[bx+4]		; End addr (high)
		sub eax,[bx-12]		; Derive the length
		sbb ecx,[bx-8]
		mov [es:di+8],eax	; Length (low)
		mov [es:di+12],ecx	; Length (high)
		cmp dword [bx+8],-1	; Type of next = end?
		jne .notdone
		xor ebx,ebx		; Done with table
.notdone:
		mov eax,edx		; "SMAP"
		pop ds
		mov ecx,20		; Bytes loaded
int15_success:
		mov byte [bp+6], 02h	; Clear CF
		pop bp
		iret

err86:
		mov byte [bp+6], 03h	; Set CF
		mov ah,86h
		pop bp
		iret

Int15Start:
		push bp
		mov bp,sp
		cmp ax,0E820h
		je near int15_e820
		cmp ax,0E801h
		je int15_e801
		cmp ax,0E881h
		je int15_e881
		cmp ah,88h
		je int15_88
oldint15:	pop bp
		jmp far [cs:OldInt15]
		
int15_e801:
		mov ax,[cs:Mem1MB]
		mov cx,ax
		mov bx,[cs:Mem16MB]
		mov dx,bx
		jmp short int15_success

int15_e881:
		mov eax,[cs:Mem1MB]
		mov ecx,eax
		mov ebx,[cs:Mem16MB]
		mov edx,ebx
		jmp short int15_success

int15_88:
		mov ax,[cs:MemInt1588]
		jmp short int15_success

;
; Routine to copy in/out of high memory
; esi = linear source address
; edi = linear target address
; ecx = 16-bit word count 
;
; Assumes cs = ds = es
;
bcopy:
		push eax
		push ebx
		push edx
		push ebp
.copy_loop:
		push esi
		push edi
		push ecx
		cmp ecx,8000h
		jna .safe_size
		mov ecx,8000h
.safe_size:
		push ecx	; Transfer size this cycle
		mov eax, esi
		mov [Mover_src1], si
		shr eax, 16
		mov [Mover_src1+2], al
		mov [Mover_src2], ah
		mov eax, edi
		mov [Mover_dst1], di
		shr eax, 16
		mov [Mover_dst1+2], al
		mov [Mover_dst2], ah
		mov si,Mover
		mov ah, 87h
		int 15h
		cli		; Some BIOSes enable interrupts on INT 15h
		pop eax		; Transfer size this cycle
		pop ecx
		pop edi
		pop esi
		jc .error
		lea esi,[esi+2*eax]
		lea edi,[edi+2*eax]
		sub ecx, eax
		jnz .copy_loop
		; CF = 0
.error:
		pop ebp
		pop edx
		pop ebx
		pop eax
		ret

%ifdef DEBUG_TRACERS
debug_tracer:	pushad
		pushfd
		mov bp,sp
		mov bx,[bp+9*4]
		mov al,[cs:bx]
		inc word [bp+9*4]
		mov ah,0Eh
		mov bx,7
		int 10h
		popfd
		popad
		ret
%endif

		section .data
		alignb 2
Int13Funcs	dw Reset		; 00h - RESET
		dw GetStatus		; 01h - GET STATUS
		dw Read			; 02h - READ
		dw Write		; 03h - WRITE
		dw Verify		; 04h - VERIFY
		dw Invalid		; 05h - FORMAT TRACK
		dw Invalid		; 06h - FORMAT TRACK AND SET BAD FLAGS
		dw Invalid		; 07h - FORMAT DRIVE AT TRACK
		dw GetParms		; 08h - GET PARAMETERS
		dw InitWithParms	; 09h - INITIALIZE CONTROLLER WITH DRIVE PARAMETERS
		dw Invalid		; 0Ah
		dw Invalid		; 0Bh
		dw Seek			; 0Ch - SEEK TO CYLINDER
		dw Reset		; 0Dh - RESET HARD DISKS
		dw Invalid		; 0Eh
		dw Invalid		; 0Fh
		dw CheckIfReady		; 10h - CHECK IF READY
		dw Recalibrate		; 11h - RECALIBRATE
		dw Invalid		; 12h
		dw Invalid		; 13h
		dw Invalid		; 14h
		dw GetDriveType		; 15h - GET DRIVE TYPE
		dw DetectChange		; 16h - DETECT DRIVE CHANGE
Int13FuncsEnd	equ $
Int13FuncsMax	equ (Int13FuncsEnd-Int13Funcs) >> 1

		alignb 8, db 0
Mover		dd 0, 0, 0, 0		; Must be zero
		dw 0ffffh		; 64 K segment size
Mover_src1:	db 0, 0, 0		; Low 24 bits of source addy
		db 93h			; Access rights
		db 00h			; Extended access rights
Mover_src2:	db 0			; High 8 bits of source addy
		dw 0ffffh		; 64 K segment size
Mover_dst1:	db 0, 0, 0		; Low 24 bits of target addy
		db 93h			; Access rights
		db 00h			; Extended access rights
Mover_dst2:	db 0			; High 8 bits of source addy
Mover_dummy2:	dd 0, 0, 0, 0		; More space for the BIOS

		alignb 4, db 0
MemDisk_Info	equ $			; Pointed to by installation check
MDI_Bytes	dw 26			; Total bytes in MDI structure
MDI_Version	db VER_MINOR, VER_MAJOR	; MEMDISK version

PatchArea	equ $			; This gets filled in by the installer

DiskBuf		dd 0			; Linear address of high memory disk
DiskSize	dd 0			; Size of disk in blocks
CommandLine	dw 0, 0			; Far pointer to saved command line

OldInt13	dd 0			; INT 13h in chain
OldInt15	dd 0			; INT 15h in chain

OldDosMem	dw 0			; Old position of DOS mem end
; ---- MDI structure ends here ---
MemInt1588	dw 0			; 1MB-65MB memory amount (1K)

Cylinders	dw 0			; Cylinder count
Heads		dw 0			; Head count
Sectors		dd 0			; Sector count (zero-extended)

Mem1MB		dd 0			; 1MB-16MB memory amount (1K)
Mem16MB		dd 0			; 16MB-4G memory amount (64K)

DriveNo		db 0			; Our drive number
DriveType	db 0			; Our drive type (floppies)
DriveCnt	db 0			; Drive count (from the BIOS)
		db 0			; Pad

MyStack		dw 0			; Offset of stack
StatusPtr	dw 0			; Where to save status (zeroseg ptr)

DPT		times 16 db 0		; BIOS parameter table pointer (floppies)

		; End patch area

Stack		dd 0			; Saved SS:ESP on invocation
		dw 0
SavedAX		dw 0			; AX saved on invocation

		alignb 4, db 0		; We *MUST* end on a dword boundary

E820Table	equ $			; The installer loads the E820 table here
TotalSize	equ $			; End pointer

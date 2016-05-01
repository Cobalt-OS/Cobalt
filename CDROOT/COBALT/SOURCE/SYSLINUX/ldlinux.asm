; -*- fundamental -*- (asm-mode sucks)
; $Id: ldlinux.asm,v 1.147 2004/06/13 20:50:44 hpa Exp $
; ****************************************************************************
;
;  ldlinux.asm
;
;  A program to boot Linux kernels off an MS-DOS formatted floppy disk.	 This
;  functionality is good to have for installation floppies, where it may
;  be hard to find a functional Linux system to run LILO off.
;
;  This program allows manipulation of the disk to take place entirely
;  from MS-LOSS, and can be especially useful in conjunction with the
;  umsdos filesystem.
;
;  This file is loaded in stages; first the boot sector at offset 7C00h,
;  then the first sector (cluster, really, but we can only assume 1 sector)
;  of LDLINUX.SYS at 7E00h and finally the remainder of LDLINUX.SYS at 8000h.
;
;   Copyright (C) 1994-2004  H. Peter Anvin
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
;  USA; either version 2 of the License, or (at your option) any later
;  version; incorporated herein by reference.
; 
; ****************************************************************************

%ifndef IS_MDSLINUX
%define IS_SYSLINUX 1
%endif
%include "macros.inc"
%include "config.inc"
%include "kernel.inc"
%include "bios.inc"
%include "tracers.inc"

;
; Some semi-configurable constants... change on your own risk.
;
my_id		equ syslinux_id
FILENAME_MAX_LG2 equ 4			; log2(Max filename size Including final null)
FILENAME_MAX	equ 11			; Max mangled filename size
NULLFILE	equ ' '			; First char space == null filename
retry_count	equ 6			; How patient are we with the disk?
%assign HIGHMEM_SLOP 0			; Avoid this much memory near the top

;
; This is what we need to do when idle
;
%macro	RESET_IDLE 0
	; Nothing
%endmacro
%macro	DO_IDLE 0
	; Nothing
%endmacro

;
; The following structure is used for "virtual kernels"; i.e. LILO-style
; option labels.  The options we permit here are `kernel' and `append
; Since there is no room in the bottom 64K for all of these, we
; stick them at vk_seg:0000 and copy them down before we need them.
;
; Note: this structure can be added to, but it must 
;
%define vk_power	7		; log2(max number of vkernels)
%define	max_vk		(1 << vk_power)	; Maximum number of vkernels
%define vk_shift	(16-vk_power)	; Number of bits to shift
%define vk_size		(1 << vk_shift)	; Size of a vkernel buffer

		struc vkernel
vk_vname:	resb FILENAME_MAX	; Virtual name **MUST BE FIRST!**
vk_rname:	resb FILENAME_MAX	; Real name
vk_appendlen:	resw 1
		alignb 4
vk_append:	resb max_cmd_len+1	; Command line
		alignb 4
vk_end:		equ $			; Should be <= vk_size
		endstruc

%ifndef DEPEND
%if (vk_end > vk_size) || (vk_size*max_vk > 65536)
%error "Too many vkernels defined, reduce vk_power"
%endif
%endif

;
; Segment assignments in the bottom 640K
; Stick to the low 512K in case we're using something like M-systems flash
; which load a driver into low RAM (evil!!)
;
; 0000h - main code/data segment (and BIOS segment)
;
real_mode_seg	equ 5000h
fat_seg		equ 3000h		; 128K area for FAT (2x64K)
vk_seg          equ 2000h		; Virtual kernels
xfer_buf_seg	equ 1000h		; Bounce buffer for I/O to high mem
comboot_seg	equ real_mode_seg	; COMBOOT image loading zone

; ---------------------------------------------------------------------------
;   BEGIN CODE
; ---------------------------------------------------------------------------

;
; Memory below this point is reserved for the BIOS and the MBR
;
 		absolute 1000h
trackbuf	equ $			; Track buffer goes here
trackbufsize	equ 16384		; Safe size of track buffer
;		trackbuf ends at 5000h


;
; Constants for the xfer_buf_seg
;
; The xfer_buf_seg is also used to store message file buffers.  We
; need two trackbuffers (text and graphics), plus a work buffer
; for the graphics decompressor.
;
xbs_textbuf	equ 0			; Also hard-coded, do not change
xbs_vgabuf	equ trackbufsize
xbs_vgatmpbuf	equ 2*trackbufsize


                absolute 5000h          ; Here we keep our BSS stuff
VKernelBuf:	resb vk_size		; "Current" vkernel
		alignb 4
AppendBuf       resb max_cmd_len+1	; append=
Ontimeout	resb max_cmd_len+1	; ontimeout
Onerror		resb max_cmd_len+1	; onerror
KbdMap		resb 256		; Keyboard map
FKeyName	resb 10*16		; File names for F-key help
NumBuf		resb 15			; Buffer to load number
NumBufEnd	resb 1			; Last byte in NumBuf
		alignb 8

		; Expanded superblock
SuperInfo	equ $
		resq 16			; The first 16 bytes expanded 8 times
		;
		; These need to follow SuperInfo
		;
RootDir		resd 1			; Location of root directory
DataArea	resd 1			; Location of data area
RootDirSize	resw 1			; Root dir size in sectors
DirScanCtr	resw 1			; Used while searching directory
EndofDirSec	resw 1			; = trackbuf+bsBytesPerSec-31

		alignb 4
E820Buf		resd 5			; INT 15:E820 data buffer
E820Mem		resd 1			; Memory detected by E820
E820Max		resd 1			; Is E820 memory capped?
HiLoadAddr      resd 1			; Address pointer for high load loop
HighMemSize	resd 1			; End of memory pointer (bytes)
RamdiskMax	resd 1			; Highest address for a ramdisk
KernelSize	resd 1			; Size of kernel (bytes)
SavedSSSP	resd 1			; Our SS:SP while running a COMBOOT image
PMESP		resd 1			; Protected-mode ESP
ClustPerMoby	resd 1			; Clusters per 64K
ClustSize	resd 1			; Bytes/cluster
KernelName      resb 12		        ; Mangled name for kernel
					; (note the spare byte after!)
OrigKernelExt	resd 1			; Original kernel extension
FBytes		equ $			; Used by open/getc
FBytes1		resw 1
FBytes2		resw 1
DirBlocksLeft	resw 1			; Ditto
RunLinClust	resw 1			; Cluster # for LDLINUX.SYS
BufSafe		resw 1			; Clusters we can load into trackbuf
BufSafeSec	resw 1			; = how many sectors?
BufSafeBytes	resw 1			; = how many bytes?
EndOfGetCBuf	resw 1			; = getcbuf+BufSafeBytes
KernelClust	resw 1			; Kernel size in clusters
FClust		resw 1			; Number of clusters in open/getc file
FNextClust	resw 1			; Pointer to next cluster in d:o
FPtr		resw 1			; Pointer to next char in buffer
CmdOptPtr       resw 1			; Pointer to first option on cmd line
KernelCNameLen  resw 1			; Length of unmangled kernel name
InitRDCNameLen  resw 1			; Length of unmangled initrd name
NextCharJump    resw 1			; Routine to interpret next print char
SetupSecs	resw 1			; Number of setup sectors
A20Test		resw 1			; Counter for testing status of A20
A20Type		resw 1			; A20 type
CmdLineLen	resw 1			; Length of command line including null
GraphXSize	resw 1			; Width of splash screen file
VGAPos		resw 1			; Pointer into VGA memory
VGACluster	resw 1			; Cluster pointer for VGA image file
VGAFilePtr	resw 1			; Pointer into VGAFileBuf
Com32SysSP	resw 1			; SP saved during COM32 syscall
CursorDX        equ $
CursorCol       resb 1			; Cursor column for message file
CursorRow       resb 1			; Cursor row for message file
ScreenSize      equ $
VidCols         resb 1			; Columns on screen-1
VidRows         resb 1			; Rows on screen-1
BaudDivisor	resw 1			; Baud rate divisor
FlowControl	equ $
FlowOutput	resb 1			; Outputs to assert for serial flow
FlowInput	resb 1			; Input bits for serial flow
FlowIgnore	resb 1			; Ignore input unless these bits set
TextAttribute   resb 1			; Text attribute for message file
RetryCount      resb 1			; Used for disk access retries
KbdFlags	resb 1			; Check for keyboard escapes
LoadFlags	resb 1			; Loadflags from kernel
A20Tries	resb 1			; Times until giving up on A20
FuncFlag	resb 1			; Escape sequences received from keyboard
DisplayMask	resb 1			; Display modes mask
CopySuper	resb 1			; Distinguish .bs versus .bss
MNameBuf        resb 11            	; Generic mangled file name buffer
InitRD          resb 11                 ; initrd= mangled name
KernelCName     resb 13                 ; Unmangled kernel name
InitRDCName     resb 13            	; Unmangled initrd name
TextColorReg	resb 17			; VGA color registers for text mode
VGAFileBuf	resb 13			; Unmangled VGA image name
VGAFileBufEnd	equ $
VGAFileMBuf	resb 11			; Mangled VGA image name
                alignb 4		; For the good of REP MOVSD
command_line	resb max_cmd_len+2	; Command line buffer
default_cmd	resb max_cmd_len+1	; "default" command line
kern_cmd_len	equ $-command_line

		section .text
                org 7C00h
;
; Some of the things that have to be saved very early are saved
; "close" to the initial stack pointer offset, in order to
; reduce the code size...
;
StackBuf	equ $-44-32		; Start the stack here (grow down - 4K)
PartInfo	equ StackBuf		; Saved partition table entry
FloppyTable	equ PartInfo+16		; Floppy info table (must follow PartInfo)
OrigFDCTabPtr	equ StackBuf-4		; The high dword on the stack

;
; Primary entry point.  Tempting as though it may be, we can't put the
; initial "cli" here; the jmp opcode in the first byte is part of the
; "magic number" (using the term very loosely) for the DOS superblock.
;
bootsec		equ $
		jmp short start		; 2 bytes
		nop			; 1 byte
;
; "Superblock" follows -- it's in the boot sector, so it's already
; loaded and ready for us
;
bsOemName	db 'SYSLINUX'		; The SYS command sets this, so...
;
; These are the fields we actually care about.  We end up expanding them
; all to dword size early in the code, so generate labels for both
; the expanded and unexpanded versions.
;;
%macro		superb 1
bx %+ %1	equ SuperInfo+($-superblock)*8+4
bs %+ %1	equ $
		zb 1
%endmacro
%macro		superw 1
bx %+ %1	equ SuperInfo+($-superblock)*8
bs %+ %1	equ $
		zw 1
%endmacro
%macro		superd 1
bx %+ %1	equ $			; no expansion for dwords
bs %+ %1	equ $
		zd 1
%endmacro
superblock	equ $
		superw BytesPerSec
		superb SecPerClust
		superw ResSectors
		superb FATs
		superw RootDirEnts
		superw Sectors
		superb Media
		superw FATsecs
		superw SecPerTrack
		superw Heads
superinfo_size	equ ($-superblock)-1	; How much to expand
		superd Hidden
		superd HugeSectors
		superb DriveNumber
		superb Reserved1
		superb BootSignature	; 29h if the following fields exist
		superd VolumeID
bsVolumeLabel	zb 11
bsFileSysType	zb 8			; Must be FAT12 or FAT16 for this version
superblock_len	equ $-superblock

SecPerClust	equ bxSecPerClust
;
; Note we don't check the constraints above now; we did that at install
; time (we hope!)
;

;floppy_table	equ $			; No sense in wasting memory, overwrite start

start:
		cli			; No interrupts yet, please
		cld			; Copy upwards
;
; Set up the stack
;
		xor ax,ax
		mov ss,ax
		mov sp,StackBuf		; Just below BSS
		mov es,ax
;
; DS:SI may contain a partition table entry.  Preserve it for us.
;
		mov cx,8		; Save partition info
		mov di,sp
		rep movsw

		mov ds,ax		; Now we can initialize DS...

		mov [di+bsDriveNumber-FloppyTable],dl
;
; Now sautee the BIOS floppy info block to that it will support decent-
; size transfers; the floppy block is 11 bytes and is stored in the
; INT 1Eh vector (brilliant waste of resources, eh?)
;
; Of course, if BIOSes had been properly programmed, we wouldn't have
; had to waste precious space with this code.
;
		mov bx,fdctab
		lfs si,[bx]		; FS:SI -> original fdctab
		push fs			; Save on stack in case we need to bail
		push si

		; Save the old fdctab even if hard disk so the stack layout
		; is the same.  The instructions above do not change the flags
		and dl,dl		; If floppy disk (00-7F), assume no
					; partition table
		js harddisk

floppy:
		mov cl,6		; 12 bytes (CX == 0)
		; es:di -> FloppyTable already
		; This should be safe to do now, interrupts are off...
		mov [bx],di		; FloppyTable
		mov [bx+2],ax		; Segment 0
		fs rep movsw		; Faster to move words
		mov cl,[bsSecPerTrack]  ; Patch the sector count
		mov [di-8],cl
		; AX == 0 here
		int 13h			; Some BIOSes need this

		jmp short not_harddisk
;
; The drive number and possibly partition information was passed to us
; by the BIOS or previous boot loader (MBR).  Current "best practice" is to
; trust that rather than what the superblock contains.
;
; Would it be better to zero out bsHidden if we don't have a partition table?
;
; Note: di points to beyond the end of PartInfo
;
harddisk:
		test byte [di-16],7Fh	; Sanity check: "active flag" should
		jnz no_partition	; be 00 or 80
		mov eax,[di-8]		; Partition offset (dword)
		mov [bsHidden],eax
no_partition:
;
; Get disk drive parameters (don't trust the superblock.)  Don't do this for
; floppy drives -- INT 13:08 on floppy drives will (may?) return info about
; what the *drive* supports, not about the *media*.  Fortunately floppy disks
; tend to have a fixed, well-defined geometry which is stored in the superblock.
;
		; DL == drive # still
		mov ah,08h
		int 13h
		jc no_driveparm
		and ah,ah
		jnz no_driveparm
		shr dx,8
		inc dx			; Contains # of heads - 1
		mov [bsHeads],dx
		and cx,3fh
		mov [bsSecPerTrack],cx
no_driveparm:
not_harddisk:
;
; Ready to enable interrupts, captain
;
		sti
;
; Insane hack to expand the superblock to dwords
;
expand_super:
		xor eax,eax
		mov es,ax			; INT 13:08 destroys ES
		mov si,superblock
		mov di,SuperInfo
		mov cl,superinfo_size		; CH == 0
.loop:
		lodsw
		dec si
		stosd				; Store expanded word
		xor ah,ah
		stosd				; Store expanded byte
		loop .loop

;
; Now we have to do some arithmetric to figure out where things are located.
; If Micro$oft had had brains they would already have done this for us,
; and stored it in the superblock at format time, but here we go,
; wasting precious boot sector space again...
;
%define		Z di-superinfo_size*8-SuperInfo
debugentrypt:
		mov ax,[bxFATs]		; Number of FATs (eax<31:16> == 0)
		mov edx,[Z+bxFATsecs]	; Sectors/FAT
		mul edx			; Get the size of the FAT area
		; edx <- 0
		add eax,[bxHidden]		; Add hidden sectors
		add eax,[Z+bxResSectors]	; And reserved sectors

		mov [RootDir],eax	; Location of root directory
		mov [DataArea],eax	; First data sector
		push eax

		mov eax,[Z+bxRootDirEnts]
		shl ax,5		; Size of a directory entry
		mov bx,[Z+bxBytesPerSec]
		add ax,bx		; Round up, not down
		dec ax
		div bx			; Now we have the size of the root dir
		mov [RootDirSize],ax
		mov [DirScanCtr],ax
		add bx,trackbuf-31
		mov [Z+EndofDirSec],bx	; End of a single directory sector
		add [Z+DataArea],eax
		pop eax			; Reload root directory starting point

;
; Now the fun begins.  We have to search the root directory for
; LDLINUX.SYS and load the first sector, so we have a little more
; space to have fun with.  Then we can go chasing through the FAT.
; Joy!!
;
sd_nextsec:	push eax
		mov bx,trackbuf
		push bx
		call getonesec
		pop si
sd_nextentry:	mov cx,11
		cmp [si],ch		; Directory high water mark
		je kaboom
; This no longer fits... since we'd be dead anyway if there
; was a nonfile named LDLINUX.SYS on the disk, it shouldn't
; matter...
;		test byte [si+11],18h	; Must be a file
;		jnz sd_not_file
		mov di,ldlinux_name
		push si
		repe cmpsb
		pop si
		je found_it
sd_not_file:	add si,byte 32		; Distance to next
		cmp si,[EndofDirSec]
		jb sd_nextentry
		pop eax
		inc eax
		dec word [DirScanCtr]
		jnz sd_nextsec
;
; kaboom: write a message and bail out.
;
kaboom:
		xor si,si
		mov ss,si		
		mov sp,StackBuf-4 	; Reset stack
		mov ds,si		; Reset data segment
		pop dword [fdctab]	; Restore FDC table
.patch:		mov si,bailmsg
		call writestr		; Returns with AL = 0
		cbw			; AH <- 0
		int 16h			; Wait for keypress
		int 19h			; And try once more to boot...
.norge:		jmp short .norge	; If int 19h returned; this is the end

;
; found_it: now we compute the location of the first sector, then
;	    load it and JUMP (since we're almost out of space)
;
found_it:	; Note: we actually leave two words on the stack here
		; (who cares?)
		mov eax,[bxSecPerClust]
		mov bp,ax		; Load an entire cluster
		movzx ebx,word [si+26]	; First cluster
		mov [RunLinClust],bx	; Save for later use
		dec bx			; First cluster is "cluster 2"
		dec bx
		mul ebx
		add eax,[DataArea]
		mov bx,ldlinux_sys
		call getlinsec
		mov si,bs_magic
		mov di,ldlinux_magic
		mov cx,magic_len
		repe cmpsb		; Make sure that the bootsector
		jne kaboom		; matches LDLINUX.SYS
;
; Done! Jump to the entry point!
;
		jmp ldlinux_ent
;
;
; writestr: write a null-terminated string to the console
;	    This assumes we're on page 0.  This is only used for early
;           messages, so it should be OK.
;
writestr:
.loop:		lodsb
		and al,al
                jz .return
		mov ah,0Eh		; Write to screen as TTY
		mov bx,0007h		; Attribute
		int 10h
		jmp short .loop
.return:	ret

;
; disk_error: decrement the retry count and bail if zero.
;	      This gets patched once we have more space to try to
;             optimize transfer sizes on broken machines.
;
disk_error:	dec si			; SI holds the disk retry counter
		jz kaboom
		; End of patched "call" instruction!
		jmp short disk_try_again

;
; getonesec: like getlinsec, but pre-sets the count to 1
;
getonesec:
		mov bp,1
		; Fall through to getlinsec

;
; getlinsec: load a sequence of BP floppy sector given by the linear sector
;	     number in EAX into the buffer at ES:BX.  We try to optimize
;	     by loading up to a whole track at a time, but the user
;	     is responsible for not crossing a 64K boundary.
;	     (Yes, BP is weird for a count, but it was available...)
;
;	     On return, BX points to the first byte after the transferred
;	     block.
;
;	     The "stupid patch area" gets replaced by the code
;	     mov bp,1 ; nop ... (BD 01 00 90 90...) when installing with
;	     the -s option.
;
;            This routine assumes CS == DS.
;
; Stylistic note: use "xchg" instead of "mov" when the source is a register
; that is dead from that point; this saves space.  However, please keep
; the order to dst,src to keep things sane.
;
getlinsec:
		mov esi,[bxSecPerTrack]
		;
		; Dividing by sectors to get (track,sector): we may have
		; up to 2^18 tracks, so we need to use 32-bit arithmetric.
		;
		xor edx,edx		; Zero-extend LBA to 64 bits
		div esi
		xor cx,cx
		xchg cx,dx		; CX <- sector index (0-based)
					; EDX <- 0
		; eax = track #
		div dword [bxHeads]	; Convert track to head/cyl
		;
		; Now we have AX = cyl, DX = head, CX = sector (0-based),
		; BP = sectors to transfer, SI = bsSecPerTrack,
		; ES:BX = data target
		;
gls_nextchunk:	push si			; <A> bsSecPerTrack
		push bp			; <B> Sectors to transfer

		; Important - this gets patched with a call.  The call
		; assumes cx, si and bp are set up, and can modify bp
		; and destroy si.  Until we have the space to do so,
		; transfer one sector at a time.
gls_set_size:
__BEGIN_STUPID_PATCH_AREA:
		mov bp,1		; 3 bytes, same as a call insn
__END_STUPID_PATCH_AREA:

		push ax			; <C> Cylinder #
		push dx			; <D> Head #

		push cx			; <E> Sector #
		shl ah,6		; Because IBM was STOOPID
					; and thought 8 bits were enough
					; then thought 10 bits were enough...
		pop cx			; <E> Sector #
		push cx			; <E> Sector #
		inc cx			; Sector numbers are 1-based, sigh
		or cl,ah
		mov ch,al
		mov dh,dl
		mov dl,[bsDriveNumber]
		xchg ax,bp		; Sector to transfer count
					; (xchg shorter than mov)
		mov si,retry_count	; # of times to retry a disk access
;
; Do the disk transfer... save the registers in case we fail :(
;
disk_try_again: 
		pusha			; <F>
		mov ah,02h		; READ DISK
		int 13h
		popa			; <F>
		jc disk_error
;
; Disk access successful
;
		pop cx			; <E> Sector #
		mov di,ax		; Reduce sector left count
		mul word [bsBytesPerSec] ; Figure out how much to advance ptr
		add bx,ax		; Update buffer location
		pop dx			; <D> Head #
		pop ax			; <C> Cyl #
		pop bp			; <B> Sectors left to transfer
		pop si			; <A> Number of sectors/track
		sub bp,di		; Reduce with # of sectors just read
		jz writestr.return	; Done!
		add cx,di
		cmp cx,si
		jb gls_nextchunk
		inc dx			; Next track on cyl
		cmp dx,[bsHeads]	; Was this the last one?
		jb gls_nonewcyl
		inc ax			; If so, new cylinder
		xor dx,dx		; First head on new cylinder
gls_nonewcyl:	sub cx,si		; First sector on new track
		jmp short gls_nextchunk

bailmsg:	db 'Boot failed', 0Dh, 0Ah, 0

bs_checkpt	equ $			; Must be <= 7DEFh

%if 1
bs_checkpt_off	equ ($-$$)
%ifndef DEPEND
%if bs_checkpt_off > 1EFh
%error "Boot sector overflow"
%endif
%endif

		zb 1EFh-($-$$)
%endif
bs_magic	equ $			; From here to the magic_len equ
					; must match ldlinux_magic
ldlinux_name:	db 'LDLINUX SYS'	; Looks like this in the root dir
		dd HEXDATE		; Hopefully unique between compiles

bootsignature	dw 0AA55h
magic_len	equ $-bs_magic

;
; ===========================================================================
;  End of boot sector
; ===========================================================================
;  Start of LDLINUX.SYS
; ===========================================================================

ldlinux_sys:

syslinux_banner	db 0Dh, 0Ah
%if IS_MDSLINUX
		db 'MDSLINUX '
%else
		db 'SYSLINUX '
%endif
		db version_str, ' ', date, ' ', 0
		db 0Dh, 0Ah, 1Ah	; EOF if we "type" this in DOS

ldlinux_magic	db 'LDLINUX SYS'
		dd HEXDATE
		dw 0AA55h

;
; This area is possibly patched by the installer.  It is located
; immediately after the EOF + LDLINUX SYS + 4 bytes + 55 AA + alignment,
; so we can find it algorithmically.
;
		alignb 4, db 0
MaxTransfer	dw 00FFh		; Absolutely maximum transfer size

		align 4
ldlinux_ent:
; 
; Note that some BIOSes are buggy and run the boot sector at 07C0:0000
; instead of 0000:7C00 and the like.  We don't want to add anything
; more to the boot sector, so it is written to not assume a fixed
; value in CS, but we don't want to deal with that anymore from now
; on.
;
		jmp 0:.next
.next:

;
; Tell the user we got this far
;
		mov si,syslinux_banner
		call writestr
;
; Remember, the boot sector loaded only the first cluster of LDLINUX.SYS.
; We can really only rely on a single sector having been loaded.  Hence
; we should load the FAT into RAM and start chasing pointers...
;
		xor ax,ax
		cwd
		inc dx				; DX:AX <- 64K
		div word [bxBytesPerSec]	; sectors/64K
		mov si,ax

		push es
		mov bx,fat_seg			; Load into fat_seg:0000
		mov es,bx
		
		mov eax,[bsHidden]		; Hidden sectors
		add edx,[bxResSectors]
		add eax,edx
		mov ecx,[bxFATsecs]		; Sectors/FAT
fat_load_loop:	
		mov ebp,ecx			; Make sure high EBP = 0
		cmp bp,si
		jna fat_load
		mov bp,si			; A full 64K moby
fat_load:	
		xor bx,bx			; Offset 0 in the current ES
		call getlinsecsr
		sub cx,bp
		jz fat_load_done		; Last moby?
		add eax,ebp			; Advance sector count
		mov bx,es			; Next 64K moby
		add bx,1000h
		mov es,bx
		jmp short fat_load_loop
fat_load_done:
		pop es
;
; Fine, now we have the FAT in memory.	How big is a cluster, really?
; Also figure out how many clusters will fit in an 8K buffer, and how
; many sectors and bytes that is
;
		mov edi,[bxBytesPerSec]		; Used a lot below
		mov eax,[SecPerClust]
		mov si,ax			; Also used a lot
		mul di
		mov [ClustSize],eax		; Bytes/cluster
		mov bx,ax
		mov ax,trackbufsize		; High bit 0
		cwd
		div bx
		mov [BufSafe],ax		; # of cluster in trackbuf
		mul si
		mov [BufSafeSec],ax
		mul di
		mov [BufSafeBytes],ax
		add ax,getcbuf			; Size of getcbuf is the same
		mov [EndOfGetCBuf],ax		; as for trackbuf
;
; FAT12 or FAT16?  This computation is fscking ridiculous...
;
		mov eax,[bxSectors]
		and ax,ax
		jnz have_secs
		mov eax,[bsHugeSectors]
have_secs:	add eax,[bsHidden]		; These are not included
		sub eax,[RootDir]		; Start of root directory
		movzx ebx,word [RootDirSize]
		sub eax,ebx			; Subtract root directory size
		xor edx,edx
		div esi				; Convert to clusters
		cmp ax,4086			; FAT12 limit
		jna is_fat12
		; Patch the jump
		mov byte [nextcluster+1],nextcluster_fat16-(nextcluster+2)
is_fat12:

;
; Patch gls_set_size so we can transfer more than one sector at a time.
;
		mov byte [gls_set_size],0xe8	; E8 = CALL NEAR
		mov word [gls_set_size+1],do_gls_set_size-(gls_set_size+3)
		mov byte [disk_error],0xe8
		mov word [disk_error+1],do_disk_error-(disk_error+3)

;
; Now we read the rest of LDLINUX.SYS.	Don't bother loading the first
; cluster again, though.
;
load_rest:
		mov cx,[ClustSize]
		mov bx,ldlinux_sys
		add bx,cx
		mov si,[RunLinClust]
		call nextcluster
		xor dx,dx
		mov ax,ldlinux_len-1		; To be on the safe side
		add ax,cx
		div cx				; the number of clusters
		dec ax				; We've already read one
		jz all_read_jmp
		mov cx,ax
		call getfssec
;
; All loaded up
;
all_read_jmp:
		jmp all_read
;
; -----------------------------------------------------------------------------
; Subroutines that have to be in the first sector
; -----------------------------------------------------------------------------
;
; getfssec: Get multiple clusters from a file, given the starting cluster.
;
;	This routine makes sure the subtransfers do not cross a 64K boundary,
;	and will correct the situation if it does, UNLESS *sectors* cross
;	64K boundaries.
;
;	ES:BX	-> Buffer
;	SI	-> Starting cluster number (2-based)
;	CX	-> Cluster count (0FFFFh = until end of file)
;
;	Returns CF=1 on EOF
;
getfssec:
.getfragment:	xor ebp,ebp			; Fragment sector count
		lea eax,[si-2]			; Get 0-based sector address
		mul dword [SecPerClust]
		add eax,[DataArea]
.getseccnt:					; See if we can read > 1 clust
		add bp,[SecPerClust]
		dec cx				; Reduce clusters left to find
		lea di,[si+1]
		call nextcluster
		cmc
		jc .eof				; At EOF?
		jcxz .endfragment		; Or was it the last we wanted?
		cmp si,di			; Is file continuous?
		je .getseccnt			; Yes, we can get
.endfragment:	clc				; Not at EOF
.eof:		pushf				; Remember EOF or not
		push si
		push cx
.getchunk:
		push eax
		mov ax,es			; Check for 64K boundaries.
		shl ax,4
		add ax,bx
		xor dx,dx
		neg ax
		setz dl				; DX <- 1 if full 64K segment
		div word [bsBytesPerSec]	; How many sectors fit?
		mov si,bp
		sub si,ax			; Compute remaining sectors
		jbe .lastchunk
		mov bp,ax
		pop eax
		call getlinsecsr
		add eax,ebp			; EBP<31:16> == 0
		mov bp,si			; Remaining sector count
		jmp short .getchunk
.lastchunk:	pop eax
		call getlinsec
		pop cx
		pop si
		popf
		jcxz .return			; If we hit the count limit
		jnc .getfragment		; If we didn't hit EOF
.return:	ret

;
; getlinsecsr: save registers, call getlinsec, restore registers
;
getlinsecsr:	pushad
		call getlinsec
		popad
		ret

;
; nextcluster: Advance a cluster pointer in SI to the next cluster
;	       pointed at in the FAT tables.  CF=0 on return if end of file.
;
nextcluster:
		jmp short nextcluster_fat12	; This gets patched

nextcluster_fat12:
		push bx
		push ds
		mov bx,fat_seg
		mov ds,bx
		mov bx,si			; Multiply by 3/2
		shr bx,1			; CF now set if odd
		mov si,[si+bx]
		jnc nc_even
		shr si,4			; Needed for odd only
nc_even:
		and si,0FFFh
		cmp si,0FF0h			; Clears CF if at end of file
		pop ds
		pop bx
nc_return:	ret

;
; FAT16 decoding routine.  Note that a 16-bit FAT can be up to 128K,
; so we have to decide if we're in the "low" or the "high" 64K-segment...
;
nextcluster_fat16:
		push ax
		push ds
		mov ax,fat_seg
		shl si,1
		jnc .seg0
		mov ax,fat_seg+1000h
.seg0:		mov ds,ax
		mov si,[si]
		cmp si,0FFF0h
		pop ds
		pop ax
		ret

;
; Routine that controls how much we can transfer in one chunk.  Called
; from gls_set_size in getlinsec.
;
do_gls_set_size:
		sub si,cx		; Sectors left on track
		cmp bp,si
		jna .lastchunk
		mov bp,si		; No more than a trackful, please!
.lastchunk:
		cmp bp,[MaxTransfer]	; Absolute maximum transfer size
		jna .oktransfer
		mov bp,[MaxTransfer]
.oktransfer:	
		ret

;
; This routine captures disk errors, and tries to decide if it is
; time to reduce the transfer size.
;
do_disk_error:
		dec si			; Decrement the retry counter
		jz kaboom		; If expired, croak
		cmp si,2		; If only 2 attempts left
		ja .nodanger
		mov al,1		; Drop transfer size to 1
		jmp short .setsize
.nodanger:
		cmp si,retry_count-2
		ja .again		; First time, just try again
		shr al,1		; Otherwise, try to reduce
		adc al,0		; the max transfer size, but not to 0
.setsize:
		mov [MaxTransfer],al
.again:
		ret

;
; Debug routine
;
%ifdef debug
safedumpregs:
		cmp word [Debug_Magic],0D00Dh
		jnz nc_return
		jmp dumpregs
%endif

rl_checkpt	equ $				; Must be <= 8000h

rl_checkpt_off	equ ($-$$)
%ifndef DEPEND
%if rl_checkpt_off > 400h
%error "Sector 1 overflow"
%endif
%endif

; ----------------------------------------------------------------------------
;  End of code and data that have to be in the first sector
; ----------------------------------------------------------------------------

all_read:
;
; Let the user (and programmer!) know we got this far.  This used to be
; in Sector 1, but makes a lot more sense here.
;
		mov si,copyright_str
		call writestr

;
; Common initialization code
;
%include "cpuinit.inc"

;
; Initialization that does not need to go into the any of the pre-load
; areas
;
		; Now set up screen parameters
		call adjust_screen

		; Wipe the F-key area
		mov al,NULLFILE
		mov di,FKeyName
		mov cx,10*(1 << FILENAME_MAX_LG2)
		rep stosb

;
; Now, everything is "up and running"... patch kaboom for more
; verbosity and using the full screen system
;
		; E9 = JMP NEAR
		mov dword [kaboom.patch],0e9h+((kaboom2-(kaboom.patch+3)) << 8)

;
; Compute some parameters that depend on cluster size
;
		xor eax,eax
		cwd				; DX <- 0
		inc dx				; DX:AX <- 64K
		div word [ClustSize]
		mov [ClustPerMoby],eax		; Clusters/64K

;
; Now we're all set to start with our *real* business.	First load the
; configuration file (if any) and parse it.
;
; In previous versions I avoided using 32-bit registers because of a
; rumour some BIOSes clobbered the upper half of 32-bit registers at
; random.  I figure, though, that if there are any of those still left
; they probably won't be trying to install Linux on them...
;
; The code is still ripe with 16-bitisms, though.  Not worth the hassle
; to take'm out.  In fact, we may want to put them back if we're going
; to boot ELKS at some point.
;
		mov si,linuxauto_cmd		; Default command: "linux auto"
		mov di,default_cmd
                mov cx,linuxauto_len
		rep movsb

		mov di,KbdMap			; Default keymap 1:1
		xor al,al
		inc ch				; CX <- 256
mkkeymap:	stosb
		inc al
		loop mkkeymap

;
; Load configuration file
;
		mov di,syslinux_cfg
		call open
		jz no_config_file

;
; Now we have the config file open.  Parse the config file and
; run the user interface.
;
%include "ui.inc"

;
; Linux kernel loading code is common.
;
%include "runkernel.inc"

;
; COMBOOT-loading code
;
%include "comboot.inc"
%include "com32.inc"
%include "cmdline.inc"

;
; Boot sector loading code
;
%include "bootsect.inc"

;
; abort_check: let the user abort with <ESC> or <Ctrl-C>
;
abort_check:
		call pollchar
		jz ac_ret1
		pusha
		call getchar
		cmp al,27			; <ESC>
		je ac_kill
		cmp al,3			; <Ctrl-C>
		jne ac_ret2
ac_kill:	mov si,aborted_msg

;
; abort_load: Called by various routines which wants to print a fatal
;             error message and return to the command prompt.  Since this
;             may happen at just about any stage of the boot process, assume
;             our state is messed up, and just reset the segment registers
;             and the stack forcibly.
;
;             SI    = offset (in _text) of error message to print
;
abort_load:
                mov ax,cs                       ; Restore CS = DS = ES
                mov ds,ax
                mov es,ax
                cli
                mov sp,StackBuf-2*3    		; Reset stack
                mov ss,ax                       ; Just in case...
                sti
                call cwritestr                  ; Expects SI -> error msg
al_ok:          jmp enter_command               ; Return to command prompt
;
; End of abort_check
;
ac_ret2:	popa
ac_ret1:	ret

;
; searchdir: Search the root directory for a pre-mangled filename in
;	     DS:DI.  This routine is similar to the one in the boot
;	     sector, but is a little less Draconian when it comes to
;	     error handling, plus it reads the root directory in
;	     larger chunks than a sector at a time (which is probably
;	     a waste of coding effort, but I like to do things right).
;
;	     FIXME: usually we can load the entire root dir in memory,
;	     and files are usually at the beginning anyway.  It probably
;	     would be worthwhile to remember if we have the first chunk
;	     in memory and skip the load if that (it would speed up online
;	     help, mainly.)
;
;	     NOTE: This file considers finding a zero-length file an
;	     error.  This is so we don't have to deal with that special
;	     case elsewhere in the program (most loops have the test
;	     at the end).
;
;	     If successful:
;		ZF clear
;		SI	= cluster # for the first cluster
;		DX:AX	= file length in bytes
;	     If unsuccessful
;		ZF set
;

searchdir:
		push bp
		mov ax,[bsRootDirEnts]
		mov [DirScanCtr],ax
		mov ax,[RootDirSize]
		mov [DirBlocksLeft],ax
		mov eax,[RootDir]
scan_group:
		movzx ebp,word [DirBlocksLeft]
		and bp,bp
		jz dir_return
		cmp bp,[BufSafeSec]
		jna load_last
		mov bp,[BufSafeSec]
load_last:
		sub [DirBlocksLeft],bp
		push eax
		mov ax,[bsBytesPerSec]
		mul bp
		add ax,trackbuf-31
		mov [EndofDirSec],ax	; End of loaded
		pop eax
		mov bx,trackbuf
		call getlinsecsr
		mov si,trackbuf
dir_test_name:	cmp byte [si],0		; Directory high water mark
		je dir_return		; Failed
                test byte [si+11],18h	; Check it really is a file
                jnz dir_not_this
		push di
		push si
		mov cx,11		; Filename = 11 bytes
		repe cmpsb
		pop si
		pop di
		je dir_success
dir_not_this:   add si,byte 32
		dec word [DirScanCtr]
		jz dir_return		; Out of it...
		cmp si,[EndofDirSec]
		jb dir_test_name
		add eax,ebp		; Increment linear sector number
		jmp short scan_group
dir_success:
		mov ax,[si+28]		; Length of file
		mov dx,[si+30]
		mov si,[si+26]		; Cluster pointer
		mov bx,ax
		or bx,dx		; Sets ZF iff DX:AX is zero
dir_return:
		pop bp
		ret

;
; writechr:	Write a single character in AL to the console without
;		mangling any registers; handle video pages correctly.
;
writechr:
		call write_serial	; write to serial port if needed
		pushfd
		pushad
		mov ah,0Eh
		mov bl,07h		; attribute
		mov bh,[cs:BIOS_page]	; current page
		int 10h
		popad
		popfd
		ret

;
;
; kaboom2: once everything is loaded, replace the part of kaboom
;	   starting with "kaboom.patch" with this part

kaboom2:
		mov si,err_bootfailed
		call cwritestr
		call getchar
		call vgaclearmode
		int 19h			; And try once more to boot...
.norge:		jmp short .norge	; If int 19h returned; this is the end

;
; mangle_name: Mangle a DOS filename pointed to by DS:SI into a buffer pointed
;	       to by ES:DI; ends on encountering any whitespace
;

mangle_name:
		mov cx,11			; # of bytes to write
mn_loop:
		lodsb
		cmp al,' '			; If control or space, end
		jna mn_end
		cmp al,'.'			; Period -> space-fill
		je mn_is_period
		cmp al,'a'
		jb mn_not_lower
		cmp al,'z'
		ja mn_not_uslower
		sub al,020h
		jmp short mn_not_lower
mn_is_period:	mov al,' '			; We need to space-fill
mn_period_loop: cmp cx,3			; If <= 3 characters left
		jbe mn_loop			; Just ignore it
		stosb				; Otherwise, write a period
		loop mn_period_loop		; Dec CX and (always) jump
mn_not_uslower: cmp al,ucase_low
		jb mn_not_lower
		cmp al,ucase_high
		ja mn_not_lower
		mov bx,ucase_tab-ucase_low
                cs xlatb
mn_not_lower:	stosb
		loop mn_loop			; Don't continue if too long
mn_end:
		mov al,' '			; Space-fill name
		rep stosb			; Doesn't do anything if CX=0
		ret				; Done

;
; Upper-case table for extended characters; this is technically code page 865,
; but code page 437 users will probably not miss not being able to use the
; cent sign in kernel images too much :-)
;
; The table only covers the range 129 to 164; the rest we can deal with.
;
ucase_low	equ 129
ucase_high	equ 164
ucase_tab	db 154, 144, 'A', 142, 'A', 143, 128, 'EEEIII'
		db 142, 143, 144, 146, 146, 'O', 153, 'OUUY', 153, 154
		db 157, 156, 157, 158, 159, 'AIOU', 165

;
; unmangle_name: Does the opposite of mangle_name; converts a DOS-mangled
;                filename to the conventional representation.  This is needed
;                for the BOOT_IMAGE= parameter for the kernel.
;                NOTE: A 13-byte buffer is mandatory, even if the string is
;                known to be shorter.
;
;                DS:SI -> input mangled file name
;                ES:DI -> output buffer
;
;                On return, DI points to the first byte after the output name,
;                which is set to a null byte.
;
unmangle_name:
                push si                 ; Save pointer to original name
                mov cx,8
                mov bp,di
un_copy_body:   lodsb
                call lower_case
                stosb
                cmp al,' '
                jbe un_cb_space
                mov bp,di               ; Position of last nonblank+1
un_cb_space:    loop un_copy_body
                mov di,bp
                mov al,'.'              ; Don't save
                stosb
                mov cx,3
un_copy_ext:    lodsb
                call lower_case
                stosb
                cmp al,' '
                jbe un_ce_space
                mov bp,di
un_ce_space:    loop un_copy_ext
                mov di,bp
                mov byte [es:di], 0
                pop si
                ret

;
; lower_case: Lower case a character in AL
;
lower_case:
                cmp al,'A'
                jb lc_ret
                cmp al,'Z'
                ja lc_1
                or al,20h
                ret
lc_1:           cmp al,lcase_low
                jb lc_ret
                cmp al,lcase_high
                ja lc_ret
                push bx
                mov bx,lcase_tab-lcase_low
               	cs xlatb
                pop bx
lc_ret:         ret

; -----------------------------------------------------------------------------
;  Common modules
; -----------------------------------------------------------------------------

%include "getc.inc"		; getc et al
%include "conio.inc"		; Console I/O
%include "writestr.inc"		; String output
%include "parseconfig.inc"	; High-level config file handling
%include "parsecmd.inc"		; Low-level config file handling
%include "bcopy32.inc"		; 32-bit bcopy
%include "loadhigh.inc"		; Load a file into high memory
%include "font.inc"		; VGA font stuff
%include "graphics.inc"		; VGA graphics
%include "highmem.inc"		; High memory sizing

; -----------------------------------------------------------------------------
;  Begin data section
; -----------------------------------------------------------------------------

CR		equ 13		; Carriage Return
LF		equ 10		; Line Feed
FF		equ 12		; Form Feed
BS		equ  8		; Backspace

;
; Lower-case table for codepage 865
;
lcase_low       equ 128
lcase_high      equ 165
lcase_tab       db 135, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138
                db 139, 140, 141, 132, 134, 130, 145, 145, 147, 148, 149
                db 150, 151, 152, 148, 129, 155, 156, 155, 158, 159, 160
                db 161, 162, 163, 164, 164

copyright_str   db ' Copyright (C) 1994-', year, ' H. Peter Anvin'
		db CR, LF, 0
boot_prompt	db 'boot: ', 0
wipe_char	db BS, ' ', BS, 0
err_notfound	db 'Could not find kernel image: ',0
err_notkernel	db CR, LF, 'Invalid or corrupt kernel image.', CR, LF, 0
err_noram	db 'It appears your computer has less than '
		asciidec dosram_k
		db 'K of low ("DOS")'
		db CR, LF
		db 'RAM.  Linux needs at least this amount to boot.  If you get'
		db CR, LF
		db 'this message in error, hold down the Ctrl key while'
		db CR, LF
		db 'booting, and I will take your word for it.', CR, LF, 0
err_badcfg      db 'Unknown keyword in syslinux.cfg.', CR, LF, 0
err_noparm      db 'Missing parameter in syslinux.cfg.', CR, LF, 0
err_noinitrd    db CR, LF, 'Could not find ramdisk image: ', 0
err_nohighmem   db 'Not enough memory to load specified kernel.', CR, LF, 0
err_highload    db CR, LF, 'Kernel transfer failure.', CR, LF, 0
err_oldkernel   db 'Cannot load a ramdisk with an old kernel image.'
                db CR, LF, 0
err_notdos	db ': attempted DOS system call', CR, LF, 0
err_comlarge	db 'COMBOOT image too large.', CR, LF, 0
err_a20		db CR, LF, 'A20 gate not responding!', CR, LF, 0
err_bootfailed	db CR, LF, 'Boot failed: please change disks and press '
		db 'a key to continue.', CR, LF, 0
ready_msg	db 'Ready.', CR, LF, 0
crlfloading_msg	db CR, LF
loading_msg     db 'Loading ', 0
dotdot_msg      db '.'
dot_msg         db '.', 0
aborted_msg	db ' aborted.'			; Fall through to crlf_msg!
crlf_msg	db CR, LF
null_msg	db 0
crff_msg	db CR, FF, 0
syslinux_cfg	db 'SYSLINUXCFG'
%if IS_MDSLINUX
manifest	db 'MANIFEST   '
%endif
;
; Command line options we'd like to take a look at
;
; mem= and vga= are handled as normal 32-bit integer values
initrd_cmd	db 'initrd='
initrd_cmd_len	equ 7

;
; Config file keyword table
;
%include "keywords.inc"

;
; Extensions to search for (in *forward* order).
;
exten_table:	db 'CBT',0		; COMBOOT (specific)
		db 'BSS',0		; Boot Sector (add superblock)
		db 'BS ',0		; Boot Sector 
		db 'COM',0		; COMBOOT (same as DOS)
		db 'C32',0		; COM32
exten_table_end:
		dd 0, 0			; Need 8 null bytes here

;
; Misc initialized (data) variables
;
%ifdef debug				; This code for debugging only
debug_magic	dw 0D00Dh		; Debug code sentinel
%endif
AppendLen       dw 0                    ; Bytes in append= command
OntimeoutLen	dw 0			; Bytes in ontimeout command
OnerrorLen	dw 0			; Bytes in onerror command
KbdTimeOut      dw 0                    ; Keyboard timeout (if any)
CmdLinePtr	dw cmd_line_here	; Command line advancing pointer
initrd_flag	equ $
initrd_ptr	dw 0			; Initial ramdisk pointer/flag
VKernelCtr	dw 0			; Number of registered vkernels
ForcePrompt	dw 0			; Force prompt
AllowImplicit   dw 1                    ; Allow implicit kernels
AllowOptions	dw 1			; User-specified options allowed
SerialPort	dw 0			; Serial port base (or 0 for no serial port)
VGAFontSize	dw 16			; Defaults to 16 byte font
UserFont	db 0			; Using a user-specified font
ScrollAttribute	db 07h			; White on black (for text mode)
;
; Stuff for the command line; we do some trickery here with equ to avoid
; tons of zeros appended to our file and wasting space
;
linuxauto_cmd	db 'linux auto',0
linuxauto_len   equ $-linuxauto_cmd
boot_image      db 'BOOT_IMAGE='
boot_image_len  equ $-boot_image
ldlinux_end	equ $
ldlinux_len	equ $-ldlinux_magic
;
; Put the getcbuf right after the code, aligned on a sector boundary
;
end_of_code	equ (ldlinux_end-bootsec)+7C00h
getcbuf		equ (end_of_code + 511) & 0FE00h

; VGA font buffer at the end of memory (so loading a font works even
; in graphics mode.)
vgafontbuf	equ 0E000h

; This is a compile-time assert that we didn't run out of space
%ifndef DEPEND
%if (getcbuf+trackbufsize) > vgafontbuf
%error "Out of memory, better reorganize something..."
%endif
%endif

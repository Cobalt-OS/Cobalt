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


; 7/2003: If you know what you are doing, you can compile with
; -DSMALLERBINS or even -DSMALLESTBINS to reduce the default
; element size from 8 sectors per bin to 4 or 2 sectors per bin,
; or -DBIGGERBINS to increase the element size to 16 sectors,
; which is the maximum. NONE of the nonstandard element sizes
; have been tested so far!!! Note that smaller elements mean smaller
; maximum cache size, e.g. size > 50 size units may crash if using
; -DSMALLERBINS, and > 25 size units may crash for -DSMALLESTBINS.


	; status table handling functions
	; DUMB BUT FAST experimental old version was:
	; bin is always equal sector number modulo sectors (or none)!

	; NEW 27.11.2001: shrink table by using one entry for more
	; than one sector! (2..16 - MS SmartDrv has the same range)
	; BUGFIX 24.01.2002: flush* were the wrong size!

	; *** NEW 09.11.2002: shrink factor changed from 4 to 8
	; *** split up into binsel.asm and binsel2.asm, improved
	; *** telltabsize (-15.11.2002)

	; *** NEW 15.11.2002 some memoization tables get shape,
	; *** as otherwise the binsel2 stuff is far too slow!

	; BUGFIX 25.08.2003: telltabsize limit also checks offsets now

%ifdef SMALLESTBINS
%define BINSHR 1	; *** 2 sectors per bin
%define BINMASK 0xfe	; *** 2 sectors per bin -> NOT ((1<<BINSHR)-1)
%define BINBITS 1	; *** 2 sectors per bin -> (1<<BINSHR) - 1
%define BINTABFORM 0x0103	; *** encoding THIS table format...

%elifdef SMALLERBINS
%define BINSHR 2	; *** 4 sectors per bin
%define BINMASK 0xfc	; *** 4 sectors per bin -> NOT ((1<<BINSHR)-1)
%define BINBITS 3	; *** 4 sectors per bin -> (1<<BINSHR) - 1
%define BINTABFORM 0x0203	; *** encoding THIS table format...

%elifdef BIGGERBINS
%define BINSHR 4	; *** 16 sectors per bin
%define BINMASK 0xf0	; *** 16 sectors per bin -> NOT ((1<<BINSHR)-1)
%define BINBITS 15	; *** 16 sectors per bin -> (1<<BINSHR) - 1
%define BINTABFORM 0x0403	; *** encoding THIS table format:...

%else
%define BINSHR 3	; *** 8 sectors per bin
%define BINMASK 0xf8	; *** 8 sectors per bin -> NOT ((1<<BINSHR)-1)
%define BINBITS 7	; *** 8 sectors per bin -> (1<<BINSHR) - 1
%define BINTABFORM 0x0303	; *** encoding THIS table format:
	; ^- HI is log2(sectors/bin), LO is log2(bytes/bin)
	; *** values for 4 s/bin were 2,0xfc,3,0x0203 (*** was 0x202)
	; *** values for 8 s/bin are  3,0xf8,7,0x0303
%endif

; %error binsel settings: BINSHR, BINMASK, BINBITS, BINTABFORM

	; Format is - per entry - D sector low, B drive, B LRU,
	; W bitfield: lsb for first sector in group

        ; Input is sector number EAX drive DL, output (xms) bin AX            
	; findbin:  finds a bin for a location (stc if not found),
	; newbin:   allocates a new bin for a given location,
	;           flushing old bins if needed.
	;           (main bin selection "intelligence" !)

	; flush:    empties all slots (guaranteed to be called
	;           BEFORE the int 0x13 dispatcher is enabled)
	; flushone: empties all slots for drive DL only

	; telltabsize: (returns carry on error)
	;           tells in AX how big a table for [sectors] will be
	;           returns carry if too big (ADJUST notion of TOO
	;           BIG here, according to STACK/TSR-CODE SIZE...).


			; *** WARNING: "bonustable" gets initially
			; *** filled with lots of near pointers to
			; *** "table", by flush and flushone!

%define BONUSSIZE 512	; *** amount of additional bytes to allocate
bonustable:		; *** pointer to additional bytes
tableend:	dw 0	; *** pointer past table
bonusend:	dw 0	; *** pointer past bonustable


telltabsize:	; calculate size of the table for THIS format
		; and for for [sectors] sectors, returned in AX.
		; returns carry set (CY) if too big.
	push bx
	push eax
	mov word [cs:tabsz],BINTABFORM	; encoding THIS table format
	movzx eax,ax	; *** NEW 24.01.2002
	add eax,BINBITS		; <- round up!
	shr eax,BINSHR		; <- ADJUST
	shl eax,3		; 8 bytes per table entry
				; *** new 11/2002: more pointers
	  mov bx,table		; label: dynamical alloc begins here
	  add bx,ax
	  mov [cs:bonustable],bx	; *** bonustable is after table
	  mov [cs:tableend],bx		; ***
	add eax,BONUSSIZE	; *** new 11/2002: bonustable
	  mov bx,table		; as above...
	  add bx,ax		; ...this time with table
	  mov [cs:bonusend],bx	; makes live easier... - end of bonustable

	push eax		; %
	add eax,table		; % end of allocated memory, NOT counting stack
				; % (setup.asm allocates << 1 kB of stack)
	cmp eax,0xfc00		; maximum allowed size (CY if more)
	pop eax			; % old check was max 0xe800 table size,
				; % new check is max 0xfc00+stack alloc size.

	cmc			; complement carry: "jb NC / jnb CY"

	jnc nosanitizetab		; % was size acceptable?
		mov bx,table+32		; % safe size in case of flush
		mov [cs:bonustable],bx	; % ***
		mov [cs:tableend],bx	; % ***
		add bx,BONUSSIZE	; %
		mov [cs:bonusend],bx	; %
	stc				; %
nosanitizetab:				; %

	mov bx,ax	; save pure (table + bonustable) size
	pop eax
	mov ax,bx	; restore size of tables
	pop bx
	ret

; -------------


%if 0		; enable when you need it in your BINSEL2.ASM

hashme: 	; find si as pointer into [table], given
		; table length [sectors] entries, 8 (EIGHT) bytes
	push edx	; each, for sector number EAX and drive DL
	push eax
	push ecx
	;
	cmp dl,0x80	; compress bits of drive number a bit
	jb hshdsk
	and dl,0x7f
	add dl,4	; (ca. 4 floppies)
hshdsk:	movzx edx,dl	; derived from drive number
	shl edx,24	; move to some part of those 32bits *** was 9
	xor eax,edx	; XOR in drive into the sector number
	;
	shr eax,BINSHR	; all entries of one main entry hash same!
	movzx ecx,word [cs:sectors]	; usually 512*N,
	shr ecx,BINSHR	; <- ADJUST!
	xor edx,edx	; N in 1,2,4,6,8,10,12,14,16,18 (>100 thus)
	dec ecx		; ... has more interesting prime factors(*)
	div ecx		; MODULO sectors as "HASH" (edx is remainder)
	;
			;  ... use remainder as index ...
	lea edx,[table+edx*8]	; 8by (EIGHT) per MAIN entry
			; still 8 byte per main entry, but 1<<BINSHR
			; sub entries now share one main entry!
	mov si,dx	; pointer to MAIN entry
	;
	pop ecx
	pop eax
	pop edx		; (*) note: 1 bin left unused, we may as well
	ret		; do a sectors-- on install...

%endif


; -------------

; macro BINBCHECK ARG does (all registers preserved):
; ARG [ds:si+6],bit 1 shl (al and BINBITS)
; where ARG can for example be OR or TEST

%imacro BINBCHECK 1	; takes the command as arg, works on bitfield
	push ax		; Byte SI+6, Bit AL
	push cx
	and al,BINBITS	; <- ADJUST
	mov cl,al
	mov ax,1
	shl ax,cl	; select the bit in the bitmask
	%1 [ds:si+6],ax	; or: fill bin
			; test: return NZ if bin is filled
	pop cx
	pop ax
%endmacro

; **************************************************************

%include "binsel2.asm"	; the main cleverness functions:

	; findbin EAX.DL (sector, drive) returns CARRY (not found)
	; or the sub-bin (in AX) that contains the sector.
	; can as side-effect update some statistics embedded in
	; the table.

	; newbin EAX.DL allocates a new bin for the sector and
	; marks the appropriate sub-bin as used. All other sub-bins
	; are discarded from the affected main bin IF the main bin
	; does not share the needed EAX.DL range!

	; can use the table on position table directly, or use
	; hashme EAX.DL to have a suggested pointer SI calculated
	; which points into table. Can use sectors, which is the
	; number of sub-bins (shr BINSHR to get the main table
	; entry number - 8 byte per main table entry. and BINBITS
	; to know sub-bin number, or "and low byte, BINMASK" to
	; calculate EAX.DL of main bin from EAX.DL of sub-bin...

	; macro BINBCHECK ARG does (all registers preserved):
	; ARG [ds:si+6],bit 1 shl (al and BINBITS)
	; where ARG can for example be OR or TEST

; **************************************************************

; -------------

flushone:	; flushing only one drive (DL)
	push ds	; mark all table entries for that
	push di	; drive as empty, using THIS table format
	push cx
	push eax
	mov cx,cs
	mov ds,cx
	mov cx,[ds:sectors]
	add cx,BINBITS	; <- ADJUST: round up
	shr cx,BINSHR	; <- ADJUST: sub entries -> main entries
	mov di,table	; no variable, but a label...
	xor eax,eax
flolp:	cmp [di+4],dl
	jnz flon	; do not flush other drives
	dec eax
	mov [di],eax	; sector -1
	inc eax
	mov [di+4],eax	; drive/LRU/bits 0
flon:	add di,8
	loop flolp
	pop eax
	pop cx
	pop di
	pop ds
	call BONUSFLUSH	; *** 11/2002: IMPORTANT: FLUSH BONUSTABLE
	ret		; (bonusflush could IN THIS CASE be a bit)
			; (nice to non-related entries, whatever.)

; -------------

flush:	push ds ; flushing/initializing the whole table
	push di	; marks every table entry as empty
	push cx	; using THIS table format now!
	push eax
	mov cx,cs
	mov ds,cx
	mov cx,[ds:sectors]
	add cx,BINBITS	; <- ADJUST: round up
	shr cx,BINSHR	; <- ADJUST: sub entries -> main entries
	mov di,table	; no variable, but a label...
	xor eax,eax
flloop:	dec eax
	mov [ds:di],eax	; sector -1
	add di,4
	inc eax
	mov [ds:di],eax	; drive 0, LRU 0 (unused), bits 0
	add di,4
	loop flloop
	pop eax
	pop cx
	pop di
	pop ds
	call BONUSFLUSH	; *** 11/2002 IMPORTANT! INIT BONUSTABLE
	ret

; -------------

BONUSFLUSH:	; *** need to init BONUSTABLE with "table" !!!
	push di
	push cx
	mov di,[cs:bonustable]
	mov cx,BONUSSIZE>>1
bfllp:	mov word [cs:di],table	; *** cannot stosw here! (es:)
	inc di
	inc di
	loop bfllp
	pop cx
	pop di
	ret


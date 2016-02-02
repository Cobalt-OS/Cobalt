; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2005-2009.
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

	; ACPI throttle / off functions for FDAPM by Eric Auer 2005-2009
	; Updates 2006:  parser & acpitool were the only updated FDAPM parts
	; Update 1/2007: Support for PM1b ports and better ROM scan
	; to be placed LAST in FDAPM binary (uses buffers after code)
	; Update 9/2009: Use sourceforge email address (coli account gone)

	; supported functions: THROTTLE - call with AX=0..8 to get
	; S1 HLT, 1..8/8 speed, and ACPIOFF - call, after flushing
	; buffers, to soft-off the system (S4/S5 state).
	; new 6/2007: ACPIDUMP - hexdumps a lot of ACPI data

	; Sn states: 0 on, 1 stopped (press power button to wake up),
	; (2 CPU off*), 3 suspend to RAM*, 4 suspend to disk*, 5 off*
	; * system will reboot on wake up, but S2-S4 let you install
	; a quick boot handler which restores the system state which
	; you had to save yourself first (too complicated for DOS!).

%define STANDALONE_ACPITOOL 0
%define CURIOUS_ACPI 0		; set to 1 to enable some debug code

%if STANDALONE_ACPITOOL		; create some test cases
	org 100h
start:	mov ax,4		; half speed
	call throttle
	jc failed
	mov ax,0		; stopped
	call throttle
	jc failed
	mov ax,8		; full speed
	call throttle
	jc failed
	call acpioff		; power off
	; (never returns if power off worked)
failed:	mov ax,4cffh
	int 21h
%endif	; STANDALONE_ACPITOOL

	; -------------

s1_sys:	; *** S1 ***		; "stop" case: caller should flush caches
	mov ax,[s1mode]		; etc. before selecting this "speed".
	cmp al,-1		; check only 1a part
	jnz s1_okay
	push ax
	push dx
	mov ah,9
	mov dx,noS1DSDTmsg	; SPEED0 not supported: ACPI S1 code unknown
	int 21h
	pop dx
	pop ax
s1_okay:
	jmp sn_sys

c_ret:	stc
	ret

acpidump:			; ACPIDUMP: Only fetch and print ACPI info
	mov byte [dumpacpi],1	; activate dump mode
	call getacpi
	mov byte [dumpacpi],0	; end dump mode
	ret			; CY set / clear depends on getacpi status

acpioff: ; *** S5 ***		; Just ACPI soft-off the system
	call getacpi		; collect ACPI data
	jc c_ret
	mov ax,[s5mode]
	cmp al,-1		; check only 1a part
	jnz s5_okay
	push ax
	push dx
	mov ah,9
	mov dx,noS5DSDTmsg	; SPEED0 not supported: ACPI S1 code unknown
	int 21h
	pop dx
	pop ax
s5_okay:
	jmp sn_sys		; Enter that mode of no return!

	; -------------

sn_sys:	cmp al,-1		; trigger S1 sleep or S5 soft-off etc.
	jz c_ret		; selected Sn state not supported
	;
	cli
	push ax
	mov dx,pm1aSts		; OFFSET of first of two variables
	call pm_read		; read pm1?Status into AX
	or ax,8700h		; clear (by writing 1) pending "waking up",
	mov dx,pm1aSts		; OFFSET of first of two variables
	call pm_write		; "RTC ring" "sleep button", "power button".
	mov dx,pm1aEn		; OFFSET of first of two variables
	call pm_read		; read pm1?Enable
	or ax,300h		; enable "sleep button" and "power button".
	mov dx,pm1aEn		; OFFSET of first of two variables
	call pm_write		; write pm1?Enable
	pop ax
	;
	push ax		; note that SLP_TYP for 1a and 1b can be different!
	mov ah,0
	shl ax,10	; 400h * number for 1a SLP_TYP (386 instructions okay)
	or ax,2000h	; "enter new mode" flag
	mov dx,[pm1aCnt] ; PM1?Control is NO normal block for SLP_TYP bits...
	out dx,ax	; Ta-Daa! Write PM1aControl first
	pop ax
	xchg al,ah
	mov ah,0
	shl ax,10	; 400h * number for 1b SLP_TYP (386 instructions okay)
	or ax,2000h	; "enter new mode" flag
	mov dx,[pm1bCnt] ; PM1?Control is NO normal block for SLP_TYP bits...
	or dx,dx
	jz no_1b2		; no 1b port, so we are done here
	out dx,ax		; Ta-Daa! Write PM1bControl second
no_1b2:
; *** we are now supposed to read the status registers to get confirmation ***
	sti
; -	mov dx,[pblk_io]	; would have to check if latency below 100ys
; -	add dx,4		; before we may use the p_lvl2 trigger port:
; -	in al,dx		; reading the port triggers CPU C2 sleep
	hlt			; HLT triggers CPU C1 sleep
	clc			; we could read pm1?Status until MSB is set
just_read_speed:
	mov ax,bp
	ret

	; -------------

pm_read:			; read value from group described at DX
	push si
	push dx
	mov si,dx
	mov dx,[si]		; pm1a
	in ax,dx		; pm1a
	mov dx,[si+2]		; pm1b
	or dx,dx
	jz no_1b1		; no 1b port, so we are done here
	push ax			; remember PM1a value
	in ax,dx		; pm1b
	pop dx			; was ax: PM1a value
	or ax,dx		; OR both blocks to get value, see ACPI spec
no_1b1:	pop dx
	pop si
	ret

pm_write:			; write value from AX to group described at DX
	push si
	push dx
	mov si,dx
	mov dx,[si]		; pm1a
	out dx,ax		; pm1a...
	mov dx,[si+2]		; pm1b
	or dx,dx
	jz no_1b1		; no 1b port, so we are done here
	out dx,ax		; pm1b...
	jmp short no_1b1	; common exit code

	; -------------

throttle:			; THROTTLE: call with AX=1..8 for 1..8/8 speed
	push ax			; or AX=0 for S1 stop or AX=9 to read speed.
				; returns old speed in AX, destroys registers.
	call getacpi		; collect ACPI data
	pop ax
	jc c_ret2
	push ax
	call read_cpu_speed	; fetch previous speed (5/2005)
	mov bp,ax
	pop ax
	cmp ax,9
	jz just_read_speed
	cmp ax,8
	ja c_ret2		; out of range
	jz no_tht
	or ax,ax		; special case: S1 (CPU stop)
	jz s1_sys
	jmp norm_tht

c_ret2: stc
        ret

no_tht:	; *** no THROTTLE ***
	xor ebx,ebx		; to-be-set value
some_tht:
	mov dx,[pblk_io]
	in eax,dx
	and al,0efh		; disable throttling
	out dx,eax		; must be disabled while modifying
	;
	push edx	; and eax,"not the throttle value for 7/8 speed"
	mov cx,[thtbits]	; low: bit offset, high: bit count
	xor edx,edx
	inc edx
	xchg cl,ch
	shl edx,cl
	dec edx	; maximum value
	xchg cl,ch
	shl edx,cl	; shift to right place
	not edx
	and eax,edx	; turn off all duty cycle bits
	pop edx
	;
	or eax,ebx		; possibly enable new throttling
	out dx,eax		; Ta-Daa!
	mov ax,bp		; previous speed
	clc
go_ret:	ret

norm_tht:			; throttle to 1..7/8 speed
	movzx eax,al
	mov cx,[thtbits]	; low: bit offset, high: bit count
	cmp ch,3
	jc go_ret		; must be at least 8 steps - return CY if less
	sub ch,3		; if more steps, just use every Nth
	add cl,ch
	shl eax,cl
	or al,10h		; bit "enable throttling"
	mov ebx,eax		; to-be-set value
	jmp short some_tht

	; -------------

	; -------------

read_cpu_speed:		; internal (must be called after getacpi) - 5/2005
	push dx		; returns current CPU throttle (8 = full speed) in EAX
	push cx		; other registers preserved
	mov dx,[pblk_io]
	in eax,dx
	test al,10h		; was throttle enabled at all?
	jz full_read_speed
	and al,0efh		; remove throttle enable bit
	mov cx,[thtbits]	; low: bit offset, high: bit count
	sub ch,3
	jbe normalized_read_speed	; assume "less than 3 means 3"
	add cl,ch		; will shift away the extra bits
normalized_read_speed:
	shr eax,cl
done_read_speed:
	movzx eax,al		; return AX value in (0) 1..7 range now
	clc
	pop cx
	pop dx
	ret
full_read_speed:
	mov al,8		; assume full speed
	jmp short done_read_speed

	; -------------
	; -------------
	; -------------

getacpi:	; fetch ACPI tables and values (destroys registers)
	mov ax,[facpptr]	; FACP pointer as "already done" flag
	or ax,[facpptr+2]
	jz no_acpi_yet	; "did not try at all yet" value
	cmp ax,1	; "already tried but failed" value
	jz no_acpi_retry
	clc		; ... then we can just return, as
	ret		; all values are already known :-)
	;
no_acpi_retry:
	stc		; we already know that it will not work
	ret
	;
inntdosbox:
	mov ah,9
	mov dx,dosboxmsg
	int 21h		; complain if tried to run in WinNT family DOS box
	jmp altacpi	; ... and show blatant ad: THROTTLE can do WinNT
	;
no_acpi_yet:
	mov ax,3306h	; get internal DOS version BL:BH.DL/DH (DOS 5+)
	xor bx,bx	; (DOS 4/older returns AL=-1)
	int 21h
	cmp bx,3205h
	jz inntdosbox
	cmp al,-1
	jz inntdosbox
	;
	mov ax,40h	; BIOS data area
	mov ds,ax
	mov ax,[ds:0eh]	; EBDA segment
	or ax,ax
	jz noebda
	cmp ax,200h	; 200..3fc probably is just an LPT I/O port
	jb hasebda
	cmp ax,3fch
	jb noebda
hasebda:
	mov cx,1024/16	; scan only first 1 kb, according to ACPI spec
	mov ds,ax	; EBDA segment
	xor bx,bx
scan_e:	call check_rsdp	; check if DS:BX has RSD PTR structure
	jz found_rsdp
	add bx,16
	loop scan_e
noebda:	mov ax,0f000h
	mov ds,ax
	xor bx,bx
scan_r:	call check_rsdp	; check if DS:BX has RSD PTR structure
	jz found_rsdp
	add bx,16
	cmp bx,0ffe0h
	jnz scan_r	; check 64k BIOS, standard f000:x space
scan_x:	mov ax,0e000h
	mov ds,ax
	xor bx,bx
	cmp word [ds:bx],0xaa55	; next byte is size in half-kB, but ignored
	jz scan_y
	push ds
	mov ax,cs
	mov ds,ax
	mov ah,9
	mov dx,badromsigmsg	; scan e000:x anyway, but complain
	int 21h
	pop ds
scan_y:	call check_rsdp	; check if DS:BX has RSD PTR structure
	jz found_rsdp
	add bx,16
	cmp bx,0ffe0h
	jnz scan_y	; check extra 64k BIOS from e000:0
noacpi:	push cs
	pop ds
	mov ah,9
	mov dx,noacpimsg
	int 21h		; show complaint message
altacpi:
	mov ah,9
	mov dx,contactmsg	; show ad: contact info, THROTTLE, PCISLEEP.
	int 21h		; THROTTLE works w/o ACPI BIOS, using a PCI table.
			; PCISLEEP can find ACPI port w/o BIOS: most chips
			; use "3 bits starting with bit 1" at ACPI base+16.
	mov word [facpptr],1	; flag to avoid trying again - no RSD PTR
	stc		; no ACPI found
	ret


found_rsdp:				; assume 386 present from here on
	mov eax,[ds:bx+9]		; ignore checksum [8] and copy OEM
	mov [cs:rsdoem],eax		; name [9] (6 bytes + trailing 00)
	mov ax,[ds:bx+13]
	mov [cs:rsdoem+4],ax
	mov eax,[ds:bx+10h]		; linear RSDT location in RAM
	push cs				; RSDT is a list of table pointers
	pop ds
	call fetch_high			; load RSDT to thebuffer
	jc noacpi
	mov ax,[thebuffer+4]		; size
	cmp ax,24h			; ... without header ...
	jbe noacpi			; ACPI 0.9 not supported
	mov bx,ax
	sub bx,4			; last pointer
	; header: dd RSDT, size / db revision, checksum / 6db OEMname /
	; 8db OEMtabname / 4db OEMrevbcd / 4db compilerid / 4db compilerrev
	mov eax,[thebuffer+10h]		; copy OEMtabname: is manufacture
	mov [rsdmodel],eax		; model ID in RSDT case...
	mov eax,[thebuffer+14h]
	mov [rsdmodel+4],eax
	xor eax,eax			; NULL item as end marker
	push eax
more_r:	mov eax,[thebuffer+bx]	; linear address of a table
	push eax			; store on stack
	sub bx,4
	cmp bx,24h			; only header left?
	jnb more_r			; else keep collecting

find_facp:
	pop eax
	or eax,eax			; end of the list?
	jz check_facp	; check if we are done
	push eax
	call fetch_high			; ignore headers, load some table
	pop eax
	jc find_facp			; if fetch failed, keep searching
	cmp dword [thebuffer],'FACP'	; FACP found?
	jz facp_found
no_facp:
	cmp dword [thebuffer],'SSDT'	; SSDT (DSDT extension) found?
	jnz no_ssdt
ssdt_found:
	mov [ssdtptr],eax		; only one SSDT supported
;	jmp short find_facp		; keep walking
no_ssdt:
	jmp short find_facp		; walk through stacked pointers

check_facp:
	mov eax,[facpptr]		; did we find a FACP?
	or eax,eax
	jz near noacpi			; end marker and no FACP yet. Bad.
	jmp near fetch_dsdt	; parse DSDT and go on

	; (could show a message now if a SSDT is present)

facp_found:
	mov [facpptr],eax		; there should be only one...
	xor ax,ax
	or ax,[thebuffer+32h]		; high word of SMI port
	or ax,[thebuffer+3ah]		; high word of PM1a event port
	or ax,[thebuffer+3eh]		; high word of PM1b event port
	or ax,[thebuffer+42h]		; high word of PM1a control reg block
	or ax,[thebuffer+46h]		; high word of PM1b control port
	; we ignore: 48 dd PM2 control port, 4c dd PM timer port
	; 50 dd GPE0 reg block (port base), 54 dd GPE1 ...
	; ... and block sizes: 58..5d of pm1x evt, pm1x ctrl,
	; pm2 ctrl, pm timer, gpe0, gpe1. 5e is gpe0->1 event number offset
	;
	; MAYBE useful: 60 dw C2 latency, 62 dw C3 latency: C2 / C3 above
	; 100 / 1000 resp. means "not supported". Problem: C3 usage can
	; require some CPU state save / restore work by the Op.Sys.!?
	; C2 saves more energy than normal STI/HLT sequence (C1 state).
	; S1 state is defined to involve STPCLK#, so CPU goes Cn, n > 0.
	;
	; 64/66 dw for cache spill (used if WBINVD is broken)
	jnz non16_acpi			; any disturbing non-zero fields
	call good_acpi	; collect some FACP contents
	jmp short find_facp		; keep walking

non16_acpi:
	mov ah,9			; which would need extra handling?
	mov dx,badacpimsg
	int 21h				; then give up and complain!
	mov dword [facpptr],1		; avoid trying again - not 16bit
	stc
	ret

good_acpi:
	mov eax,[thebuffer+24h]		; FACS address (not really used yet)
	mov [facsptr],eax
	mov eax,[thebuffer+28h]		; DSDT address
	mov [dsdtptr],eax
%if CURIOUS_ACPI
	mov ax,[thebuffer+2eh]
	mov [smi_irq],ax		; SMI IRQ (triggered by ACPI/SMI)
	call showax
	mov ax,[thebuffer+30h]		; (high word checked later)
	mov [smi_io],ax			; SMI command port (to control SMI)
	call showax
	mov ax,[thebuffer+34h]
	mov [smi_flg],ax		; low/high: SMI command ACPI/SMI mode
	call showax
	; 36: SMI command for S4BIOS (triggers BIOS assisted S3->S4...)
%endif
	mov ax,[thebuffer+38h]		; (high word known to be 0)
	mov [pm1aSts],ax		; PowerMgmt 1a event reg block
	mov [pm1aEn],ax			; second register in block, fixed later
	add ax,10h			; normal pm1a_evt -> p_blk offset :-)
	mov [pblk_io],ax		; THROTTLE port base
	mov ax,[thebuffer+3ch]		; (high word known to be 0)
	mov [pm1bSts],ax		; PowerMgmt 1b event reg block
	mov [pm1bEn],ax			; second register in block, fixed later
	xor ax,ax
	mov al,[thebuffer+58h]		; PM1_EVT_SIZE, either 4 or (rarely) 8
	shr ax,1
	add [pm1aEn],ax			; fix second register base in block
	add [pm1bEn],ax			; fix second register base in block
	;
	mov ax,[thebuffer+40h]		; (high word known to be 0)
	mov [pm1aCnt],ax		; PowerMgmt 1a control reg block
	mov ax,[thebuffer+44h]		; (high word known to be 0)
	mov [pm1bCnt],ax		; PowerMgmt 1b control reg block
	mov ax,[thebuffer+68h]
	mov [thtbits],ax		; low: duty cycle shift value
					; high: duty cycle bit count
	; 6a/6b/6c: if nonzero, RTC alarm reg numbers for day/month/century
	; 5f/6d reserved, 6e dd flags: bit 0 "wbinvd supported",
	; 1 "wbinvd needs cache cheat", 2 "all CPUs can do C1 sleep"
	; 3 "C2 configured for multi-CPU" (4..6 differ for ACPI 0.9...)
	; 4 power button as ctrl method , 5 sleep button: on = ctrl method
	; or none, else fixed feature... 6 no rtc wake support in fixed
	; reg space, 7 rtc can wake from S4, 8 timer is 32 (not 24) bits.
ret

fetch_dsdt:
	mov eax,[dsdtptr]		; fetch DSDT pointer
	or eax,eax			; null pointer check
	jz nodsdt
	call fetch_high
	jc nodsdt			; access failed, bad luck
	cmp dword [thebuffer],'DSDT'	; ACPI must always have a DSDT
	jnz nodsdt		; next are dd size, db version, db checksum
; --- 	mov eax,[thebuffer+10]		; usually e.g. chipset name
; --- 	mov [dsdtoem],eax		; ... often equal to RSDT OEM name
; --- 	mov ax,[thebuffer+14]
; --- 	mov [dsdtoem+4],ax
	call analyze_dsdt		; find Sn state codes for poweroff
					; ... and allow pblk_io override

	mov al,[s5mode]			; found S5/1a?
	or al,[s1mode]			; found S1/1a?
	cmp al,-1			; if one is missing, al is -1
	jnz got_s1s5			; skip if we are already happy

try_ssdt:
	mov eax,[ssdtptr]		; fetch SSDT pointer
	or eax,eax			; might be 0, SSDT is optional
	jz nodsdt
	call fetch_high
	jc nodsdt			; access failed, bad luck
	cmp dword [thebuffer],'SSDT'
	jnz nodsdt
	call analyze_dsdt		; find Sn state codes for poweroff

	mov ah,9
	mov dx,useSSDTmsg
	int 21h
	mov ax,[ssdtptr+2]
	call showax
	mov ax,[ssdtptr]
	call showaxFull
	call crlf

got_s1s5:
nodsdt:	mov ah,9			; ACPI checks done, display results now
	mov dx,acpimsg
	int 21h				; show some names which we found
	mov ax,[pm1aSts]	; usually general ACPI port base
	call showax
	mov dx,sizeDSDTmsg
	mov ah,9
	int 21h
	mov ax,[dsdtsz]		; yes, we ignore the high 16 bits.
	call showax		; only low 16 bits displayed... 64 kB max.
	mov al,'.'
	call showtty
	call crlf
	;
	mov ah,9		; second line of more verbose ACPI info
	mov dx,facpmsg
	int 21h
	mov ax,[facpptr+2]
	call showax
	mov ax,[facpptr]
	call showaxFull
	mov ah,9
	mov dx,dsdtmsg
	int 21h
	mov ax,[dsdtptr+2]
	call showax
	mov ax,[dsdtptr]
	call showaxFull
	mov ah,9
	mov dx,facsmsg
	int 21h
	mov ax,[facsptr+2]
	call showax
	mov ax,[facsptr]
	call showaxFull
	mov ah,9
	mov dx,pmctrlmsg
	int 21h
	mov ax,[pm1aCnt]
	call showax
	mov al,'/'
	call showtty
	mov ax,[pm1bCnt]
	call showax
	mov ah,9
	mov dx,pmstatmsg
	int 21h
	mov ax,[pm1aSts]
	call showax
	mov al,'/'
	call showtty
	mov ax,[pm1bSts]
	call showax
	call crlf
	;
	clc				; everything okay
	ret

	; -------------

check_rsdp:				; returns ZF if "RSD PTR " at ds:bx
	cmp word [ds:bx],'RS'
	jnz no_rsdp
	cmp word [ds:bx+2],'D '
	jnz no_rsdp
	cmp word [ds:bx+4],'PT'
	jnz no_rsdp
	cmp word [ds:bx+6],'R '
	jnz no_rsdp
no_rsdp:
	ret

	; -------------

analyze_dsdt:		; DSDT (and SSDT) is a very complex byte stream
	; you would need a virtual machine and name space tree model and
	; other stuff (Intel talks about a 70k library) to handle all, but
	; we only look for _S1_ and _S5_ in static definition block, which
	; contain 2-3 byte packages starting with the PM1a byte constant
	; for the hardware state number for S1 (or S5). Just grabbing a
	; constant from the "program" is quite easy ;-).
	;
	; Names can be: "abcd", ".abcdefgh", "/"+nn+"nn times 4 chars",
	; where 4 char blocks use A-Z 0-9 _, _ padded, must start with A-Z _
	; ... can be prefixed by "\" (root) or 1 or more "^" (parent).
	; We just assume that "\_Sn_" is used, not "^_Sn_" or plain "_Sn_".
	; Sizes: 00..3f or 4x yy or 8x yyyy or cx yyyyyy
	;
	mov eax,[thebuffer+4]		; DSDT size including header
	cmp dword [thebuffer],'DSDT'	; do not store dsdtsz if not DSDT
	jnz small_dsdt
	mov [dsdtsz],eax		; store for later display
small_dsdt:
	mov cx,ax			; scan size
	mov al,'1'
	mov di,s1mode
	push cx
	call find_Sn			; find \_S1_ (standby), store s1mode
	pop cx	; *** now scan for \_S3_ (suspend to RAM) if no _S1_??? ***
	mov al,'5'
	mov di,s5mode
	push cx
	call find_Sn			; find \_S5_ (power off), store s5mode
	pop cx
	mov di,pblk_io			; fetch cpu control port base
	call find_pblk			; scan for ProcessorOp(s)...
	;
	mov ax,[thtbits]		; those are not from the pblk, though
	cmp ah,3		; duty cycle setting defined?
	jae tht_okay
	or ax,ax
	jnz tht_wrong		; unexpected but nonzero throttle definition
	mov ah,9
	mov dx,thtwarn0msg	; "BIOS said throttling is not supported"
	int 21h
	jmp short tht_override
tht_wrong:
	push ax
	mov ah,9
	mov dx,thtwarnmsg
	int 21h
	pop ax
	call showax
	mov ah,9
	mov dx,thtwarnmsg2
	int 21h
tht_override:
	mov ax,301h	; IGNORE BIOS duty cycle info and use "3 bits, shl 1"!
tht_okay:
	mov [thtbits],ax		; low: duty cycle shift value
	;
	mov al,[s5mode]			; found S5/1a? (needed for ACPIOFF)
	and al,[s1mode]			; found S1/1a? (needed for SPEED0)
	cmp al,-1			; if NEITHER found, value is -1 now
	jnz s1s5_found			; found at least one of the codes
	;
	mov dx,noSnSSDTmsg
	cmp dword [thebuffer],'SSDT'	; did we just check a SSDT?
	jz fallbackSSDT_failed
	mov dx,noSnDSDTmsg
	mov eax,[ssdtptr]
	or eax,eax
	jz fallbackSSDT_failed		; no SSDT to be checked next
	mov dx,fallbackSSDTmsg		; DSDT useless but SSDT avail
	mov ah,9
	int 21h
	clc				; (do not set "avoid trying again")
	ret
	;
fallbackSSDT_failed:
	mov ah,9
	int 21h
	mov ah,9
	mov dx,contactmsg
	int 21h
	mov dword [facpptr],1	; avoid trying again later - no info in xxDT
	clc			;  if we got that far, throttle/1..8 should
	ret			; work, but acpioff and throttle/0 will fail.

s1s5_found:
	mov al,[s1mode]		; 1a
	cmp al,-1
	jz s1_notfound
	add al,'0'
	mov [s1msg],al
	mov al,[s1mode+1]	; 1b
	add al,'0'
	mov [s1msg+2],al
s1_notfound:
	mov al,[s5mode]		; 1a
	cmp al,-1
	jz s5_notfound
	add al,'0'
	mov [s5msg],al
	mov al,[s5mode+1]	; 1b
	add al,'0'
	mov [s5msg+2],al
s5_notfound:
	mov ah,9
	mov dx,s1s5msg
	int 21h
	clc
	ret

	; -------------

find_Sn:	; find _S?_ (\_S?_ before 8/2006) in CX size thebuffer
	; with ? being AL from user. Store first 2 byte items to var at DI...
	; _Sn info block: (if size were > 3F, it would use n-byte encoding)
	; 08 (DefName) "\_Sn_" 12 (PackageOp) nn (size) 02 (item count)
	; ITEM ITEM, where ITEM can be: 0, 1, -1 constants or 0A xx for
	; "byte xx" (0B xxxx / 0C xxxxxxxx would be word/dword...)
	; Norbert Remmel has a DSDT which omits the \ before the _Sn_ :-p
	push cx
	push si
	push eax
	mov si,thebuffer+24h	; base (header skipped)
	sub cx,24h+10		; consider header and target size
	jc no_Sn
	mov ah,'_'		; al is the '?' variable from the caller
	shl eax,16
	mov ax,'_S'		; EAX is "_Sn_" now
scan_S:	cmp [si],eax
	jz maybe_Sn
half_Sn:
	inc si
	loop scan_S
	jmp short no_Sn
maybe_Sn:
	cmp byte [si+4],12h	; datatype Package at _Sn_?
	jnz half_Sn	; looked good, but was not the real thing
	cmp byte [si+5],3fh
	ja half_Sn	; that is implausibly big
;	cmp byte [si+6],...	; contains 2 items normally, 4 for dual CPU?
			; first 2 items are PM1a and PM1b state codes, resp.
	mov ax,[si+7]	; 2 bytes are enough to get 1 byte item: PM1a SLP_TYP
	or al,al	; "zero"?
	jz got_Sn
	cmp al,1	; "one"?
	jz got_Sn
	cmp al,-1	; "minus one"?
	jz got_Sn_min1
	cmp al,0ah	; "byte constant"?
	jnz no_Sn	; we cannot parse other variants
	mov al,ah	; constant is in next byte
	inc si		; next byte item is 1 byte later in this case...!
	jmp short got_Sn
got_Sn_min1:
	mov al,7	; Sn values must be 3 bit values, so -1 is 7 here
got_Sn:	mov [di],al	; YES, we found a value for PM1a SLP_TYP
	mov ax,[si+8]	; 2 bytes are enough to get 1 byte item: PM1b SLP_TYP
	or al,al	; "zero"?
	jz got2Sn
	cmp al,1	; "one"?
	jz got2Sn
	cmp al,-1	; "minus one"?
	jz got2Sn_min1
	cmp al,0ah	; "byte constant"?
	jnz no_Sn	; we cannot parse other variants
	mov al,ah	; constant is in next byte
;	inc si		; next byte item is 1 byte later in this case
	jmp short got2Sn
got2Sn_min1:
	mov al,7	; Sn values must be 3 bit values, so -1 is 7 here
got2Sn:	mov [di+1],al	; YES, we found a value for PM1b SLP_TYP
no_Sn:			; common exit code
	pop eax
	pop si
	pop cx
	ret

	; -------------

find_pblk:			; find \_PR_ in CX size thebuffer and store
	; the Processor p_blk base to word at DI if it is in 1-ffff range...
	push cx
	push si
	push eax
	mov si,thebuffer+24h	; base (header skipped)
	sub cx,24h+20		; consider header and target size
	jc no_Sn		; just use the same exit code
	; Processor block info, size 0b, pblk at 8010, 6 ports (0, 4 or 6)
	; "\_PR_" 5b(prefix) 83(processor)
	; [0b "CPU0"-0 10-80-00-00(base) 6(size)]
	mov eax,'\_PR'
scan_p:	cmp [si],eax
	jz maybe_p
half_p:
	inc si
	loop scan_p
	jmp short no_Sn		; just use the same exit code
maybe_p:
	cmp byte [si+4],'_'
	jnz half_p
	cmp word [si+5],835bh
	jnz half_p
	cmp byte [si+7],11	; our parser is limited, so we use fixed size
	jz parse_p
	cmp byte [si+7],11+6	; well at least we support TWO sizes
	jnz unparse_p		; end of our flexibility
	cmp word [si+8],'\.'	; two part name: "\._PR_CPU0" for example
	jnz unparse_p
	add si,4	; an evil skip over the 4 bytes after "\."
parse_p:
	cmp byte [si+7+10],4
	jb no_p			; zero size pblk is as good as no pblk
	cmp byte [si+7+10],6	; pblk must be 4-6 ports big
	ja unparse_p
	mov eax,[si+7+1]	; CPU "name"
	mov [pblk_cpu],eax	; patch into message
	; we ignore the CPU "number" at [si+7+5]
	cmp word [si+7+6+2],0	; high word of port address
	jnz unparse_p
	mov ax,[si+7+6]		; low word of port address
	or ax,ax
	jz no_p			; zero pblk port is as good as no pblk port
	mov [di],ax		; we got a value!
	push dx
	mov ah,9		; show message
	mov dx,pblk_msg		; report processor data
	int 21h
	mov ax,[di]		; fetch port again
	call showax
	call crlf
	pop dx
	cmp word [si+7+11],835bh	; another processor block?
	jnz no_p
	push dx
	mov dx,pblk_2nd		; mention 2nd processor block
msg_p:	mov ah,9		; show message
	int 21h
	pop dx
no_p:	jmp no_Sn		; recycle the common exit code

unparse_p:
	push dx
	mov dx,pblk_odd		; warn about odd pblk contents
	jmp short msg_p

	; from Norbert's DSDT, the first with a nonzero PBLK that I met:
	; 0000000: 4453 4454 c647 0000 0139 4655 4a5f 5f5f  DSDT.G...9FUJ___
	; 0000010: 4546 355f 5f5f 5f5f 0000 0406 4d53 4654  EF5_____....MSFT
	; 0000020: 0e00 0001 1020_5c5f 5052 5f_5b 830b 4350  ..... \_PR_[..CP
	; 0000030: 5530 0010 8000 0006_5b83 0b43 5055 3101  U0......[..CPU1.
	; 0000040: 1080 0000 0608 5f53 305f 120a 040a 000a  ......_S0_......
	; 0000050: 000a 000a 0008 5f53 335f 120a 040a 030a  ......_S3_......
	; 0000060: 030a 000a 0008 5f53 345f 120a 040a 040a  ......_S4_......
	; 0000070: 040a 000a 0008 5f53 355f 120a 040a 050a  ......_S5_......
	; 0000080: 050a 000a 005b 805c 4445 4247 010b 8010  .....[.\DEBG....
	;
	; The IASL output for that:
	;  DefinitionBlock ("DSDT.aml", "DSDT", 1, 
	;      "FUJ___", "EF5_____", 0x06040000) {
	;    Scope (\_PR) { Processor (CPU0, 0x00, 0x00008010, 0x06) {}
	;      Processor (CPU1, 0x01, 0x00008010, 0x06) {} }
	;    Name (_S0, Package (0x04) { 0x00, 0x00, 0x00, 0x00 })
	;    Name (_S3, Package (0x04) { 0x03, 0x03, 0x00, 0x00 })
	;    Name (_S4, Package (0x04) { 0x04, 0x04, 0x00, 0x00 })
	;    Name (_S5, Package (0x04) { 0x05, 0x05, 0x00, 0x00 })
	;    OperationRegion (\DEBG, SystemIO, 0x1080, 0x01) ... ... ... }
	; Processor: namestring, idbyte, dword adress, byte length (0 or 6)

	; -------------

fetch_high:				; copy ??? bytes from EAX to thebuffer
	push eax			; ??? is taken from copied_data[4]
	mov [gdt_src+2],ax		; low part
	shr eax,16
	mov [gdt_src+4],al		; mid part
	mov word [gdt_src+5],93h	; data segment
	mov [gdt_src+7],ah		; high part
	xor eax,eax
	mov ax,cs
	shl eax,4
	add eax,thebuffer		; linear buffer address
	mov [gdt_dst+2],ax		; low part
	shr eax,16
	mov [gdt_dst+4],al		; mid part
	mov word [gdt_dst+5],93h	; data segment
	mov [gdt_dst+7],ah		; high part
	push cx
	push si
	mov cx,16	; FIRST, we only fetch the header
	mov word [gdt_src],cx	; in theory, cx+cx-1, but with emm386,
	mov word [gdt_dst],cx	; we need one more (cx+cx) here!?
	shr cx,1			; copy BUFSIZE/2 words
	mov si,gdt			; structure offset (in ES)
	mov ah,87h			; high memory copy
	int 15h				; misc BIOS services
	jc toobig_fetch	; chains to failed_fetch
	;
	mov cx,[thebuffer+4]	; read table size from header
	inc cx	; SECOND step: we fetch ALL the data (round up to words)
	jz toobig_fetch
	and cx,0fffeh
	xor ax,ax
	cmp ax,[thebuffer+6]
	jz acceptable_fetch
toobig_fetch:
	stc
	jmp short failed_fetch
acceptable_fetch:
	mov ax,cx
	add ax,thebuffer
	jc toobig_fetch
	add ax,1024		; keep some stack space untouched
	jc toobig_fetch
	cmp sp,ax		; SP marks end of useable memory
	jb toobig_fetch
	;
	mov word [gdt_src],cx	; in theory, cx+cx-1, but with emm386,
	mov word [gdt_dst],cx	; we need one more (cx+cx) here!?
	shr cx,1			; copy BUFSIZE/2 words
	mov si,gdt			; structure offset (in ES)
	mov ah,87h			; high memory copy
	int 15h				; misc BIOS services
	jc failed_fetch

okay_fetch:
	test byte [dumpacpi],1	; dump mode active?
	jz dump_done
	mov eax,[thebuffer]	; start dump: show initial tag string
dump_tag:
	call showtty
	shr eax,8
	or al,al
	jnz dump_tag
				; dump incl tag and size bytes follows
	mov cx,[thebuffer+4]	; size (we ignore the upper 16 bits)
	mov si,thebuffer	; offset
dump_fetch:			; hexdump the whole fetched data
	mov ax,si
	sub ax,thebuffer	; offset
	test al,15		; multiple of 16?
	jnz dump_mid
	call crlf		; if yes, show CR, LF, address
	call showaxFull		; the address
	mov al,':'
	call showtty
	mov al,' '
	call showtty
dump_mid:
	mov al,[si]
	call showalFull		; a data byte
	mov al,' '
	call showtty
	inc si
	loop dump_fetch
	call crlf		; add CR LF at the end
dump_done:
	clc

failed_fetch:
	pop si
	pop cx
	pop eax
	ret				; returns CY set if error

	; -------------

	; *** useful constants taken from FACP ***
facpptr	dd 0				; location of FACP itself
facsptr	dd 0				; not really used for FDAPM
dsdtptr	dd 0	; for S1/S5 prep	; has to be parsed for poweroff
ssdtptr	dd 0	; for S1/S5 prep	; optional extension for DSDT
%if CURIOUS_ACPI
smi_irq	dw 0				; not yet used
smi_io	dw 0				; not yet used
smi_flg	dw 0				; not yet used
%endif
	; NOTE: 1b variable must be stored right after 1a one for each base!
pm1aSts	dw 0				; PowerMgmt 1a event port base: status
pm1bSts dw 0				; PowerMgmt 1b event port base: status
pm1aEn	dw 0				; second register: enable
pm1bEn	dw 0				; second register: enable
pm1aCnt	dw 0	; for S1/S5...		; PowerMgmt 1a control port base
pm1bCnt	dw 0	; for S1/S5...		; PowerMgmt 1b control port base
thtbits	dw 301h	; for throttle		; CPU duty cycle: low offs, high size
pblk_io	dw 0	; for throttle		; CPU port base, default pm1aSts + 10h

	; *** useful constants taken from DSDT ***
dsdtsz	dd 0	; size of DSDT (used for "too big" message and status message)
	; s0mode is not used, as wake-from-S1 should mean S0 anyway
s1mode	db -1,-1	; pm1aCnt, pm1bCnt mode number for S1 sleep - SHOULD
	; first put all PCI/AGP devices to sleep, too, as far as supported.
	; s2mode is optional, s3mode is only used if s3 differs from
	; "suspend to RAM by shutting down all devices and go to S1"
	; s4mode is often equal to S4 (suspend to disk) mode...
s5mode	db -1,-1	; pm1aCnt, pm1bCnt mode number for S5 soft-off
	; (at least one of S1...S5 must be possible)
	; DSDT can also override pblk_io

	; -------------

acpimsg		db "ACPI for '"
rsdoem		db "??????' found: "
rsdmodel	db "????????"	; --- (DSDT for '"
; --- dsdtoem		db "??????'), port base $"
		db ", port base 0x$"
sizeDSDTmsg	db ", DSDT size 0x$"
useSSDTmsg	db "Scanning SSDT at 0x$"

dsdtmsg		db " DSDT@$"
facpmsg		db " FACP@$"
facsmsg		db " FACS@$"
pmctrlmsg	db " control@$"
pmstatmsg	db " status@$"

s1s5msg		db "ACPI mode codes from DSDT: S1 (sleep) is "
s1msg		db "-/-, S5 (soft-off) is "
s5msg		db "-/-.",13,10,"$"

dosboxmsg	db "WinNT/2k/XP 'DOS 5.50' box: No ACPI."
		db 13,10,"$"

noS5DSDTmsg	db "ACPI S5 poweroff code not in DSDT, cannot use ACPIOFF."
		db " Check BIOS CMOS setup?",13,10,"$"
noS1DSDTmsg	db "ACPI S1 sleep code not in DSDT, cannot use ACPI freeze."
		db " Check BIOS CMOS setup?",13,10,"$"
noSnDSDTmsg	db "ACPI S1 sleep and S5 poweroff both not defined in DSDT."
		db " Check BIOS CMOS setup?",13,10,"$"
fallbackSSDTmsg	db "ACPI S1 sleep and S5 poweroff both not defined in DSDT."
		db " Trying SSDT instead.",13,10,"$"
noSnSSDTmsg	db "ACPI S1 sleep and S5 poweroff both not defined in SSDT"
		db " either. Check BIOS?",13,10,"$"

thtwarnmsg	db "BIOS throttle info '$"
thtwarnmsg2	db "' rejected, SPEEDn will use '301' instead.",13,10,"$"
thtwarn0msg	db "BIOS said we cannot throttle - will try anyway ;-)"
		db 13,10,"$"

badacpimsg	db "I/O bases must all be 16bit",13,10,"$"
badromsigmsg	db "No ROM header at E000, scanning 64k anyway...",13,10,"$"
noacpimsg	db "No ACPI? Check BIOS CMOS setup.",13,10,"$"

contactmsg	db "Try PCISLEEP or try www.oldskool.org/pc/throttle/"
		db " by Jeff Leyda,",13,10
		db "or contact: mceric at users.sourceforge.net",13,10,"$"

pblk_msg	db "Found processor block for "
pblk_cpu	db "CPU0, port base: 0x$" ; add AX and crlf after this
pblk_2nd	db "Processor block for other CPU(s) ignored",13,10,"$"
pblk_odd	db "Cannot parse ACPI processor block, please report!"
		db 13,10,"$"

dumpacpi	db 0	; set to 1 to hexdump all acpi related data

	; -------------

	align 8
gdt:	dw 0, 0, 0, 0			; NULL descriptor
	dw 0, 0, 0, 0			; for BIOS (GDT alias?)
gdt_src	dw 0, 0, 0, 0			; descriptor for source
gdt_dst	dw 0, 0, 0, 0			; descriptor for destination
	; NOTE: first word should be LIMIT, but some int 15.87 want SIZE!?
	; So we use the value (CX*2) instead of (CX*2)-1 here...
	dw 0, 0, 0, 0			; for BIOS (CS?)
	dw 0, 0, 0, 0			; for BIOS (SS?)

thebuffer:		; label for (8kb) buffer, must be at end of code!



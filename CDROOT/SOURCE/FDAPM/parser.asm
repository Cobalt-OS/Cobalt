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

; This file: The parser. Calls all other parts as appropriate.
; Sets up quitmode and tsrmode as appropriate. Destroys registers.
; Returns CY set if quitmode is 1. Function name: parseOptions.

quitmode	db 0	; 0 means EXIT, 1 means GO TSR
	; *** (TSR means: keep everything up to parser itself) ***
tsrmode		db 0	; 0 means no TSR, 1 means int 2f API needed
	; tsrmode can be 1 while quitmode is 0: use old TSR then.

apmdosmsg	db "If APMDOS slows down any app, use ADV:REG instead."
		db 13,10,"$"
; *** quitmsg	db "Done.",13,10,"$"
tsrmsg		db "Going resident.",13,10,"$"
oldtsrmsg	db "Configured resident FDAPM or POWER driver.",13,10,"$"

parseOptions:	; parse command line and call functions as needed.
	cld
	mov si,81h	; command line buffer (ignore length byte)
lpCase:	mov al,[si]
	cmp al,9	; TAB
	jnz noTab
	mov al,' '	; turn tab into space
	mov [si],al
noTab:	cmp al,'a'
	jb okCase
	cmp al,'z'
	ja okCase
	sub al,'a'-'A'	; make command line upper case
	mov [si],al
okCase:	inc si
	cmp si,0ffh
	ja fullCL
	cmp al,13	; CR / NUL / ... end of line?
	ja lpCase
fullCL:	mov al,0
	dec si		; rewind the inc si
	mov [si],al	; NUL terminate command line
	cmp si,81h	; do not eat more than whole string :-p
	jz noTrail
	cmp byte [si-1],' '	; eat trailing space: "fdapm keyword > file"
	jz fullCL	; note that we already turned tab to space above
noTrail:
	mov [0ffh],al
	;
	mov si,81h	; command line buffer (ignore length byte)
preSkip:		; skip over leading space, slashes, dashes
	lodsb
	cmp al,'-'
	jz preSkip
	cmp al,'/'
	jz preSkip
	cmp al,' '
	jz preSkip
	cmp al,'?'	; treat "?" as help request
	jz thaHelp
;	cmp al,9	; TAB
;	jz preSkip
	cmp al,13	; CR, NUL, etc.
	ja anyWord	; ... else line only contains non-content chars

noWord:			; reached at end of command line. Old style
			; was: show help if no command found.
	call ifINSTALL	; search "COMSPEC=" in environment
	or ax,ax	; found?
	jz noWordConfig	; else assume "INSTALL=FDAPM..." line
	call wxStats	; TSR statistics (if any)
	call wxInfo	; APM statistics (if APM found)
	jmp parserQuit

noWordConfig:
	mov word [cs:IdleHardness],8	; 7..8 is "max"
	call wxDos
	jmp parserQuit

thaHelp:
	call wxHelp	; show help screen if nothing found
	jmp parserQuit

anyWord:
	dec si		; rewind SI after lodsb
	;
	cmp word [si],'SP'	; special "SPEEDn" handling 2/2005
	jnz noSpeedWord
	cmp word [si+2],'EE'
	jnz noSpeedWord
	mov ax,[si+4]
	cmp al,'D'
	jnz noSpeedWord
	cmp byte [si+6],0
	jnz noSpeedWord
	cmp ah,'0'
	jb thaHelp
	cmp ah,'9'
	ja thaHelp
	mov al,ah
	sub al,'0'
	call speedCall	; has argument AL!
	jmp short parserQuit
	;	
noSpeedWord:
	mov cx,si	; remember start
	mov si,keywordlist
tryNextWord:
	mov bx,cx	; (re-)point to word start
	lodsw		; pointer to string for comparison
	mov di,ax
	lodsw		; pointer to processing function
	mov bp,ax
	or di,di
	jz otherWord	; end of list reached
	or bp,bp
	jz otherWord	; end of list reached
strCmp:			; now compare strings at BX and DI
	mov al,[bx]	; USER word
	cmp al,[di]	; LIST word
	jnz tryNextWord	; comparison MISMATCH
	or al,al	; end of string?
	jz wordFound	; comparison MATCH
	inc di
	inc bx
	jmp short strCmp

otherWord:		; 5/2005
	mov dx,syntaxerrormsg
	mov ah,9
	int 21h
	jmp thaHelp
	
wordFound:
	mov dx,wordfoundmsg
	mov ah,9
	int 21h
	mov si,cx	; fetch word again
citeIt:	mov al,[si]
	or al,al	; cite word until NUL reached
	jz cited
	call showtty
	inc si
	jmp short citeIt
cited:	call crlf	; go to next line on stdout
	mov ax,processedReturn
	push ax		; where to RETURN from CALL
	push bp		; where to START with CALL
	ret		; CALL the processing function!

processedReturn:	; DONE with all (only 1 word checked)
	mov al,[ds:tsrmode]
	or al,al		; TSR involved?
	jz noTSRused
	call statusPOWER	; *** show status of the TSR
noTSRused:
parserQuit:			; *** must always check for TSR mode!
	mov al,[ds:quitmode]
	or al,al
	jz parserQuit2
	stc
	ret

parserQuit2:
; ***	mov dx,quitmsg
; ***	mov ah,9
; ***	int 21h
	clc
	ret

; -------------

	; Set CPU speed to n/8 of maximum (0 freezes until PwrBtn pressed)
	; 9 just shows the current speed (new 5/2005)
speedCall:			; takes an argument in AL. Func new 2/2005.
	mov ah,0
	mov [cpuspeed],al
	add byte [cpuspeed],'0'
	or al,al
	jnz notStopping
	mov ah,9
	mov dx,cpustopmsg
	int 21h
	call flushCaches	; ... and stop floppy
	xor ax,ax
notStopping:
	push ax
	call throttle		; set speed AX, return previous speed in AX
	pushf			; (!)
	add al,'0'
	mov [cpuoldspeed],al
	popf			; (!)
	pop ax
	pushf
	jc speedFail
	cmp ax,9		; speed 9 never sets speed, just reads it
	jz speedRead
	or ax,ax		; stop okay, or rather speed okay?
	jz stopOkay		; "stop worked" is obvious - no message
	mov ah,9
	mov dx,cpuspeedmsg
	int 21h
speedDone:
	popf
	ret

stopOkay:
	call fixTime		; adjust timer tick time after CPU stop
	jmp short speedDone

speedRead:
	mov dx,cpuoldspeedmsg
	mov ah,9
	int 21h
	popf
	clc
	ret

speedFail:
	mov dx,cpustopfailmsg
	or ax,ax		; stop failed, or rather speed failed?
	jz stopFail
	mov dx,cpuspeedfailmsg
stopFail:
	mov ah,9
	int 21h
	jmp short speedDone

; -------------

	; SHOW APM INFORMATION
wxInfo:	call statusAPM
	mov ax,[cs:apmversion]
	cmp ax,100h
	jae wxInf1
	mov dx,noapminfomsg
	mov ah,9
	int 21h
wxInf1:	ret

	; SHOW POWER TSR INFORMATION
wxStats:
	call statusPOWER
	ret

%if 0	; UNLOAD TSR (JUST TELL "NOT IMPLEMENTED")
wxUnl:	mov dx,unloadmsg
	mov ah,9
	int 21h
	ret
%endif

	; SHOW HELP MESSAGE
wxHelp:	mov dx,helpmsg
	mov ah,9
	int 21h
	ret

; -------------

	; DISABLE ALL SAVINGS
wxOff:	call connectAPM
	jc wxOffNoAPM
	mov ax,5306h	; CPU BUSY
	int 15h
	call disableAPM
	jmp short wxOffAPM
wxOffNoAPM:
	mov dx,havenoapmmsg
	mov ah,9
	int 21h
wxOffAPM:
	mov ax,5400h
	xor bx,bx
	int 2fh ; POWER install check
	cmp bx,504dh
	jnz wxOffNoPOWER
	mov ax,5401h
	mov bx,0100h	; set (1) to mode "no DOS, no APM" (0)
	int 2fh
	ret
wxOffNoPOWER:
	mov dx,havenopowermsg
	mov ah,9
	int 21h
	ret

	; ENABLE BIOS APM SAVINGS
	; Should a POWER be installed if only BIOS APM requested?
	; Currently, my answer to this question is NO.
wxBios:	call connectAPM
	jc wxBiosNoAPM
; -	mov ax,5305h	; CPU IDLE (until next IRQ)
; -	int 15h
	call enableAPM
	jmp short wxBiosAPM
wxBiosNoAPM:
	mov dx,havenoapmmsg
	mov ah,9
	int 21h
wxBiosAPM:
	mov ax,5400h
	xor bx,bx
	int 2fh ; POWER install check
	cmp bx,504dh
	jnz wxBiosNoPOWER
	mov ax,5401h
	mov bx,0102h	; set (1) to mode "no DOS, but APM" (2)
	int 2fh
	ret
wxBiosNoPOWER:
	mov dx,havenopowermsg
	mov ah,9
	int 21h
	ret

wxZapStats:
	mov ax,5400h
	xor bx,bx
	int 2fh ; POWER install check
	cmp bx,504dh
	jnz wxBiosNoPOWER
	mov ax,5481h
	mov bx,-1	; clear statistics (FDAPM only)
	int 2fh
	mov dx,zapokmsg
	cmp bx,55aah	; confirmed?
	jz zapconfirm
	mov dx,zapfailmsg
zapconfirm:
	mov ah,9
	int 21h
	ret

	; ENABLE DOS AND BIOS APM SAVINGS
	; Should a POWER be installed if only BIOS APM requested?
	; Currently, my answer to this question is NO.
wxDos:	mov dx,apmdosmsg
	mov ah,9
	int 21h
wxDos0:	call connectAPM
	jc wxDosNoAPM
; -	mov ax,5305h	; CPU IDLE (until next IRQ)
; -	int 15h
	call enableAPM
	jmp short wxDosAPM
wxDosNoAPM:
	mov dx,havenoapmmsg
	mov ah,9
	int 21h
wxDosAPM:
	mov ax,5400h
	xor bx,bx
	int 2fh ; POWER install check
	cmp bx,504dh
	jz existingTSR 		; or silent: jz wxDosPOWER
	; *** if no POWER TSR found, create one!
useTSR:	call hookhandlers	; HOOK ALL NEEDED INTERRUPTS!
	jc existingTSR
newTSR:	mov dx,tsrmsg		; new TSR loaded
	mov ah,9
	int 21h
	mov byte [quitmode],1
	jmp short wxDosPOWER
existingTSR:
	mov dx,oldtsrmsg
	mov ah,9
	int 21h
	; ***
wxDosPOWER:
	mov ax,5401h
	mov bx,0103h	; set (1) to mode "DOS and APM" (3)
	int 2fh
	mov ax,5403h	; update resident "hardness" setting
	mov bx,[cs:IdleHardness]	; 1..5 min, 6 reg, 7..8 max
	int 2fh
	ret

; -------------

	; PUT SYSTEM INTO STAND BY MODE
wxStdby:
	mov ax,3	; stand-by
	call shutDownHandler
	jmp short wxNorm

	; PUT SYSTEM INTO SUSPEND MODE
wxSusp2:	; added no-spin-down version (9/2004)
	mov word [cs:spinDownDisks],0xc3f8	; zap spin down disk handler
						; (handler is now 'clc' 'ret')
wxSusp:
	mov ax,4	; suspend
	call shutDownHandler
	jmp short wxNorm

	; fetch ACPI tables and print the data, for debugging (6/2007)
wxACPIInfo:
	call acpidump	; print hex dump of all ACPI tables while reading
	ret

	; spin-down and power off, explicitly using ACPI (2/2005)
wxACPIOff:
	call flushCaches
	call spinDownDisks
	call acpioff
	mov dx,acpiofffailmsg
	mov ah,9
	int 21h
	ret

	; POWER OFF SYSTEM (NEEDS APM 1.1+)
wxPow2:		; added no-spin-down version (9/2004)
	mov word [cs:spinDownDisks],0xc3f8	; zap spin down disk handler
						; (handler is now 'clc' 'ret')
wxPowOff:
	mov ax,5	; power off
	call shutDownHandler
	; (probably not reached)
	; (fall through to wxNorm anyway: MY BIOS offers power off
	; but MY POWER SUPPLY cannot turn itself off!)
	; shutDownHandler itself already goes "SUSPEND" if "OFF" is n/a!
	; jmp short wxNorm

wxNorm:			; system RETURNING from standby / suspend
	mov dx,normalstatemsg
	mov ah,9
	int 21h
	ret

	; just flush caches (added 9/2004)
wxFlush:
	call flushCaches
	ret

	; INT 19 REBOOT (will not work on most systems: the idea
	; is to reload the boot sector / kernel, but if any driver
	; fails notice int 19 as a "unhook interrupts" signal...)
wxHot:
	mov ax,0	; hot reboot
	call shutDownHandler
	ret


	; WARM REBOOT
wxWarm:
	mov ax,1	; warm reboot
	call shutDownHandler
	ret


	; COLD REBOOT
wxCold:
	mov ax,2	; cold reboot
	call shutDownHandler
	ret

	; TURN ON VGA SIGNAL
wxVgaOn:
	mov ax,0	; ENABLE refresh, set screen to on
	jmp short wxVga	; control refresh via EGA / VGA / VBE

	; TURN OFF VGA SIGNAL / SCREEN
wxVgaOff:
	mov ax,1	; DISABLE refresh, set screen to off/suspend
wxVga:	call findEga	; preserves AX, tests for VGA, sets CY if not found
	jc wxNVga
	call dodpms	; control refresh via EGA / VGA / VBE
	ret
wxNVga:	mov dx,needegamsg	; abort if not at least EGA
	mov ah,9
	int 21h
	stc
	ret

; -------------

wxOff2:	
	call compatwarn
	jmp wxOff
wxBios2:
	call compatwarn
	jmp wxBios
wxDos2a:
	mov word [cs:IdleHardness],1	; 1..5 is "min"
	jmp short wxDos2
wxDos2b:
	mov word [cs:IdleHardness],6	; 6 is "reg"
	jmp wxDos0			; or with compatwarn: wxDos2
wxDos2c:
	mov word [cs:IdleHardness],8	; 7..8 is "max"
	jmp short wxDos2
wxDos2:
	call compatwarn
	jmp wxDos0

	; tell that we are doing things only for MS POWER
	; compatibility that we would not normally do!
compatwarn:
	push ds
	push ax
	push dx
	mov ax,cs
	mov ds,ax
	mov dx,compatmsg
	mov ah,9
	int 0x21
	pop dx
	pop ax
	pop ds
	ret

	; analyze the environment: If it looks like we were
	; started by INSTALL=..., trigger going resident in
	; APMDOS mode as default action.
	; FreeDOS kernel has a master_env "PATH=. CONFIG=x"
	; (config only if menu function is in use).
	; We assume that PROMPT or COMSPEC are not set before
	; autoexec is reached.
ifINSTALL:
	mov ax,[cs:0x2c]	; environment segment
	or ax,ax
	jz ifINSx		; none at all
	push ds
	push dx
	push si
	cld
	mov ds,ax
	xor si,si

ifINSl:	lodsw
ifINSz:	dec si			; overlapping scan
	or ax,ax		; end of normal environment?
	jz ifINS0		; end of env. reached
	cmp ax,'CO'		; as in "COMSPEC"
	jnz ifINSl		; else search on

ifINSa:	inc si			; not overlapping
	lodsw
	or ax,ax
	jz ifINS0
	cmp ax,'MS'
	jnz ifINSz		; search on, overlapping
	lodsw
	or ax,ax
	jz ifINS0
	cmp ax,'PE'
	jnz ifINSz		; search on, overlapping
	lodsw
	or ax,ax
	jz ifINS0
	cmp ax,'C='
	jnz ifINSz		; search on, overlapping
ifINS1:	mov ax,-1		; found "COMSPEC="
	jmp short ifINS2

ifINS0:	mov ax,cs
	mov ds,ax
	mov dx,compatINSTmsg
	mov ah,9
	int 0x21
	xor ax,ax		; not found
ifINS2: pop si
	pop dx
	pop ds
ifINSx:	ret

; -------------

keywordlist:
	dw wOff, wxOff, wBios, wxBios, wDos, wxDos
	dw wOffMS, wxOff2, wBiosMS, wxBios2, wDosMS1, wxDos2a
	dw wDosMS2, wxDos2a, wDosMS3, wxDos2b, wDosMS4, wxDos2c
	dw wInfo, wxInfo, wStats, wxStats, wHelp, wxHelp
	dw wStdby, wxStdby, wSusp, wxSusp, wSusp2, wxSusp2
	dw wPowOff, wxPowOff, wPow2, wxPow2, wFlush, wxFlush
	dw wHot, wxHot, wWarm, wxWarm, wCold, wxCold
	dw wVgaOn, wxVgaOn, wVgaOff, wxVgaOff
	dw wSpinUp, spinUpDisks, wSpinDn, spinDownDisks
	dw wZap, wxZapStats, wAOff, wxACPIOff
	dw wAInfo, wxACPIInfo	; new 6/2007
	dw 0,0

wOff	db "APMOFF",0
wOffMS	db "OFF",0
wBios	db "APMBIOS",0
wBiosMS	db "STD",0
wDos	db "APMDOS",0
wDosMS1	db "ADV",0
wDosMS2	db "ADV:MIN",0
wDosMS3	db "ADV:REG",0
wDosMS4	db "ADV:MAX",0

wInfo	db "INFO",0
wStats	db "STATS",0
%if 0
wUnl	db "UNLOAD",0	; not yet implemented, see also wxUnl.
%endif
wHelp	db "HELP",0

wStdby	db "STANDBY",0
wSusp	db "SUSPEND",0
wSusp2	db "PURESUSP",0
wPowOff	db "POWEROFF",0
wPow2	db "PUREOFF",0

wFlush	db "FLUSH",0
wHot	db "HOTBOOT",0
wWarm	db "WARMBOOT",0
wCold	db "COLDBOOT",0

wVgaOn	db "VGAON",0
wVgaOff	db "VGAOFF",0

wSpinUp	db "SPINUP",0
wSpinDn	db "SPINDOWN",0

wZap	db "ZAPSTATS",0
wAOff	db "ACPIOFF",0
wAInfo	db "ACPIDUMP",0

; unloadmsg	db "Removal of FDAPM TSR not yet possible.",13,10,"$"

normalstatemsg	db "Back on..."
		db 13,10,"$"

needegamsg	db "Requires EGA or newer graphics.",13,10,"$"

noapminfomsg	db "No APM information - no APM 1.x BIOS?"
		db 13,10,"$"
havenoapmmsg	db "APM not available, skipping APM setup.",13,10,"$"
havenopowermsg	db "Not yet resident, skipping TSR setup.",13,10,"$"

		; zapstats is new 2/2005
zapokmsg	db "Workload and uptime stats cleared.",13,10,"$"
zapfailmsg	db "Could not clear stats - incompatible TSR?",13,10,"$"

		; 6 new message lines 2/2005
acpiofffailmsg	db "No ACPI power off, use PUREOFF.",13,10,"$"
cpuspeedfailmsg	db "No ACPI CPU speed control - try THROTTLE tool.",13,10,"$"
cpustopmsg	db "Will stop CPU - press power button to continue.",13,10,"$"
cpustopfailmsg	db "Stop failed.",13,10,"$"

cpuspeedmsg	db "CPU speed set to "
cpuspeed	db "?/8 of maximum (until reset).",13,10
cpuoldspeedmsg	db "CPU speed was "
cpuoldspeed	db "?/8 of maximum.",13,10,"$"

wordfoundmsg	db "Performing action: $"

compatmsg	db "POWER compatibility option - use FDAPM HELP",13,10
		db "to learn about new FDAPM syntax.",13,10,"$"

compatINSTmsg	db "No COMSPEC, is this an INSTALL= call?",13,10
		db "Loading APMDOS (default action).",13,10,"$"

syntaxerrormsg	db "Unkown command - ignored.",13,10,"$"

helpmsg	db "Usage: Give FDAPM any one option from the following list.",13,10
	db "Case is irrelevant, - or / are optional. HELP or /? "
	db    "shows this help.",13,10
	db "This is free open source GPL 2 software.",13,10
	; *** VERSION LINE FOLLOWS ***
	db "Written and conceived by Eric Auer. Version: 11 Sep 2009." ; *****
	db 13,10,13,10
	db "INFO / STATS - show information about APM status / APMdos",13,10
	db "APMdos   - keep FDAPM in RAM, save most energy. "
	db    "To stop, select APMbios/APMoff"
	db 13,10
	db "APMbios / APMoff  - turn plain BIOS APM "
	db    "energy saving mode on / off",13,10
	db 13,10
	db "SPINUP / SPINDOWN - wake up IDE disks / flush caches and "
	db    "stop IDE disks",13,10
	db "VGAoff / VGAon    - turn EGA/VGA/VESA DPMS screen off "
	db    "(no auto wake up!) / on",13,10
	db "FLUSH / ZapStats  - flush caches / clear STATS counters",13,10
	db "SPEEDn   - set CPU speed to n/8 of max. (0 halts until "
	db    "'power' pressed).",13,10
	db "ACPIDUMP - show ACPI data tables (to debug SPEEDn, stand-by, "
	db    "power off, etc)",13,10
	db "STANDBY  - flush caches, enter stand-by mode",13,10
	db "SUSPEND  - flush caches, stop (will auto wake up) IDE disks, "
	db    "suspend PC",13,10
	db "POWEROFF - flush caches, stop IDE disks, power off VGA and "
	db    "PC", 13, 10
	db "PURESUSP / PUREOFF - as SUSPEND / POWEROFF but does not "
	db    "stop IDE disks first", 13,10
	db 13,10
	db "COLDboot / WARMboot - do a reboot with / without BIOS "
	db    "tests (POST)",13,10
	db "HOTboot  - try fast int 19h reboot (will often crash!)"
	db 13,10
	db "$"

	; OFF, STD, ADV, ADV:MIN, ADV:REG, ADV:MAX are undocumented
	; options for MS POWER syntax compatibility and to reach the
	; ADV:MIN / ADV:REG modes to avoid int 28h Novell slowdown.

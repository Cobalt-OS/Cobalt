; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <mceric@users.sf.net> 2003-2005.
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

; This is the main file. To assemble the binary:
; nasm -o fdapm.com fdapm.asm
; See: http://nasm.sourceforge.net/
; You may want to compress the binary: upx --8086 fdapm.com
; See: http://upx.sourceforge.net/

	org 100h	; a com file
	jmp parser

; ---------------

; Contains: All interrupt handlers and related things that
; belong to the TSR part of the tool.
; If UNIDLECHECK is %define-d, more handlers are enabled.
; Main configuration variables are in main file:
; dw apmversion, db savingstrat, dw MainStatus,
; db IdleStrategy, dw IdleHardness, dw APMPollFreq.

%include "handlers.asm"

; ---------------

apmversion	dw -1	; only set once: AH.AL = APM version
			; if AH is more than 1, clamp AH.AL to 1.2
savingstrat	db 3	; bits: 1 - sti hlt idling  2 - APM idling
			; FDPAM never disables HLT idling itself.
			; and APM idling is on if APM BIOS found.

MainStatus	db 3	; 0 off 2 std 3 adv
IdleStrategy	db 15	; default "all 4 strategies". Possible
			; values: -1 (not applicable) and 0 to 15.
			; POWER seems to have this stuck to 15.
			; FDAPM checks all bits anyway (5/2005).
IdleHardness	dw 8	; "max" (if below 7, int 28h idling disabled)
			; 1..5 is ADV:MIN, 6 ADV:REG, 7..8 ADV:MAX
APMPollFreq	dw 0	; "APM polling frequency" (usually 0)

maxSavingstrat	db 3	; mask to define maximum allowed savingstrat

eofTSR		db "*** *** *** ***"	; TSR ends here.

; ---------------

; This file: hookhandlers - call to hook interrupt handlers.
; Returns CY set if POWER is already resident.
; To be used together with handlers.asm, of course. Like
; handlers, this is influenced by the UNIDLECHECK define.
; FUTURE extension: removetsr - call to remove existing TSR

%include "hooktsr.asm"

; ---------------

; Contains: DPMS related functions for EGA, VGA and VBE graphics.
; Functions: dodpms, findEga, findVga, vbeSet

%include "dpmsfunc.asm"

; ---------------

; Contains: The parser. Calls all other parts as appropriate.
; Sets up quitmode and tsrmode as appropriate. Destroys registers.
; Returns CY set if quitmode is 1. Function name: parseOptions.

%include "parser.asm"

; ---------------

parser:	; parse command line
	call connectAPM	; try to connect to APM, store version

	; call statusAPM	; display APM status
	; removed statusAPM call - only show on INFO request.
	
	call parseOptions	; returns: CY set if going TSR
				; also RUNS selected commands!
	jnc quitProg

goTSR:	mov dx,eofTSR+15
	mov cl,4
	shr dx,cl
	mov ax,3101h	; go TSR, errorlevel 1
	int 21h
quitProg:
	mov ax,4c00h	; exit, errorlevel 0
	int 21h

; ---------------

; Contains: crlf (write cr lf to stdout),
; hex2dec (convert AX from hex to bcd, with saturation),
; showal / showax (write AL / AX to stdout, in hex, strip leading 0s),
; showalFull / showaxFull (write AL / AX ... do not strip leading 0s),
; showtty (write AL to stdout, as char),
; showlist (write DOS string pointed to by [si+(2*AL)] to stdout).

%include "display.asm"

; ---------------

; Contains: statusAPM (destroys registers)
; Writes APM status to stdout, or nothing if no APM installed.

%include "apmstat.asm"

; ---------------

; Contains: statusPOWER (destroys registers)
; Writes POWER TSR status to stdout, or just tells if no TSR found.

%include "powstat.asm"

; ---------------

; Contains: flushCaches (destroys registers)
; Call to flush all known caches. Writes status to stdout.

%include "flush.asm"

; ---------------

; Contains: connectAPM - call to connect to APM if not already
; done, and return APM version in AH.AL and cs:apmversion ...
; at most, version 1.2 is reported. CY set if no APM found.
; enableAPM - call to enable APM and - if APM 1.1 or better - CPU
; management. AX BX CX destroyed.
; disableAPM - call to disable APM and if enabled CPU management.
; AX BX CX destroyed.

%include "apmfunc.asm"

; ---------------

; Contains: shutDownHandler (destroys registers, CY on error)
; Call with type selection AX. Writes status to stdout.
; 0 hot reboot 1 warm reboot 2 cold reboot
; 3 stand by 4 suspend 5 power off (APM 1.1+ only)
; *** 3/4/5 now (2/2005) call ACPI functions below if no APM found

%include "reboot.asm"

; ---------------

; Contains: fixTime (destroys registers, shows messages)
; Use after suspend, to fix the timer tick time with help of RTC

%include "fixtime.asm"

; ---------------

; ACPI throttle / off functions for FDAPM by Eric Auer 2005
; must be placed LAST in FDAPM binary (uses buffers after code)
; Contains: throttle (destroys regs, shows messages)
; Call with speed selection AX (0 frozen to S1 ... 8 max)
; Contains: acpioff (never returns, just puts system to S5 soft-off)
; Call after appropriate "flush disk caches" functions!

acpistuff:	; label: non-acpi can overwrite things after this point

%include "acpitool.asm"


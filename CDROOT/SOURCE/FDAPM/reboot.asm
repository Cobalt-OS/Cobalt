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

; This file: shutDownHandler (destroys registers, CY on error)
; Call with type selection AX. Writes status to stdout.
; 0 hot reboot 1 warm reboot 2 cold reboot
; 3 stand by 4 suspend 5 power off (APM 1.1+ only)

shutDownHandler:	; various kinds of shutdown (select in AX):
	; 0 hot reboot 1 warm reboot 2 cold reboot
	; 3 stand by 4 suspend 5 power off (APM 1.1+ only)
	; returns carry set on error.
	cmp ax,5
	jbe sdokay
	push ax
	mov dx,sdinvmode
	mov ah,9
	int 21h
	pop ax
	call showax
	stc
	ret

sdokay:	xor bx,bx	; assume APM version 0.0
	cmp al,3	; standby, suspend, off?
	jb sdok2	; otherwise no APM needed
	push ax
	call connectAPM	; need APM for the required feature, so...
	mov bx,ax	; remember version
	pop ax
	jnc sdok2
	xor bx,bx	; APM detect failed
	push ax
	mov dx,sdnotapm	; show warning: cannot use APM suspend/standby/off
	mov ah,9
	int 21h
	pop ax
sdok2: 	cmp al,5	; off?
	jb sdok3
	or bx,bx	; any APM?
	jz sdok3	; if not, then we already displayed a warning
	cmp bx,0101h	; APM 1.1 (or better?)
	jae sdok3
	mov dx,sdnotapmoff
	mov ah,9
	int 21h		; cannot turn off without APM 1.1 or better
	; UPDATE 21 jan 2005: RBIL tells that int 15.5307.1.3 would
	; be APM 1.2+, but on the other hand, int 15.5307 main entry
	; only tells that APM 1.0 does not support .1.3 ... So even
	; APM 1.1 is actually enough for poweroff (discovered by Fox).

sdok3:	mov si,shutdownlist	; show message matching for action code AL
	call showlist
	push bx			; APM version
	push ax
	call flushCaches	; flush all caches first in any case!
	pop ax
	push ax
	cmp al,4
	jb notSpinningDown	; if suspend or off, spin down disks
	call spinDownDisks	; Laptop BIOSes often spin down anyway
%ifdef EXTRAVGAOFF
	call wxVgaOff		; Almost every BIOS turns VGA off anyway
%endif
notSpinningDown:
	pop ax
	pop bx
	add ax,ax
	xchg bx,ax
	jmp near [cs:sdcode+bx]	; AX contains APM version

hotboot:
	int 19h			; reload DOS only
	; if failed, fall through to warmboot

warmboot:
	push ds
	mov ax,40h
	mov ds,ax
	mov word [ds:72h],1234h	; warm boot only
	pop ds
	jmp far [cs:bootpt]

coldboot:
	push ds
	mov ax,40h
	mov ds,ax
	mov word [ds:72h],0000h	; full cold boot
	pop ds
	mov al,0feh		; keyboard reboot
	out 64h,al		; try keyboard...
	jmp far [cs:bootpt]	; ...and normal reboot 

bootpt	dw 0,0ffffh	; boot entry point is FFFF:0000

standby:
	or ax,ax	; any APM?
	jz freezeACPI
	call enableAPM	; needed?
	mov ax,5307h
	mov bx,1	; whole system
	mov cx,1	; standby
	int 15h		; APM call
	mov cx,3
	jmp short recoveryTime

freezeACPI:
	mov ah,9
	mov dx,freezemsg
	int 21h
	xor ax,ax
	call throttle	; new 2/2005: use ACPI freeze if no APM available
	mov cx,1
	jmp short recoveryTime

suspend:
	or ax,ax	; any APM?
	jz freezeACPI	; if no APM, use ACPI freeze instead
	call enableAPM	; needed?
	mov ax,5307h
	mov bx,1	; whole system
	mov cx,2	; suspend
	int 15h		; APM call
%ifdef WARMUPDISKS
	mov cx,6
%else
	mov cx,4
%endif
	; jmp short recoveryTime
	
recoveryTime:
	push cx
%ifdef EXTRAVGAOFF
	call wxVgaOn	; if we turned it OFF, we must turn it back on, too!
%endif
%ifdef WARMUPDISKS
	call spinUpDisks	; let disks spin up
%endif
	mov ah,9
	mov dx,recoveringMsg
	int 21h		; ask the user for patience
	pop cx
	call countBar	; take some time to wake up completely
	mov ah,9
	mov dx,recoveredMsg
	int 21h		; back working
	call fixTime	; *** 1/2005: fix DOS clock after suspend/standby
	clc
	ret

poweroff:
	or ax,ax	; any APM?
	jz poweroffACPI
; ---	cmp ax,0101h	; too old APM?
; ---	jz poweroffACPI	; old APM but new ACPI? Very unlikely...
	push ax		; APM version
	call enableAPM	; needed?
	mov ax,5307h
	mov bx,1	; whole system
	mov cx,3	; power off (APM 1.1 or newer needed)
	int 15h		; APM call
	; should not return, so we chain to suspend...
	mov ah,9
	mov dx,nOffMsg
	int 21h		; tell user that power off did not work
	pop ax		; APM version
	jmp suspend	; flush/spindown/vgaoff already done, only suspend

poweroffACPI:
	call acpioff	; new 2/2005 ... should not return, obviously
	jmp freezeACPI	; if power off failed, try freeze instead.

spinUpDisks:
	mov al,0e1h	; immediate idle (motor on, possibly again)
	mov [cs:spinCommand],al
	mov dx,dskup1	; tell user to wait for disk spin up
	jmp short spinUpDisks2

spinDownDisks:
	mov al,0e0h	; immediate standby (motor off)
	mov [cs:spinCommand],al
	mov ah,9
	mov dx,dskmsg1
	int 21h		; first "wait"
	mov cx,3	; 3 seconds for the last leftovers
	call countBar	; of cache flushing calls...
	mov dx,dskmsg2
	;
spinUpDisks2:
	mov ah,9
	int 21h		; now announce spin down / spin up
	;
	mov dx,1f6h	; primary controller
spinDownLoop:
	mov al,0a0h	; no LBA, master disk
	out dx,al
	inc dx
	call miniWait
	mov al,[cs:spinCommand]	; e0 for standby, e1 for on/idle
	out dx,al
	dec dx
	call miniWait
	mov al,0b0h	; no LBA, slave disk
	out dx,al
	inc dx
	call miniWait
	mov al,[cs:spinCommand]	; e0 for standby, e1 for on/idle
	out dx,al
	dec dx
	sub dx,80h	; 1f6 -> 176 -> done
	cmp dx,176h
	jz spinDownLoop	; repeat with secondary controller
	;
	mov al,[cs:spinCommand]
	cmp al,0e1h	; spinning UP?
	jz spinUpWait
	;
spinDownWait:
	mov cx,5
	call countBar	; give disks time to spin down
	mov ah,9
	mov dx,dskmsg3
	int 21h		; declare spin down finished
	clc
	ret

spinUpWait:
	mov cx,7
	call countBar
	mov ah,9
	mov dx,dskup2
	int 21h
	clc
	ret

		; IDE/ATEA commands: E1 immediate idle (motor on)
		; E6 immediate sleep (motor and logics off)
spinCommand:	; E0 immediate standby (motor off, *easy wakeup*)
	db 0e0h	; e.g. Seagate Barracuda: seek 12W spinup 25W
		; working 9W idle 8W *standby* 1W sleep < 1W
	; commands with arguments / return values:
	; E2 config standby timer E3 config idle timer E5 read state
	; EC read description (receive by doing 200h in 1f0h!)

miniWait:	; wait a moment
	xchg ax,bx
	inc ax
	xchg ax,bx
	dec bx
	ret

countBar:	; wait N seconds (destroys AX CX DX=
	push cx
	mov ah,9
	mov dx,dotmsg
	int 21h
	call getTicks
	add ax,18	; 1 second
	mov dx,ax
	sti		; some int 13, 15, 16, 21 or 2f "flush" call can have
			; accidentally disabled interrupts :-( (fix 1/2007)
countWaiting:		; we do NOT call int 28h / idling while waiting!
	call getTicks
	cmp ax,dx
	jnz countWaiting
	pop cx
	loop countBar
	ret

getTicks:	; get low word of timer tick count into AX
	push ds
	xor ax,ax
	mov ds,ax
	mov ax,[ds:46ch]
	pop ds
	ret

shutdownlist:
	dw hotmsg, warmmsg, coldmsg
	dw stdbmsg, suspmsg, offmsg
sdcode	dw hotboot, warmboot, coldboot
	dw standby, suspend, poweroff

hotmsg	db "Triggering hot int 19h reboot...",13,10,"$"
warmmsg	db "Triggering warm reboot...",13,10,"$"
coldmsg	db "Triggering cold reboot...",13,10,"$"
stdbmsg	db "Putting system into STAND BY mode...",13,10,"$"
suspmsg db "Putting system into SUSPEND mode...",13,10,"$"

offmsg  db "Turning OFF system...",13,10,"$"
nOffMsg	db "OFF failed - using SUSPEND.",13,10,"$"

recoveringMsg	db "Warming up...$"
recoveredMsg	db " Back...",13,10,"$"

	; this should actually measure how far down the disks are!
dskmsg1	db "Giving disk handlers some time$"
dskmsg2	db " Spinning down$"
dskmsg3	db " Disks stopped.",13,10,"$"
dskup1	db "Spinning up disks$"
dskup2	db " Disks on.",13,10,"$"

dotmsg	db ".$"

sdinvmode	db "Invalid shutdown mode: $"
sdnotapmoff	db "APM BIOS too old to power off.",13,10,"$"
		; not trying ACPI here - we FOUND APM, but it is too OLD.
sdnotapm	db "No APM BIOS, trying ACPI...",13,10,"$"
freezemsg	db "Press power button to leave ACPI stop state.",13,10,"$"


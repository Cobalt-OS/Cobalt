;	MBR.ASM
;	Master Boot Record
;	----------------------------------------------------------------------
;	Copyright (c) 1994-98 by Florian Painke (f.painke@gmx.de).

;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the GNU General Public License as published by
;	the Free Software Foundation; either version 2 of the License, or
;	(at your option) any later version.

;	This program is distributed in the hope that it will be useful, but
;	WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;	General Public License for more details.

;	You should have received a copy of the GNU General Public License
;	along with this program; if not, write to
;		Free Software Foundation, Inc.
;		59 Temple Place - Suite 330
;		Boston, MA 02111-1307, USA
;	or visit the GNU Homepage at http://www.gnu.org/.



        ASSUME  cs: _code, ds: _code

        ORG     00000h

        P286

        NOJUMPS

;        CryptSize EQU 01DB4h
; new with 0.9.3:
        CryptSize EQU 019B4h

_code   segment byte public 'CODE'

;- Segmente initialisieren ---------------------------------------------------

        cli                             ;Initialisierung des Stacks
        mov     ax, 00000h
        mov     ss, ax                  ;Stack-Segment 00000h
        mov     sp, 07C00h              ;Stackpointer  07C00h
        sti

        mov     ax, 00060h
        mov     ds, ax                  ;Daten-Segment 00060h

        xor     ax, ax
        mov     es, ax                  ;Extra-Segment 00000h

;- Programm verschieben ------------------------------------------------------

        mov     si, 07600h              ;von 00060h:076000h
        mov     di, 00600h              ;nach 00000h:00600h
        mov     cx, 00100h              ;256 WORDs moven
        cld                             ;Vorw„rts!
        rep     movsw                   ;und kopieren

;		jmp 00060h:00024h		;weiter in der Kopie...
        db      0EAh
        dw      $+4, 00060h

;- MENU.BIN laden ------------------------------------------------------------

;;        mov     al, 010h                ;16 Sektoren laden
;; new with 0.9.3:
;        mov     al, 00Eh                ;14 Sektoren laden
;        mov     bx, 00800h              ;OFS 00800h (ES:)
;;        mov     cx, 00002h              ;Sektor 002h, Zylinder 000h
;        mov     cx, 00004h              ;Sektor 004h, Zylinder 000h
;        mov     dx, 00080h              ;Drive 080h, Head 000h
;        mov     di, offset _msgre       ;Partitionssektor laden Fehlermeldung

; 0.9.3a3: read last 14 secs of first head

; 1.) get #sectors
        mov     di, offset _msgre       ;Partitionssektor laden Fehlermeldung

        mov      ah, 08h
        mov      al, 80h
        xor      cx, cx                 ; cx = 0 in case of failure
        int      13h                    ; => cx = seczyl
        and      cx, 003Fh
        sub      cl, 0Dh                ; cx = startsec of mgr
;        jc       _prerr                ; out of range - ignore?
; 2.) load
        mov     al, 00Eh                ;14 Sektoren laden
        mov     bx, 00800h              ;OFS 00800h (ES:)
        mov     dx, 00080h              ;Drive 080h, Head 000h
        mov     di, offset _msgre       ;Partitionssektor laden Fehlermeldung

        call    _gets

        cmp     word ptr ds: [01BFEh], 0AA55h
        jne     _norml

;- MENU.BIN entschlsseln ----------------------------------------------------

        mov     di, 00200h              ;OFS MENU.COM
        mov     bp, CryptSize           ;Counter

_chk_1: call    _newrn
        mov     bx, 0100h
        mov     cx, dx
        mul     bx
        mov     ax, cx
        mov     cx, dx
        mul     bx
        add     ax, cx
        adc     dx, 0
        mov     ax, dx
        xor     byte ptr [di], al
        inc     di
        dec     bp
        jnz     _chk_1

        push    cs
        push    offset _norml

;		jmp 00080h:00000h		;Sprung nach MENU.BIN
        db      0EAh
        dw      00000h, 00080h

;- Partitionstabelle prfen --------------------------------------------------

_norml: mov     cx, 00004h              ;Partitionstabelle checken (4 Eintr„ge)
        mov     si, 001BEh              ;OFS Partitionstabelle

_plop1: cmp     byte ptr [si], 080h     ;Aktiver Eintrag?
        je      _ldptb                  ;Dann laden...

_plop2: add     si, 010h                ;N„chster Eintrag
        loop    _plop1

        mov     di, offset _msgap       ;keine aktive Partition? Schade...
        jmp     short _prerr

;- BOOT.BIN laden ------------------------------------------------------------

_ldptb: mov     al, 001h                ;1 Sektor lesen
        mov     bx, 07C00h              ;OFS 07C00h (ES:)
        mov     cx, word ptr [si+2]     ;Sektor, Zylinder
        mov     dl, 080h                ;Drive 080h
        mov     dh, byte ptr [si+1]     ;Head
        mov     di, offset _msgre       ;Bootsektor
        call    _gets

        cmp     word ptr ds: [077FEh], 0AA55h ;Betriebsystem gltig?
        je      _jump

        mov     di, offset _msgnb       ;Betriebsystem ungltig
        jmp     short _prerr

_jump:  xor     ax, ax
        mov     ds, ax

        add     si, 00600h              ;SI: OFS aktive Partition
        mov     bp, si                  ;BP: SI

;		jmp 00000h:07C00h		;Sprung nach BOOT.BIN
        db      0EAh
        dw      07C00h, 00000h

;- Fehlermeldung ausgeben ----------------------------------------------------

_prerr: push    di
        mov     al, byte ptr [di]
        cmp     al, 0
        je      short _ende
        mov     ah, 00Eh
        int     010h
        pop     di
        inc     di
        jmp     short _prerr

_ende:  nop
        jmp     short _ende

;- Zufallszahl errechnen -----------------------------------------------------

_newrn  proc    near

        mov     ax, word ptr ds: [01BF4h]
        mov     bx, word ptr ds: [01BF6h]
        mov     cx, ax
        mul     word ptr _rnfct
        shl     cx, 3
        add     ch, cl
        add     dx, cx
        add     dx, bx
        shl     bx, 2
        add     dx, bx
        add     dh, bl
        shl     bx, 5
        add     dh, bl
        add     ax, 1
        adc     dx, 0
        mov     word ptr ds: [01BF4h], ax
        mov     word ptr ds: [01BF6h], dx
        ret

_newrn  endp

;- Sektor laden --------------------------------------------------------------

_gets   proc    near

        mov     bp, 00003h              ;Fehlerz„hler auf 3...

_lop_1: mov     ah, 002h                ;Lesen
        int     013h
        jnc     _lop_e                  ;nix Schiefgegangen? Dann ab die Post...

        mov     ah, 0Dh                 ;Reset des HDD-Controllers
        int     013h
        dec     bp
        jnz     _lop_1                  ;und nocheinmal...
        jmp     _prerr                  ;Ja was? Keine Festplatte drinnen?

_lop_e: ret

_gets   endp

;- Meldungen -----------------------------------------------------------------

INCLUDE MBR.ASI

_rnfct: dw      08405H

_code   ends

        end

;=============================================================================

;Aufbau des Speichers:
;0060:0000 \
;...		> MBR.BIN  512 B  =  1 Sektor
;0060:01FF /
;0060:0200 \
;...		> MENU.BIN	24 KB = 48 Sektoren
;0060:61FF /
;			  Damit errechnet sich der OFS eines Sektors zu
;			  00200h * (Sektor - 1)
;0680:0000 \
;...		> STACK 	 5 KB
;0680:13FF /
;0060:7600 \
;...		> BOOT.BIN 512 B  =  1 Sektor
;0060:77FF /

;Registerzuordnung:
;CS Code 00060h
;DS Data 00060h
;ES Mem  00000h
;SS Stck 00680h

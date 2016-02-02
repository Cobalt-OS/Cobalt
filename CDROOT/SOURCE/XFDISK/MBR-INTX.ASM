Version mit Untersttzung der Int13-eXtensions und Bootmanagerkodierung
Problem: ist zu groá fr MBR

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

        CryptSize EQU 01DB4h

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

        mov     al, 010h                ;16 Sektoren laden
        mov     bx, 00800h              ;OFS 00800h (ES:)
        mov     cx, 00002h              ;Sektor 002h, Zylinder 000h
        mov     dx, 00080h              ;Drive 080h, Head 000h
        mov     di, offset _msgre       ;Partitionssektor laden Fehlermeldung
        call    _gets

        cmp     word ptr ds: [01FFEh], 0AA55h
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

_ldptb:
; UM: Erweiterung zur Unterstuetzung der INT 13 Extensions:
;     erstmal pruefen, ob BIOS INT 13 Extensions unterstuetzt

            mov     ah, 041h
            mov     bx, 055AAh
            mov     dl, 080h                ;Drive 080h
            int     13h
            jc      _noint13x
            cmp     bx, 0AA55h
            jne     _noint13x
            and     cx, 1                   ; bit 0: read/write/support
            jc      _noint13x

; UM: jetzt pruefen, ob das Bootlaufwerk unterstuetzt wird:

            mov     ah, 048h
            mov     dl, 080h                ;Drive 080h
            push    si
            mov     si, offset i13x_param
            int     13h
            pop     si
            jc      _noint13x

; UM: jetzt Bootsektor mit INT 13 Extensions laden:
            mov     di, 00003h              ;3 Versuche

            mov     bx, word ptr [si+008h]  ;Distance low word
            mov     i13x_struct+08h, bx

            mov     bx, word ptr [si+00Ah]  ;Distance high word
            mov     i13x_struct+0Ah, bx

_botrd_x:   mov     dl, 080h                ;Drive 080h
            push    si
            mov     si, offset i13x_struct

            mov     ah, 42h                 ;Sektor lesen
            int     013h
            pop     si
            jnc     _got_bootsec
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _botrd_x

            mov     di, offset _msgre       ;Bootsektor
            jmp     _prerr                  ;Ja was? Keine Festplatte drinnen?

; UM: keine INT 13 Extensions, also weiter im Originalcode:
_noint13x:  mov     al, 001h                ;1 Sektor lesen
        mov     bx, 07C00h              ;OFS 07C00h (ES:)
        mov     cx, word ptr [si+2]     ;Sektor, Zylinder
        mov     dl, 080h                ;Drive 080h
        mov     dh, byte ptr [si+1]     ;Head
        mov     di, offset _msgre       ;Bootsektor
        call    _gets

_got_bootsec:
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

        mov     ax, word ptr ds: [01FF4h]
        mov     bx, word ptr ds: [01FF6h]
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
        mov     word ptr ds: [01FF4h], ax
        mov     word ptr ds: [01FF6h], dx
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

;- Variablen -----------------------------------------------------------------
; UM: Speicherstruktur zur Unterstuetzung der INT 13 Extensions:

i13x_param  dw      1Ah                     ; Record Size
            dw      ?                       ; Flags
            dd      ?                       ; Cylinders
            dd      ?                       ; Heads/Cylinder
            dd      ?                       ; Sectors/Track
            dd      ?, ?                    ; Total Disk Size in Sectors
            dw      ?                       ; Bytes/Sector
i13x_struct dw      10h                     ; Record Size
            dw      01h                     ; Number of Sectors
            dw      7C00h, 0                ; Pointer to the Buffer
            dw      ?                       ; LBA Address of the first Sector
            dw      ?                       ; LBA Address of the first Sector
            dd      0                       ; LBA Address of the first Sector

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

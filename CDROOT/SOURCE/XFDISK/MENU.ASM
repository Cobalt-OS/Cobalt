;   eXtended FDisk I
;   ----------------------------------------------------------------------
;   Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

;   MENU.ASM
;   Boot Manager Menu

;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 2 of the License, or
;   (at your option) any later version.

;   This program is distributed in the hope that it will be useful, but
;   WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;   General Public License for more details.

;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to
;       Free Software Foundation, Inc.
;       59 Temple Place - Suite 330
;       Boston, MA 02111-1307, USA
;   or visit the GNU Homepage at http://www.gnu.org/.



            ASSUME  cs: _code, ds: _code

            ORG     00000h

            P286

            NOJUMPS

;            CheckSize EQU 01DB4h
            CheckSize EQU 019B4h

; 01Dxx -> 019xx, 01Cxx -> 01Bxx

_code       segment byte public 'CODE'

            jmp     _bment

msgcpr      db      'eXtended FDisk I 0.9.3 Beta ', LANG, 13, 10
            db      'written by Florian Painke & Ulrich MÅller', 0

;- Segmente initialisieren ---------------------------------------------------

_bment:     mov     ax, 00080h
            mov     ds, ax                  ;Daten-Segment 00080h

;- Checksumme berechnen ------------------------------------------------------

            xor     bx, bx                  ;Offset
            mov     cx, CheckSize           ;Counter
            xor     si, si
            xor     di, di

_chkl1:     mov     al, byte ptr [bx]       ;Byte laden ab OFS 00000h
            xor     ah, ah
            xor     dx, dx
            inc     bx
            imul    bx
            add     si, ax
            adc     di, dx

            test    si, 00001h              ;Gerade oder Ungerade?
            jz      _chkl2

            xor     si, 01F86h
            xor     di, 012A0h
            jmp     short _chkl3

_chkl2:     xor     si, 01AE9h
            xor     di, 0041Ah

_chkl3:     loop    _chkl1

            cmp     word ptr ds: [019F8h], si
            jne     _retrn
            cmp     word ptr ds: [019FAh], di
            jne     _retrn
            jmp     short _rfchk

_retrn:     mov     bx, offset msginv
            call    _print
            jmp     _errlp

;- Anzahl der Entries PrÅfen -------------------------------------------------

_rfchk:     mov     si, 01800h              ;OFS Entries
            cmp     word ptr [si+01Eh], 0AA55h ;mindestens ein Eintrag vorhanden?
            je      _rfend                  ;dann weiter

            mov     bx, offset msgnoen
            call    _print

            mov     si, offset _retfp       ;Funktion kopieren
            mov     di, 00800h              ;nach 00800h (ES:)
            mov     cx, offset _rfend - offset _retfp ;LÑnge berechnen
            cld                             ;VorwÑrts!
            rep     movsb

;       jmp 00080h:00000h
            db      0EAh
            dw      00000h, 00080h

_retfp:     mov     di, 00800h              ;OFS BootmenÅ berechnen
            add     di, offset _rfend - offset _retfp
            mov     cx, 03A00h              ;LEN BootmenÅ berechnen
            sub     cx, offset _rfend - offset _retfp
            xor     ax, ax
            cld                             ;VorwÑrts!
            rep     stosw                   ;Lîschen!

            mov     ax, 00060h
            mov     ds, ax

            retf

_rfend:     pop     si                      ;pop retf Adredresse vom Stack
            pop     si

;- Test auf MONO / COLOR -----------------------------------------------------

_mono:      mov     si, 00400h              ;BIOS Variablen
            cmp     word ptr es: [si+063h], 003D4h ;CRTPort: COLOR-Karte?
            jne     _mastr
            mov     word ptr vidseg[002h], 0B800h ;Segment f. COLOR-Karte
            mov     byte ptr wincol, 017h   ;Fensterfarbe hellgrau auf blau

;- Masterpasswort Abfrage ----------------------------------------------------

_mastr:     mov     bx, 01900h              ;OFS MasterPWD
            call    _passw                  ;Passwortabfrage

;- Bildschirmaufbau ----------------------------------------------------------

_scrn:      mov     si, 00400h              ;BIOS Variablen
            mov     ax, word ptr es: [si+060h] ;Cursorgrî·e
            mov     word ptr cursiz, ax     ;sichern
            mov     ax, word ptr es: [si+050h] ;Cursorposition
            mov     word ptr curpos, ax     ;sichern
            call    _ssave

            cmp     byte ptr ds: [019EFh], 01h ;OFS ClearScrn = TRUE?
            jne     _scr_0
            call    _sclr                   ;Bildschirm lîschen

_scr_0:     mov     ah, 001h                ;Funktion Cursorgrî·e setzen
            mov     cx, 02000h              ;Cursor lîschen
            int     010h

            mov     dx, 01700h              ;Zeile 23 / Spalte 0
            call    _goto
            mov     bx, offset msgcpr       ;Copyright Message
            call    _print

;- Fenster zeichnen ----------------------------------------------------------

            mov     cx, 0000Fh              ;15 Zeilen
_scr_1:     push    cx
            mov     dx, 0040Dh              ;Anfang Zeile 18 / Spalte 13
            add     dh, cl                  ;cx - Zeilennummer addieren
            call    _goto
            mov     ax, 00920h              ;Funktion Zeichen/Farbe ausgeben
            mov     bl, byte ptr wincol     ;Farbe
            mov     cx, 00036h              ;54 Spalten
            int     010h
            pop     cx
            loop    _scr_1

            mov     bx, offset winln1       ;1. Zeile ausgeben
            call    _print

            inc     dh                      ;Zeile 5 / Spalte 14
            call    _goto
            mov     bx, offset winln2       ;2. Zeile ausgeben
            cmp     byte ptr ds: [019EAh], 01h ;SimpleMenus?
            jne     _scr_ml
            mov     bx, offset winln2s
_scr_ml:    call    _print

            inc     dh                      ;Zeile 6 / Spalte 14
            call    _goto
            mov     bx, offset winln3       ;3. Zeile ausgeben
            call    _print

            mov     cx, 008h                ;8 Zeilen schreiben
_scr_2:     inc     dh                      ;Zeile 6 - 15 / Spalte 14
            call    _goto
            mov     bx, offset winln4       ;4. - 11. Zeile ausgeben
            call    _print
            loop    _scr_2

            inc     dh                      ;Zeile 16 / Spalte 14
            call    _goto
            mov     bx, offset winln3       ;12. Zeile ausgeben
            call    _print

            inc     dh                      ;Zeile 17 / Spalte 14
            call    _goto
            mov     bx, offset winln5       ;13. Zeile ausgeben
            call    _print

            inc     dh                      ;Zeile 18 / Spalte 14
            call    _goto
            mov     bx, offset winln4       ;14. Zeile ausgeben
            call    _print

            inc     dh                      ;Zeile 19 / Spalte 14
            call    _goto
            mov     bx, offset winln7       ;15. Zeile ausgeben
            call    _print

;- MenÅsystem ----------------------------------------------------------------

_menu:      mov     cx, 00001h
            call    _beep
            xor     dx, dx

            mov     byte ptr ds: [019E8h], 00h ;default: do no boot from floppy (F2)

_shwm1:     call    _entry
            cmp     al, 000h                ;Entry Empty?
            je      _shwm2
            inc     dh
            cmp     dh, byte ptr maxent     ;Maximale Anzahl erreicht?
            je      _shwm2
            jmp     short _shwm1

_shwm2:     xor     ax, ax                  ;ah Aktuell, al Alt
            cmp     byte ptr ds: [019EBh], 01h ;Boot Last?
            jne     _shwm3
            mov     al, byte ptr ds: [019ECh] ;Last Entry
            mov     ah, al

_shwm3:

; 000814-UM
; Alternatives Bootverhalten: um zeitabhÑngig zwischen zwei Partitionen
; zu wÑhlen, kann wie folgt verfahren werden:
; die Partitionen im Bootmanager als 1. und 2. BootmenÅeintrag wÑhlen,
; folgenden Code bis incl. ";boot_x:" ent-kommentieren:

;            push    ax
;            push    cx
;            push    dx

;; reserves bx, si, di, bp + segmentregister
;            mov     ah, 02h
;            int     1Ah

;; returns:  ch : hour (BCD)
;;           cl : minutes (BCD)
;;           dh : seconds (BCD)
;;           dl : daylight savings flag (00h standard time, 01h daylight time)
;;           BCD (Binary Coded Decimal): Kodierung von zweistelligen Dezimal-
;;               zahlen. Die Werte 0 bis 9 sind jeweils mit 4 Bit kodiert
;; Beispiel: zwischen 07 und 19 Uhr Partition 1 booten, sonst Partition 2
;
;            pop     dx
;
;            cmp     ch, 07h
;            jb      _boot_2
;            cmp     ch, 19h
;            jae     _boot_2
;
;            pop     cx
;            pop     ax
;            mov     ah, 00h
;            jmp     _boot_x
;
;_boot_2:    pop     cx
;            pop     ax
;            mov     ah, 01h
;
;_boot_x:

;_shwm3:
            dec     dh                      ;dh wieder - 1!
            xor     cl, cl                  ;cl min
            mov     ch, dh                  ;ch max

            mov     dh, ah                  ;Anzeige Aktuell
            mov     dl, 001h                ;Markiert
            push    ax
            call    _entry
            pop     ax

_shwto:     mov     bx, word ptr ds: [019FCh] ;OFS Timeout
            mov     word ptr outtim, bx     ;Timeout Zeit
            cmp     bx, 00000h              ;Timeout inaktiv?
            mov     byte ptr tflag2, 001h   ;Timeout vorsichtshalber aktiv setzen
            jne     _sto_1
            mov     byte ptr tflag2, 002h   ;Timeout inaktiv.
_sto_1:     call    _instt                  ;Timer installieren
            call    _timef                  ;Timeout Zeile schreiben

_men_1:     push    ax
            call    _dotim
            mov     ah, 001h                ;Zeichen vorhanden?
            int     016h
            jnz     _men1a
            jmp     _men_5                  ;nein, dann mal sehen ob timeout

_men1a:     xor     ah, ah                  ;Zeichen auslesen...
            int     016h

            cmp     byte ptr ishidn, 001h   ;Ist das MenÅ versteckt?
            je      _menf5                  ;Wir reagieren nur auf F5!

_menes:     cmp     al, 01Bh                ;ESC gedrÅckt?
            jne     _men_3
            cmp     byte ptr tflag2, 000h   ;Timeout deaktiviert?
            jne     _men_2
            mov     byte ptr tflag2, 001h   ;aktivieren
            mov     bx, word ptr ds: [019FCh] ;OFS Timeout
            mov     word ptr outtim, bx     ;Timeout Zeit
            call    _timef                  ;Zeit ausgeben
            call    _instt                  ;Zeit zurÅcksetzen
            jmp     _men_6
_men_2:     cmp     byte ptr tflag2, 002h   ;Timeout inaktiv?
            je      _men_3
            mov     byte ptr tflag2, 000h   ;Timeout deaktivieren
            call    _timef
            jmp     _men_6

_men_3:     cmp     al, 00Dh                ;Return gedrÅckt?
            jne     _men_4
            jmp     _loadf                  ;dann Bootsektor laden...

_men_4:     cmp     al, 000h                ;Sondertaste gedrÅckt?
            je      _menup
            jmp     _men_5

_menup:     cmp     ah, 048h                ;UP Taste?
            jne     _mendn
            pop     ax
            push    ax
            cmp     ah, cl                  ;Aktiv bereits minimum?
;            je      _men_5
            jne     _menup_2
            jmp     _men_5
_menup_2:
            pop     ax
            dec     ah
            push    ax

            call    _hndlt
;            jmp     short _men_5
            jmp     _men_5

_mendn:     cmp     ah, 050h                ;DOWN Taste?
            jne     _menf5
            pop     ax
            push    ax
            cmp     ah, ch
            je      _men_5                  ;Aktiv bereits maximum?
            pop     ax
            inc     ah
            push    ax

            call    _hndlt
            jmp     short _men_5

_menf5:     cmp     ax, 03F00h              ;F5?
            jne     _menf1

            cmp     byte ptr ishidn, 001h   ;schon versteckelt?
            jne     _mf5_1

            mov     word ptr savseg, 03800h
            call    _srest                  ;Bildschirm wiederherstellen
            mov     word ptr savseg, 02800h
            mov     byte ptr ishidn, 000h
            cmp     byte ptr tflag2, 003h   ;Timeout vorÅbergehend deaktiviert?
            jne     _men_6                  ;sonst kÅmmert uns das Handling nicht
            mov     byte ptr tflag2, 001h   ;dann wieder aktivieren...
            call    _hndlt
            jmp     short _men_5

_menf1:     cmp     ah, 03Bh                ;F1?
            jne     _menf2
            pop     ax
            push    0FF00h                  ;Diskettenlaufwerk 0
            jmp     _loadf

_menf2:     cmp     ah, 03Ch                ;F2?
            jne     _men_5
            mov     byte ptr ds: [019E8h], 01h ;floppy boot (F2)
            jmp     _loadf

_mf5_1:     mov     word ptr savseg, 03800h
            call    _ssave                  ;Bildschirm sichern
            mov     word ptr savseg, 02800h
            call    _srest                  ;Bildschirm wiederherstellen
            mov     byte ptr ishidn, 001h   ;Versteckt
            cmp     byte ptr tflag2, 001h   ;Timeout aktiv?
            jne     _men_5
            mov     byte ptr tflag2, 003h   ;Timeout vorÅbergehend deaktivieren
            jmp     short _men_6

_men_5:     cmp     byte ptr tflag2, 001h   ;Timout aktiv?
            jne     _men_6
            cmp     byte ptr tflag1, 000h   ;Timeout eingetreten?
            je      _men_6
            mov     byte ptr tflag1, 000h   ;Flag wieder lîschen...
            dec     word ptr outtim
            cmp     word ptr outtim, 000h   ;Zeit abgelaufen?
            je      _loadf
            call    _instt                  ;Timer installieren
            cmp     byte ptr ishidn, 001h   ;Versteckelt?
            je      _men_6
            call    _timef                  ;Timeout Zeile schreiben

_men_6:     pop     ax
            cmp     ah, al                  ;Position geÑndert?
            je      _men_e
            mov     dh, al                  ;Anzeige Alt
            xor     dl, dl                  ;nicht selektiert
            push    ax
            call    _entry
            pop     ax
            mov     dh, ah                  ;Anzeige Neu
            mov     dl, 001h                ;Selektiert
            push    ax
            call    _entry
            pop     ax
            mov     al, ah                  ;Alt = Neu!
            call    _bipp
_men_e:     jmp     _men_1

;- Bildschirm wiederherstellen -----------------------------------------------

_loadf:     call    _srest
            mov     dx, word ptr curpos     ;Cursor positionieren
            call    _goto
            mov     ah, 001h                ;Funktion Cursorgrî·e setzen
            mov     cx, word ptr cursiz     ;Cursor wiederherstellen
            int     010h

;- BootLoader ---------------------------------------------------------------

            pop     ax                      ;InfoEntry nummer
            xor     bp, bp                  ;OFS Bootpartition lîschen.

            cmp     ah, 0FFh                ;Sondercode?
            jne     _savle
            jmp     _diskb

_savle:     cmp     byte ptr ds: [019EBh], 01h ;Boot Last?
            jne     _lbs_1
            push    ax

            mov     di, 00003h              ;3 Versuche
_lodcs:     mov     ax, 00201h              ;1 Sektor lesen
            mov     dl, 080h                ;Drive
            xor     dh, dh                  ;Head 0
            mov     cx, 00010h              ;Cylinder 0, Sektor 16
            mov     bx, 07C00h              ;OFS Partitionssektor (ES:)
            int     013h
            jnc     _confl
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _lodcs
            jmp     _err_1

_confl:     pop     ax
            push    ax
            cmp     byte ptr es: [07DECh], ah
            je      _confe
            mov     byte ptr es: [07DECh], ah

            mov     di, 00003h              ;3 Versuche
_wrtcs:     mov     ax, 00301h              ;1 Sektor schreiben
            mov     dl, 080h                ;Drive
            xor     dh, dh                  ;Head 0
            mov     cx, 00010h              ;Cylinder 0, Sektor 16
            mov     bx, 07C00h              ;OFS Partitionssektor (ES:)
            int     013h
            jnc     _confe
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _wrtcs
            jmp     _err_2

_confe:     pop     ax

_lbs_1:     xor     al, al
            xchg    al, ah
            push    ax

            shl     ax, 005h                ;Entry * 32
            mov     si, 01800h              ;OFS InfoEntries
            add     si, ax

            mov     bx, si
            add     bx, 00Dh                ;Name der Partition
            call    _print                  ;ausgeben
            mov     bx, offset msgbot
            call    _print

            pop     ax
            mov     dl, 012h
            mul     dl                      ;Entry * 18
            mov     bx, ax
            add     bx, 01924h              ;OFS PWDEntries
            call    _passw                  ;Passwortabfrage

            cmp     byte ptr [si+006h], 000h ;PRI/EXT?
            jne     _lbs_2

            jmp     _lodbs

_lbs_2:     mov     al, byte ptr [si+005h]  ;Systemkennung
            mov     bx, offset fstbl1       ;Anpassungstabelle
            call    _fschk                  ;PrÅfen
            or      al, al
            jnz     _lodps
            jmp     _actpe

;- Partitionssektor anpassen -------------------------------------------------

;UM Kommentar: Partitionssektor laden und Typ anpassen
_lodps:     mov     di, 00003h              ;3 Versuche
_hddr1:     mov     ax, 00201h              ;1 Sektor lesen
            mov     dl, byte ptr [si]       ;Drive
            xor     dh, dh                  ;Head 0
            mov     cx, 00001h              ;Cylinder 0, Sektor 1
            mov     bx, 07C00h              ;OFS Partitionssektor (ES:)
            int     013h
            jnc     _conff
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _hddr1
            jmp     _err_1

_conff:     mov     di, 075BEh              ;OFS Partitionstabelle
            mov     cx, 00004h              ;4 EintrÑge checken
_cfg_1:     mov     bl, byte ptr [di+004h]  ;Partitionstyp
            cmp     bl, 001h                ;FAT12
            je      _cfg_2
            cmp     bl, 004h                ;FAT16 <32MB
            je      _cfg_2
            cmp     bl, 006h                ;FAT16 >32MB
            je      _cfg_2
            cmp     bl, 007h                ;HPFS
            je      _cfg_2
            cmp     bl, 00Bh                ;FAT32
            je      _cfg_2
            cmp     bl, 00Ch                ;FAT32 INT13X
            je      _cfg_2
            cmp     bl, 00Eh                ;FAT16 INT13X
            jne     _cfg_3
_cfg_2:     cmp     byte ptr ds: [019E9h], 01h ;Autohide?
            jne     _cfg_3
            add     byte ptr [di+004h], 010h
_cfg_3:     add     di, 010h                ;nÑchster Eintrag...
            loop    _cfg_1

            mov     di, 075BEh
; added UM: search partition entry to boot from
            mov     cx, 00004h              ;4 EintrÑge checken
_fnd_e1:    mov     bx, word ptr [di+008h]  ;Distanze (low)
            cmp     bx, word ptr [si+009h]  ;Distanze (low)
            jne     _fnd_e2

            mov     bx, word ptr [di+00Ah]  ;Distanze (high)
            cmp     bx, word ptr [si+00Bh]  ;Distanze (high)
            jne     _fnd_e2

            jmp     _fnd_entry              ;Eintrag gefunden

_fnd_e2:    add     di, 010h                ;nÑchster Eintrag...
            loop    _fnd_e1

            ;Eintrag aus BootmenÅ steht nicht im Partitionssektor!
            mov     bx, offset msgnbs
            call    _print
            jmp     _scrn

;            mov     cl, byte ptr [si+006h]  ;Eintrag in die Partitionstabelle
;            ; -> cl = Nummer der zu bootenden Partition [1..4]
;
;            xor     ch, ch
;            dec     cl
;            shl     cx, 004h                ;OFS in die Tabelle berechnen
;            add     di, cx

; UM: weiter im Original, di zeigt nun auf richten Eintrag...
_fnd_entry: mov     bl, byte ptr [di+004h]  ;Partitionstyp
            cmp     bl, 011h                ;FAT12
            je      _cfg_4
            cmp     bl, 014h                ;FAT16 <32MB
            je      _cfg_4
            cmp     bl, 016h                ;FAT16 >32MB
            je      _cfg_4
            cmp     bl, 017h                ;HPFS
            je      _cfg_4
            cmp     bl, 01Bh                ;FAT32
            je      _cfg_4
            cmp     bl, 01Ch                ;FAT32 INT13X
            je      _cfg_4
            cmp     bl, 01Eh                ;FAT16 INT13X
            jne     _actpj
_cfg_4:     sub     byte ptr [di+004h], 010h
            jmp     short _actpj

;UM Kommentar: Partitionssektor laden und Typ nicht anpassen
_actpe:     mov     di, 00003h              ;3 Versuche
_hddr2:     mov     ax, 00201h              ;1 Sektor lesen
            mov     dl, byte ptr [si]       ;Drive
            xor     dh, dh                  ;Head 0
            mov     cx, 00001h              ;Cylinder 0, Sektor 1
            mov     bx, 07C00h              ;OFS Partitionssektor (ES:)
            int     013h
            jnc     _actpj
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _hddr2
            jmp     _err_1

_actpj:     mov     di, 075EEh              ;OFS letzter Eintrag PTable
            mov     cx, 00004h              ;4 EintrÑge checken
_actlp:     mov     byte ptr [di], 000h     ;Partition deaktivieren.

;UM: activate boot partition
            ;cmp     byte ptr [si+006h], cl
            mov     bx, word ptr [di+008h]  ;Distanze (low)
            cmp     bx, word ptr [si+009h]  ;Distanze (low)
            jne     _actl1
            mov     bx, word ptr [di+00Ah]  ;Distanze (high)
            cmp     bx, word ptr [si+00Bh]  ;Distanze (high)
            jne     _actl1
            mov     byte ptr [di], 080h     ;Partition aktivieren.
            cmp     byte ptr [si], 080h     ;HDD 0?
            jne     _actl1
            mov     bp, cx
            dec     bp
            shl     bp, 004h
            add     bp, 007BEh
_actl1:     sub     di, 10h
            loop    _actlp

_wrtps:     mov     di, 00003h              ;3 Versuche
_hddwr:     mov     ax, 00301h              ;1 Sektor schreiben
            mov     dl, byte ptr [si]       ;Drive
            xor     dh, dh                  ;Head 0
            mov     cx, 00001h              ;Cylinder 0, Sektor 1
            mov     bx, 07C00h              ;OFS Partitionssektor (ES:)
            int     013h
            jnc     _lodbs
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _hddwr
            jmp     _err_2

;- BOOT.BIN laden ------------------------------------------------------------

_lodbs:

;001012-UM: bei Boot via F2 jetzt zur Floppy-Boot verzweigen}
            cmp     byte ptr ds: [019E8h], 00h ; floppy boot?
            je      _no_diskb
            jmp     _diskb

_no_diskb:
; UM: Erweiterung zur Unterstuetzung der INT 13 Extensions:
;     erstmal pruefen, ob BIOS INT 13 Extensions unterstuetzt

            mov     ah, 041h
            mov     bx, 055AAh
            mov     dl, byte ptr [si]       ;Drive
            int     13h
            jc      _noint13x
            cmp     bx, 0AA55h
            jne     _noint13x
            and     cx, 1                   ; bit 0: read/write/support
            jc      _noint13x

; UM: jetzt pruefen, ob das Bootlaufwerk unterstuetzt wird:

            mov     ah, 048h
            mov     dl, byte ptr [si]       ;Drive
            push    si
            mov     si, offset i13x_param
            int     13h
            pop     si
            jc      _noint13x

; UM: jetzt Bootsektor mit INT 13 Extensions laden:
            mov     di, 00003h              ;3 Versuche

            mov     bx, word ptr [si+009h]  ;Distance low word
            mov     i13x_struct+08h, bx

            mov     bx, word ptr [si+00Bh]  ;Distance high word
            mov     i13x_struct+0Ah, bx

_botrd_x:   mov     dl, byte ptr [si]       ;Drive
            push    si
            mov     si, offset i13x_struct

            mov     ah, 42h                 ;Sektor lesen
            int     013h
            pop     si
            jnc     _bootj
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _botrd_x
            jmp     _err_3

; UM: keine INT 13 Extensions, also weiter im Originalcode:
_noint13x:  mov     di, 00003h              ;3 Versuche
_botrd:     mov     ax, 00201h              ;Sektor lesen
            mov     dl, byte ptr [si]       ;Drive
            mov     dh, byte ptr [si+001h]  ;Head
            mov     cx, word ptr [si+002h]  ;SecCyl
            mov     bx, 07C00h              ;OFS Bootsektor (ES:)
            int     013h
            jnc     _bootj
            mov     ah, 0Dh                 ;Reset
            int     013h
            dec     di
            jnz     _botrd
            jmp     _err_3

;- Bootsektor patchen --------------------------------------------------------

_bootj:     mov     al, byte ptr [si+005h]  ;Systemkennung
            mov     bx, offset fstbl2       ;Patchtabelle
            call    _fschk                  ;PrÅfen
            or      al, al
            jz      _bschk

            mov     bl, byte ptr [si]       ;Drive
            mov     byte ptr ds: [07424h], bl

            cmp     byte ptr [si+006h], 000h ;PRI/EXT?
            jne     _bschk

            mov     bl, byte ptr [si+004h]  ;Logisches Laufwerk
            mov     byte ptr ds: [07425h], bl
            mov     bx, word ptr [si+009h]  ;Distance low word
            mov     word ptr ds: [0741Ch], bx
            mov     bx, word ptr [si+00Bh]  ;Distance high word
            mov     word ptr ds: [0741Eh], bx
            jmp     short _bschk

;_btj_1: jmp short _bschk                ;Sprung zur öberprÅfung.

;- Booten von Diskette -------------------------------------------------------

_diskb:     mov     bx, offset msgdsk       ;Diskette A:
            call    _print                  ;ausgeben

            mov     bx, offset msgbot       ;booten
            call    _print

            mov     bx, 01912h              ;OFS FloppyPWD
            call    _passw

            mov     di, 00003h              ;3 Versuche
_dskrd:     mov     ah, 002h                ;Sektor(en) lesen
            xor     dl, dl                  ;Drive 0
            mov     dh, 000h                ;Head 0
            mov     cx, 00001h              ;SecCyl 0/1
            mov     al, 001h                ;Anzahl der Sektoren
            mov     bx, 07C00h              ;OFS Bootsektor
            int     013h
            jnc     _bschk
            xor     ah, ah                  ;Reset
            int     013h
            dec     di
            jnz     _dskrd
            jmp     _scrn

_bschk:     cmp     word ptr ds: [075FEh], 0AA55h ;Betriebsystem gÅltig?
            jne     _bch_1
            jmp     short _jump             ;Und ab die Post...

_bch_1:     mov     bx, offset msgnbs
            call    _print
            jmp     _scrn

;- Labels --------------------------------------------------------------------

_err_1:     mov     bx, offset msgrr1
            call    _print
            jmp     short _errlp
_err_2:     mov     bx, offset msgrr2
            call    _print
            jmp     short _errlp
_err_3:     mov     bx, offset msgrr3
            call    _print
_errlp:     hlt
            jmp     short _errlp

_jump:      mov     si, offset _rentr       ;Funktion kopieren
            mov     di, 00800h              ;nach 00800h (ES:)
            mov     cx, offset _reend - offset _rentr ;LÑnge berechnen
            cld                             ;VorwÑrts!
            rep     movsb

;       jmp 00080h:00000h
            db      0EAh
            dw      00000h, 00080h

_rentr:     mov     di, 00800h              ;OFS BootmenÅ berechnen
            add     di, offset _reend - offset _rentr
            mov     cx, 03A00h              ;LEN BootmenÅ berechnen
            sub     cx, offset _reend - offset _rentr
            xor     ax, ax
            cld                             ;VorwÑrts!
            rep     stosw                   ;Lîschen!

            xor     ax, ax
            mov     ds, ax

            mov     si, bp                  ;SI: BP OFS aktive Partition

;       jmp 00000h:07C00h               ;Sprung nach BOOT.BIN
            db      0EAh
            dw      07C00h, 00000h

_reend:                                     ;Ende der Fahnenstange


;- Funktionen ----------------------------------------------------------------

pwdin       db      'l.We-mk9[3%74lOw'
pwdtbl1     db      'Ui3=kE@#l~}4D$h&'
pwdtbl2     db      '4E)\De<-5l?Q$1Wi'

_print:     push    ax
            push    si
            push    di
            push    bp
            push    es

_prnl1:     mov     ah, 00Eh                ;Zeichen ausgeben
            mov     al, [bx]                ;Zeichen holen (bx OFS message)
            cmp     al, 0                   ;Endezeichen? (NULL-Terminiert)
            je      short _endpr
            int     010h
            inc     bx                      ;nÑchstes Zeichen
            jmp     short _prnl1

_endpr:     pop     es
            pop     bp
            pop     di
            pop     si
            pop     ax
            ret



_goto:      push    ax
            push    si
            mov     ah, 002h                ;Funktion GotoXY
            xor     bh, bh                  ;Bildschirmseite 0
            int     010h

_endgo:     pop     si
            pop     ax
            ret



_entry:     push    bx
            push    cx
            push    dx                      ;Entrynummer / Status sichern
            add     dh, 008h                ;Zeile 8
            mov     dl, 00Eh                ;Spalte 14!
            mov     ah, 002h                ;Funktion GotoXY
            xor     bh, bh                  ;Bildschirmseite 0
            int     010h
            mov     bl, 070h                ;Balken schwarz auf grau
            pop     dx
            push    dx
            cmp     dl, 001h                ;Aktiver Eintrag?
            je      _ent_1
            mov     bl, byte ptr wincol     ;Balken grau auf blau/schwarz
_ent_1:     mov     ax, 00920h              ;Funktion Zeichen/Farbe ausgeben
            mov     cx, 00034h              ;52 Spalten!
            int     010h
            pop     dx
            push    dx
            xchg    dh, dl
            xor     dh, dh
            shl     dx, 005h                ;dx * 32
            mov     si, 01800h              ;OFS Entries
            add     si, dx
            xor     al, al                  ;Return Status Entry Empty
            cmp     word ptr [si+01Eh], 0AA55h ;Markierung vorhanden?
            jne     _enden
            push    si
            mov     bx, si
            add     bx, 00Dh                ;Name der Partition
            mov     ax, 00E20h              ;2 Leerzeichen voranstellen
            int     010h
            mov     ax, 00E20h              ;Funktion Ausgabe eines Zeichens
            int     010h
            call    _print
            pop     si
            cmp     byte ptr ds: [019EAh], 01h ;SimpleMenus?
            je      _reten
            pop     dx
            push    dx
            push    si
            add     dh, 008h                ;Zeile 8
            mov     dl, 020h                ;Spalte 32!
            mov     ah, 002h                ;Funktion GotoXY
            xor     bh, bh                  ;Bildschirmseite 0
            int     010h
            pop     si
            push    si
            mov     bx, offset prtpri       ;PRIMéR
            cmp     byte ptr [si+006h], 000h ;PRI/EXT?
            jne     _ent_2
            mov     bx, offset prtlog
_ent_2:     call    _print
            pop     si
            push    si
            mov     bx, offset prtsiz
            mov     ax, word ptr [si+007h]  ;Partitionsgrî·e in MB
            mov     cx, 00005h              ;5 Stellen
_ent_3:     xor     dx, dx
            cmp     ax, 00000h              ;ist die Division abgeschlossen?
            je      _ent_4
            push    cx
            mov     cx, 0000Ah
            div     cx                      ;ax / 10 -> ax, Rest dx
            pop     cx
            add     dl, 030h                ;ASCII 48 ('0') addieren
            jmp     short _ent_5
_ent_4:     mov     dl, 020h                ;ASCII 32 (' ');
_ent_5:     mov     di, bx
            add     di, cx
            mov     byte ptr [di], dl       ;In den String eintragen
            loop    _ent_3
            call    _print
            pop     si
            push    si
            mov     al, byte ptr [si+005h]  ;Systemkennung
            mov     cl, 00Dh
            mul     cl                      ;al * 13
            mov     bx, offset prtsys
            add     bx, ax
            call    _print
            pop     si
_reten:     mov     al, 001h                ;Return Status OK
_enden:     pop     dx
            pop     cx
            pop     bx
            ret

_timef:     push    ax
            push    bx
            push    cx
            push    dx
            mov     dx, 0120Fh              ;Zeile 18 / Spalte 15;
            mov     ah, 002h                ;Funktion GotoXY
            xor     bh, bh                  ;Bildschirmseite 0
            int     010h
            cmp     byte ptr tflag2, 001h   ;Timeout aktiv?
            jne     _timin
            mov     si, offset outtim       ;TimeOut Zeit
            mov     ax, word ptr [si]
            mov     bx, offset timout       ;TimeOut Variable holen
            mov     cx, 00004h              ;4 Stellen
_tim_1:     xor     dx, dx
            cmp     ax, 00000h              ;ist die Division abgeschlossen?
            je      _tim_2
            push    cx
            mov     cx, 0000Ah
            div     cx                      ;ax / 10 -> ax, Rest dx
            pop     cx
            add     dl, 030h                ;ASCII 48 ('0') addieren
            jmp     short _tim_3
_tim_2:     mov     dl, 020h                ;ASCII 32 (' ');
_tim_3:     mov     di, bx
            add     di, cx
            mov     byte ptr [di], dl       ;In den String eintragen
            loop    _tim_1
            mov     bx, offset msgtim       ;TimeOut Message
            call    _print
            jmp     short _endti
_timin:     mov     bx, offset msgnot
            call    _print
            cmp     byte ptr tflag2, 000h   ;Timeout deaktiviert?
            jne     _endti
            mov     bx, offset msgtin
            call    _print
_endti:     pop     dx
            pop     cx
            pop     bx
            pop     ax
            ret

_instt:     push    ax
            push    bx
            push    cx
            push    dx
            mov     bx, offset tflag1       ;TimeFlag
            mov     byte ptr [bx], 00000h   ;ZurÅcksetzen
            xor     ah, ah                  ;Funktion ZeitzÑhler auslesen
            int     01Ah
            mov     bx, offset tstore       ;Zeitspeicher
            mov     word ptr [bx], dx       ;lo-word des ZeitzÑhlers
_endit:     pop     dx
            pop     cx
            pop     bx
            pop     ax
            ret

_dotim:     push    ax
            push    bx
            push    cx
            push    dx
            xor     ah, ah                  ;ZeitzÑhler auslesen
            int     01Ah
            mov     bx, offset tstore       ;Zeitspeicher
            mov     ax, dx
            mov     cx, word ptr [bx]
            sub     ax, cx                  ;Aktuelle Zeit - Gespeicherte
            cmp     ax, 012h                ;>18 ?
            jbe     _enddo
            mov     bx, offset tflag1       ;Zeitflag
            inc     byte ptr [bx]           ;erhîhen
_enddo:     pop     dx
            pop     cx
            pop     bx
            pop     ax
            ret

_hndlt:     push    bx
            cmp     byte ptr tflag2, 001h   ;Timeout Åberhaupt aktiv?
            jne     _ht_e
            mov     bl, byte ptr ds: [019EEh] ;OFS TimeHndl
            cmp     bl, 02h                 ;TimeoutIgnore
            je      _ht_e
            cmp     bl, 01h                 ;TimeoutReset
            jne     _ht_1
            mov     bx, word ptr ds: [019FCh] ;OFS Timeout
            mov     word ptr outtim, bx     ;Timeout Zeit
            call    _instt                  ;Timer installieren
            call    _timef                  ;Timeout Zeile schreiben
            jmp     short _ht_e
_ht_1:      mov     byte ptr tflag2, 000h   ;Timeout deaktivieren
            call    _timef
_ht_e:      pop     bx
            ret

_ssave:     push    cx
            push    ds
            push    es
            mov     cx, 007D0h              ;2000 Words kopieren...
            les     di, dword ptr savseg    ;Sicherungssegment (ZIEL)
            lds     si, dword ptr vidseg    ;Videosegment (QUELLE)
            cld                             ;VorwÑrts!
            rep     movsw                   ;Bildschirm sichern
            pop     es
            pop     ds
            pop     cx
            ret

_srest:     push    cx
            push    ds
            push    es
            mov     cx, 007D0h              ;2000 Words zurÅcklesen...
            les     di, dword ptr vidseg    ;Videosegment (ZIEL)
            lds     si, dword ptr savseg    ;Sicherungssegment (QUELLE)
            cld                             ;VorwÑrts!
            rep     movsw                   ;Bildschirm restaurieren
            pop     es
            pop     ds
            pop     cx
            ret

_sclr:      push    cx
            push    es
            mov     ax, 00720h              ;Attributbyte/Ascii 32
            mov     cx, 007D0h              ;2000 Words...
            les     di, dword ptr vidseg    ;Videosegment (ZIEL)
            cld                             ;VorwÑrts!
            rep     stosw                   ;Bildschirm lîschen
            pop     es
            pop     cx
            ret

_fschk:     push    cx                      ;Auf Laufwerk prÅfen (FAT/HPFS/NTFS)
            xor     ch, ch
_fsckl:     mov     cl, byte ptr [bx]
            cmp     cl, al
            je      _fscke
            inc     bx
            or      cl, cl
            jnz     _fsckl
            xor     al, al
_fscke:     pop     cx
            ret

_passw:     push    ax
            push    si

            cmp     word ptr ds: [019F0h], 0AA55h ;OFS VerMark
            jne     _pwdj1

            cmp     word ptr ds: [019F2h], 00069h ;OFS VerInfo
            jb      _pwdj1

            mov     al, byte ptr [bx]       ;Passwortschutz?
            or      al, al
            jz      _pwdj1

            mov     cx, 00002h
            call    _beep

            mov     cx, 00003h              ;3 Abfragen
            inc     bx
            jmp     short _pwdl0

_pwdj1:     jmp     _pwdrt                  ;Jump an's Ende

_pwdl0:     push    cx
            push    bx
            mov     bx, offset msgpwd       ;Passwortabfrage
            call    _print

            mov     cx, 00011h
            mov     di, offset pwdin
_pwdl1:     mov     byte ptr [di], 000h
            inc     di
            loop    _pwdl1

            mov     cx, 00010h
            mov     di, offset pwdin
_pwdl2:     mov     ah, 000h
            int     16h

            or      al, al                  ;Sondertaste? (ignorieren)
            jz      _pwdl2

            cmp     al, 00Dh                ;Return?
            je      _pwdl4

            cmp     al, 008h                ;Bakspace?
            jne     _pwdl3
            cmp     di, offset pwdin
            je      _pwdl2
            dec     di
            mov     byte ptr [di], 000h

            mov     bx, offset msgbksp
            call    _print

            jmp     short _pwdl2

_pwdl3:     mov     byte ptr [di], al
            inc     di

            mov     bx, offset msgastr
            call    _print

            loop    _pwdl2

_pwdl4:     sub     di, offset pwdin
            and     di, 00001h
            or      di, di
            mov     si, offset pwdtbl1      ;ODD Table
            jnz     _pwdl5
            mov     si, offset pwdtbl2      ;EVEN Table

_pwdl5:     mov     cx, 00010h
            mov     di, offset pwdin

_pwdl6:     mov     dl, byte ptr [si]
            xor     byte ptr [di], dl
            inc     si
            inc     di
            loop    _pwdl6

            pop     bx
            push    bx
            mov     si, bx
            mov     di, offset pwdin
            mov     cx, 00010h
_pwdl7:     mov     dl, byte ptr [si]
            xor     byte ptr [di], dl
            jnz     _pwdl8
            inc     si
            inc     di
            loop    _pwdl7

            pop     bx
            pop     cx
            mov     bx, offset msgcrlf
            call    _print
            jmp     short _pwdrt

_pwdl8:     mov     cx, 00003h
            call    _beep

            mov     bx, offset msgpinv
            call    _print

            pop     bx
            pop     cx
            loop    _pwdj2
            jmp     short _pwdl9

_pwdj2:     jmp     _pwdl0

_pwdl9:     mov     bx, offset msgphlt
            call    _print
            jmp     _errlp

_pwdrt:     pop     si
            pop     ax
            ret


_beep:      cmp     byte ptr ds: [019EDh], 01h ;OFS BlindSupp
            jne     _bpend

            push    ax
            push    dx
            push    si
            push    di
            push    bp
            push    es

_bplp:      push    cx
            mov     ah, 086h
            mov     cx, 00003h
            mov     dx, 00D40h              ;200 ms warten
            int     015h

            mov     ax, 00E07h              ;BELL
            int     010h

            pop     cx
            loop    _bplp

            pop     es
            pop     bp
            pop     di
            pop     si
            pop     dx
            pop     ax

_bpend:     ret


_bipp:      cmp     byte ptr ds: [019EDh], 01h ;OFS BlindSupp
            jne     _biend

            push    ax
            push    si
            push    di
            push    bp
            push    es

            mov     ax, 00E07h              ;BELL
            int     010h

            pop     es
            pop     bp
            pop     di
            pop     si
            pop     ax

_biend:     ret

;- Variablen -----------------------------------------------------------------

vidseg      dw      00000h, 0B000h          ;Segment der MONO-Karte
savseg      dw      02800h, 00000h          ;Segment zum Sichern des Videomem
wincol      db      007h                    ;Fenster hellgrau auf schwarz
cursiz      dw      00000h                  ;Cursorgrî·e
curpos      dw      00000h                  ;Cursorposition
outtim      dw      00000h                  ;TimeOut-Zeit in Sekunden...
tflag1      db      000h                    ;1 Sekunden Takt!
tflag2      db      001h                    ;Timeout aktiv / inaktiv
tstore      dw      00000h                  ;Abgelaufene 18.2tel Sekunden
ishidn      db      000h                    ;Versteckt

;- Konstanten ----------------------------------------------------------------

INCLUDE     MENU.ASI

maxent      db      008h
msgcrlf     db      13, 10, 0
msgbksp     db      8, 32, 8, 0
msgastr     db      '*', 0
fstbl1      db      001h, 004h, 005h, 006h, 00Ah, 00Bh, 00Ch, 00Fh
            db      011h, 012h, 013h, 015h, 016h, 017h, 000h
fstbl2      db      001h, 004h, 005h, 006h, 011h, 012h, 013h, 000h
prtsys      db      'N/A         ', 0
            db      'FAT12 <32M  ', 0
            db      'XENIX root  ', 0
            db      'XENIX usr   ', 0
            db      'FAT16 <32M  ', 0
            db      'FAT16 >32M  ', 0
            db      'HPFS/NTFS   ', 0
            db      'AIX Boot    ', 0
            db      'AIX/Coherent', 0
            db      'Boot Manager', 0
            db      'FAT32       ', 0
            db      'FAT32 INT13X', 0
            db      'FAT16 INT13X', 0
            db      'WIN95 EXT.  ', 0
            db      'OPUS        ', 0
            db      'FAT12 <32M  ', 0
            db      'Compaq Diag ', 0
            db      'FAT16 <32M  ', 0
            db      'FAT16 >32M  ', 0
            db      'HPFS/NTFS   ', 0
            db      'AST Swap    ', 0
            db      'FAT32       ', 0
            db      'FAT32 INT13X', 0
            db      'FAT16 INT13X', 0
            db      'NEC DOS     ', 0
            db      'Recovery    ', 0
            db      'VENIX 80286 ', 0
            db      'SFS         ', 0
            db      'Disk Manager', 0
            db      'Disk Manager', 0
            db      'CP/M        ', 0
            db      'VFeature    ', 0
            db      'SpeedStor   ', 0
            db      'System V/386', 0
            db      'NetWare     ', 0
            db      'NetWare     ', 0
            db      'Multi-Boot  ', 0
            db      'PC/IX       ', 0
            db      'Minix       ', 0
            db      'Linux Minix ', 0
            db      'Linux Swap  ', 0
            db      'Linux Native', 0
            db      'FAT16 <32M  ', 0
            db      'Amoeba      ', 0
            db      'Amoeba BBT  ', 0
            db      'BSD/386     ', 0
            db      'BSDI        ', 0
            db      'BSDI Swap   ', 0
            db      'DR DOS FAT12', 0
            db      'DR DOS FAT16', 0
            db      'DR DOS FAT16', 0
            db      'Syrinx Boot ', 0
            db      'CP/M        ', 0
            db      'SpeedStor 12', 0
            db      'DOS R/O     ', 0
            db      'SpeedStor 16', 0
            db      'DOS 2ndary  ', 0
            db      'SpeedStor   ', 0
            db      'Prologue    ', 0
            db      'LANstep     ', 0
            db      'Xenix BBT   ', 0


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

_code       ends

            end

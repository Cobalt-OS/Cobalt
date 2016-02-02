;                           ÜฺอออออออออออออออออฟÜ                            ;
;                        ฤออนณ CPUidle for DOS ณฬออฤ                         ;
;                           ฿ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู฿                            ;


;[KERNEL CHARACTERISTICS]
; Kernel name:          CPUidle for DOS.
; Kernel version:       V2.10 [Build 0077]
; Programming stage:    Working version, Under development.
; Last modified:        1998 May 07.

;[NOTES]
; Ralphs intlist -> more idle possibilities.



;ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป;
;บ ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ RESIDENT PART OF PROGRAM ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ บ;
;ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑ GLOBAL CODE & DATA FOR ALL HANDLERS ฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

ideal                                   ; Yep, this prog is TASM 4.0 coded!
include "_stddata.ah"
include "_tsrres.ah"
include "_dcon.ah"


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


Struct  qk_item
        prog    db 12 dup (0), 0        ; Name of the child process.
        hooknum db 0                    ; Number of FN hooks.
        execnum dw 0                    ; "PID number" of child.
Ends

Struct  qk_hook
        fnaddr  dw 0                    ; Address of FN to hook.
        newaddr dw 0                    ; New address of the FN.
        oldaddr dw 0                    ; Old address of the FN.
Ends


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


MODE_OPTIMIZE   = 01h                   ; Set if CPU optimization selected.
MODE_HLT        = 02h                   ; Set if normal HLT method selected.
MODE_APM        = 04h                   ; Set if APM cooling method selected.
MODE_NOFORCE    = 08h                   ; Set if any FORCE MODE is disabled.
MODE_WFORCE     = 10h                   ; Set if WEAK FORCE strategy selected.
MODE_SFORCE     = 20h                   ; Set if STRONG FORCE strategy selected.

IRQ_00          = 01h                   ;
IRQ_01          = 02h                   ;
IRQ_02          = 04h                   ;
IRQ_03          = 08h                   ;
IRQ_04          = 10h                   ;
IRQ_05          = 20h                   ; Flag set if that specific IRQ was
IRQ_06          = 40h                   ; invoked. Should later be cleared by
IRQ_07          = 80h                   ; kernel...

INT_XXH_FORCE   = 300                   ; # of calls to FN before forced HLT. 


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTDATA begin

Align 4
int_xxh_fcount  dd 0                    ;  # int xxh FN(x) called repeatedly.

mode_flags      db MODE_SFORCE          ; Config flags for program startup.
irq_flags       db 0                    ; IRQ flags for kernel.

quirk_table     qk_item <"NC.EXE", 1, 0>
                 qk_hook <int_21h_fntable + 2ch * 2, int_xxh_forcehlt, int_xxh_zerocount>
                qk_item <"SCANDISK.EXE", 1, 0>
                 qk_hook <int_21h_fntable + 0bh * 2, int_xxh_zerocount, int_xxh_forcehlt>
                QK_ITEMS = 2

exec_calls      dw 200                  ; Count of DOS FN 4bh calls.
child_name      db 13 dup (0)           ; Name of the child to be executed.

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Proc    _str_cmp                        ; NOTE: Copied from _string.h!!
        push ax cx si di

        mov cx,DEF_STR_LEN              ; CX = default string length.
@@cmp:  mov al,[ds:si]                  ; Read char from string #1.
        cmp al,[es:di]                  ; Char (#1) == char (#2)?
        jne short @@done                ; Nope, strings can't be equal.

        test al,al                      ; Done (char (#1) = char (#2) = 0)?
        jz short @@done                 ; Yes, string are equal.

        inc si                          ;
        inc di                          ;
        loop @@cmp                      ; Continue.

@@done: pop di si cx ax
        ret
Endp


;----------------------------------------------------------------------------;

Proc    int_xxh_forcehlt
        inc [int_xxh_fcount]                    ; Increase force counter.
        cmp [int_xxh_fcount],INT_XXH_FORCE      ; Over the minimum?
        jb short @@done                         ; Nah, don't HLT yet.

        mov [irq_flags],0               ; Clear IRQ flags.
        sti                             ; Enable IRQs for following HLT.

        test [mode_flags],MODE_APM      ; APM usage requested?
        jnz short @@apm                 ; Yes.

        ;-  -  -  -  -  -  -  -  -  -  -;
@@std:  test [mode_flags],MODE_SFORCE   ; Running under STRONG FORCE mode?
        jnz short @@stds                ; Yes.

@@stdw: hlt                             ; Enter power saving mode.
        ret                             ; Fast exit.

@@stds: and [irq_flags],not IRQ_00      ; Clear IRQ0 occurred flag.
	hlt                             ; Enter power saving mode.

        cmp [irq_flags],IRQ_00          ; Was it IRQ0 (timer) ONLY?!
        je @@stds                       ; Yes, go back HLTing.
        ret                             ; Fast exit.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
@@apm:  test [mode_flags],MODE_SFORCE   ; Running under STRONG FORCE mode?
        jnz short @@apms                ; Yes.

@@apmw: mov ax,5305h                    ;
        pushf                           ;
        call [dword old_int_15h]        ; Call APM FN to put the CPU idle.
        ret

@@apms: and [irq_flags],not IRQ_00      ; Clear IRQ0 occurred flag.
        mov ax,5305h                    ;
        pushf                           ;
        call [dword old_int_15h]        ; Call APM FN to put the CPU idle.

        cmp [irq_flags],IRQ_00          ; Was it IRQ0 (timer) ONLY?!
        je @@apms                       ; Yes, go back HLTing.
        ;-  -  -  -  -  -  -  -  -  -  -;
@@done: ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    int_xxh_zerocount               ; Zero ALL FORCE counters.
        mov [int_xxh_fcount],0          ; Zero int xxh force counter.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    int_xxh_skip                    ; Skip ALL FORCE counter updates.
        ret
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑ INT 21H HANDLER ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

INT_21H_TOPFN   = 4ch                   ; Highest FN that is handled.


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTDATA begin

Align 4
old_int_21h     rmdw <0, 0>

int_21h_fntable dw int_xxh_zerocount    ; FN 00h: Terminate.
                dw int_21h_normalhlt    ; FN 01h: Keyboard input.
                dw int_xxh_zerocount    ; FN 02h: Display char.
                dw int_xxh_skip         ; FN 03h: Auxiliary input.
                dw int_xxh_zerocount    ; FN 04h: Auxiliary output.
                dw int_xxh_zerocount    ; FN 05h: Printer output.
                dw int_21h_fn06h        ; FN 06h: Console I/O.
                dw int_21h_normalhlt    ; FN 07h: No echo unfiltered input.
                dw int_21h_normalhlt    ; FN 08h: No echo input.
                dw int_xxh_zerocount    ; FN 09h: Display string.
                dw int_xxh_skip         ; FN 0ah: Buffered input.
                dw int_xxh_forcehlt     ; FN 0bh: "Keypressed?"
                dw int_xxh_skip         ; FN 0ch: Clear buffer and input.
                dw 24h dup (int_xxh_zerocount)  ; FNs 0dh - 30h.
                dw int_21h_fn31h        ; FN 31h: Terminate and Stay Resident.
                dw 19h dup (int_xxh_zerocount)  ; FNs 32h - 4ah.
                dw int_21h_fn4bh        ; FN 4bh: Execute child process.
                dw int_21h_fn4ch        ; FN 4ch: Terminate child process.

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Proc    int_21h_fn06h                   ; DOS FN: Console I/O.
        cmp dl,0ffh                     ; "Keypressed?" function requested?
        jne short @@done                ; No.

        jmp [int_21h_fntable + 0bh * 2] ; Force HLT (as FN 0bh does it).
@@done: ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    int_21h_fn31h                   ; DOS FN: Terminate and Stay Resident.
        jmp int_21h_fn4ch               ; Same as standard exit...
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    int_21h_fn4bh                   ; DOS FN: Execute child process.
        pusha
        push es                         ;
        mov bp,sp                       ; BP = ptr to entry DS on stack.

        inc [exec_calls]                ; Increase count of exec calls.

        test al,al                      ; Load and execute child?
        jnz short @@done                ; No, no work for us.

        mov ax,[ss:bp + 2 + 16 + 2]     ; Get DS from stack.
        mov es,ax                       ; 
        mov di,dx                       ; ES:DI = caller's DS:DX = child name.

        ;-  -  -  -  -  -  -  -  -  -  -;
        lea si,[child_name]             ; DS:SI = target buffer for child name.
        xor bx,bx                       ; BX = index of char at [DS:SI].

@@read: mov al,[es:di]                  ; Get char of child name in int 21h.
        mov [ds:si + bx],al             ; Save it to our buffer.

        cmp al,':'                      ; Was it a DRIVE specifier?
        je short @@kill                 ; Yes.

        cmp al,'\'                      ; Was it a PATH separator?
        jne short @@next                ; No.

@@kill: mov bx,-1                       ; Restart savig to buffer...

@@next: inc di                          ;
        inc bx                          ; Advance index pointers.
        test al,al                      ; At end of ASCIIZ filename?
        jnz @@read                      ; No.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
        lea bx,[quirk_table]            ; BX = ptr to table of quirky programs.
        mov cx,QK_ITEMS                 ; CX = number of entries in table.
        lea di,[child_name]             ;
        mov ax,ds                       ;
        mov es,ax                       ; ES:DI = ptr to ASCIIZ name of child.

@@find: lea si,[(qk_item bx).prog]      ; SI = ptr to quirky child name.
        call _str_cmp                   ; Is this child being executed?
        je short @@set                  ; Yes, handle it.

        mov al,[(qk_item bx).hooknum]   ; AL = number of FN hooks.
        mov ah,size qk_hook             ; AH = size of one FN hook.
        mul ah                          ; AX = value to increment BX with.

        add bx,ax                       ;
        add bx,size qk_item             ;
        loop @@find                     ; Continue.
        jmp short @@done                ; Child is NOT a quirky program, done.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
@@set:  mov ax,[exec_calls]             ;
        mov [(qk_item bx).execnum],ax   ; Save "PID" of this quirky program.

        xor ch,ch                       ;
        mov cl,[(qk_item bx).hooknum]   ; CX = number of hooks to install.
        add bx,size qk_item             ; BX = ptr to first hook data.

        test cl,cl                      ; No hooks needed?
        jz short @@done                 ; Yes, crazy but quit...

@@hook: mov si,[(qk_hook bx).fnaddr]    ; SI = address of FN to hook.
        mov ax,[ds:si]                  ; Get old FN handler.
        mov [(qk_hook bx).oldaddr],ax   ; Save it.

        mov ax,[(qk_hook bx).newaddr]   ; Get new address for FN handler.
        mov [ds:si],ax                  ; Hook FN.

        add bx,size qk_hook             ;
        loop @@hook                     ; Continue.
        ;-  -  -  -  -  -  -  -  -  -  -;

@@done: pop es
        popa
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    int_21h_fn4ch                   ; DOS FN: Terminate child process.
        pusha
        lea bx,[quirk_table]            ; BX = ptr to table of quirky programs.
        mov cx,QK_ITEMS                 ; CX = number of entries in table.

        ;-  -  -  -  -  -  -  -  -  -  -;
@@find: mov ax,[(qk_item bx).execnum]   ; Get "PID" of saved program.
        cmp ax,[exec_calls]             ; Is it this program?
        je short @@set                  ; Yes, handle it.

        mov al,[(qk_item bx).hooknum]   ; AL = number of FN hooks.
        mov ah,size qk_hook             ; AH = size of one FN hook.
        mul ah                          ; AX = value to increment BX with.

        add bx,ax                       ;
        add bx,size qk_item             ;
        loop @@find                     ; Continue.
        jmp short @@done                ; Finish, program wasn't found.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
@@set:  xor ch,ch                       ;
        mov cl,[(qk_item bx).hooknum]   ; CX = number of hooks to deinstall.
        add bx,size qk_item             ; BX = ptr to first hook data.

        test cl,cl                      ; No hooks needed?
        jz short @@done                 ; Yes, crazy but quit...

@@unhk: mov si,[(qk_hook bx).fnaddr]    ; SI = address of FN to unhook.
        mov ax,[(qk_hook bx).oldaddr]   ; Get old FN handler.
        mov [ds:si],ax                  ; Restore original handler.

        add bx,size qk_hook             ;
        loop @@unhk                     ; Continue.
        ;-  -  -  -  -  -  -  -  -  -  -;

@@done: dec [exec_calls]
        popa
        ret
Endp


;----------------------------------------------------------------------------;


Proc    int_21h_normalhlt
        sti                             ; Enable IRQs for following HLT.
        mov ah,0bh                      ; Int 21h FN: "Keypressed?".

        test [mode_flags],MODE_APM      ; APM usage requested?
        jnz short @@apml                ; Yes.

@@stdl: hlt                             ; Enter power saving mode.
        pushf                           ;
        call [dword old_int_21h]        ; Simulate int 21h without reentrancy.

        cmp al,0ffh                     ; Keystroke ready?
        jne @@stdl                      ; No, continue HLTing.
        jmp short @@done                ; Finish.

@@apml: mov ax,5305h                    ;
        pushf                           ;
        call [dword old_int_15h]        ; Call APM FN to put the CPU idle.

        mov ah,0bh                      ; Int 21h FN: "Keypressed?"
        pushf                           ;
        call [dword old_int_21h]        ; Simulate int 21h without reentrancy.

        cmp al,0ffh                     ; Keystroke ready?
        jne @@apml                      ; No, continue HLTing.
@@done: ret
Endp


;----------------------------------------------------------------------------;


Align 16
Proc    int_21h_handler                 ; DOS functions handler.
        push ax bx ds
        mov bx,cs                       ;
        mov ds,bx                       ; CODE = DATA.

        cmp ah,INT_21H_TOPFN            ; FN irrelevant for our handler?
        ja short @@old                  ; Yes, zero force counter and chain.

        xor bh,bh                       ;
        mov bl,ah                       ;
        add bx,bx                       ; BX = index to int_21h_fntable.
        add bx,offset int_21h_fntable   ; BX = offset of handler.

        call [word bx]                  ; Call the appropriate FN handler.
        jmp short @@oldn                ; Chain without zeroing force count.

@@old:  mov [int_xxh_fcount],0          ; Zero int xxh force counter.

@@oldn: pop ds bx ax
        jmp [dword cs:old_int_21h]      ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑ INT 16H HANDLER ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

INT_16H_TOPFN   = 12h                   ; Highest FN that is handled.


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTDATA begin

Align 4
old_int_16h     rmdw <0, 0>

int_16h_fntable dw int_16h_normalhlt    ; FN 00h: Keyboard input.
                dw int_xxh_forcehlt     ; FN 01h: "Keypressed?".
                dw int_xxh_forcehlt     ; FN 02h: "SHIFT Keypressed?".
                dw 0dh dup (int_xxh_zerocount)  ; FNs 03h - 09h.
                dw int_16h_normalhlt    ; FN 10h: Keyboard input (101-keys).
                dw int_xxh_forcehlt     ; FN 11h: "Keypressed?" (101-keys).
                dw int_xxh_forcehlt     ; FN 12h: "SHIFT Keypressed?" (101).

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Proc    int_16h_normalhlt
	inc ah                          ; Int 16h FN: Is keystroke ready?
        mov bh,ah                       ; Save AH (FN number).
	sti                             ; Enable IRQs for following HLT.

        test [mode_flags],MODE_APM      ; APM usage requested?
        jnz short @@apml                ; Yes.

@@stdl: pushf                           ;
        call [dword old_int_16h]        ; Simulate int 16h without reentrancy.
        jnz short @@done                ; If ZF == 0 then key is ready.

        hlt                             ; Enter power saving mode.

        mov ah,bh                       ; Restore saved AH (FN number).
        jmp @@stdl

@@apml: pushf                           ;
        call [dword old_int_16h]        ; Simulate int 16h without reentrancy.
        jnz short @@done                ; If ZF == 0 then key is ready.

        mov ax,5305h                    ;
        pushf                           ;
        call [dword old_int_15h]        ; Call APM FN to put the CPU idle.

        mov ah,bh                       ; Restore saved AH (FN number).
        jmp @@apml                      ; No, continue HLTing.
@@done: ret
Endp


;----------------------------------------------------------------------------;


Align 16
Proc    int_16h_handler                 ; BIOS keyboard functions handler.
        push ax bx ds
        mov bx,cs                       ;
        mov ds,bx                       ; CODE = DATA.

        cmp ah,INT_16H_TOPFN            ; FN irrelevant for our handler?
        ja short @@old                  ; Yes, zero force counter and chain.

        xor bh,bh                       ;
        mov bl,ah                       ;
        add bx,bx                       ; BX = index to int_16h_fntable.
        add bx,offset int_16h_fntable   ; BX = offset of handler.

        call [word bx]                  ; Call the appropriate FN handler.
        jmp short @@oldn                ; Chain without zeroing force count.

@@old:  mov [int_xxh_fcount],0          ; Zero int xxh force counter.

@@oldn: pop ds bx ax
        jmp [dword cs:old_int_16h]      ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑ INT 2FH HANDLER ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

INT_2FH_TOPFN   = 0ffffh                ; Highest FN that is handled.


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTDATA begin

Align 4
old_int_2fh     rmdw <0, 0>

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Align 16
Proc    int_2fh_handler
        push dx ds
        mov dx,cs                       ;
        mov ds,dx                       ; CODE = DATA.
        
        cmp ax,1680h                    ; DPMI release time slice?
        je short @@dpmi                 ; Yes.

        cmp ax,1607                     ; Windows VMPoll Idle callout?
        jne short @@old                 ; No, exit.

@@vmpl: cmp bx,0018h                    ; Is it the VMPoll VxD ID number?
        jne short @@old                 ; No, exit.

        test cx,cx                      ; Is it the VMPoll driver?
        jnz short @@old                 ; No, exit.

@@dpmi: call int_xxh_forcehlt           ; Enter power saving mode.
        jmp short @@oldn                ; Chain without zeroing force count.

@@old:  mov [int_xxh_fcount],0          ; Zero int xxh force counter.

@@oldn: pop ds dx
        jmp [dword cs:old_int_2fh]      ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑ INT 14H HANDLER ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

INT_14H_TOPFN   = 03h                   ; Highest FN that is handled.


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTDATA begin

Align 4
old_int_14h     rmdw <0, 0>

int_14h_fntable dw int_xxh_zerocount    ; FN 00h: Init COM port.
                dw int_xxh_zerocount    ; FN 01h: Send char to COM port.
                dw int_14h_normalhlt    ; FN 02h: Read char from COM port.
                dw int_xxh_forcehlt     ; FN 03h: "Char ready?"

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Proc    int_14h_normalhlt
	sti                             ; Enable IRQs for following HLT.

        test [mode_flags],MODE_APM      ; APM usage requested?
        jnz short @@apml                ; Yes.

@@stdl: hlt                             ; Enter power saving mode.
        mov ah,03h                      ; Int 14h FN: Get serial port status.
        pushf                           ;
        call [dword old_int_14h]        ; Simulate int 14h without reentrancy.

        test ah,1                       ; Is data ready?
        jz @@stdl                       ; No, continue loop.
        jmp short @@done                ; Finish.

@@apml: mov ax,5305h                    ;
        pushf                           ;
        call [dword old_int_15h]        ; Call APM FN to put the CPU idle.

        mov ah,03h                      ; Int 14h FN: Get serial port status.
        pushf                           ;
        call [dword old_int_14h]        ; Simulate int 14h without reentrancy.

        test ah,1                       ; Is data ready?
        jz @@apml                       ; No, continue loop.
@@done: ret
Endp


;----------------------------------------------------------------------------;


Align 16
Proc    int_14h_handler                 ; BIOS serial I/O handler.
        push ax bx ds
        mov bx,cs                       ;
        mov ds,bx                       ; CODE = DATA.

        cmp ah,INT_14H_TOPFN            ; FN irrelevant for our handler?
        ja short @@old                  ; Yes, zero force counter and chain.

        xor bh,bh                       ;
        mov bl,ah                       ;
        add bx,bx                       ; BX = index to int_14h_fntable.
        add bx,offset int_14h_fntable   ; BX = offset of handler.

        call [word bx]                  ; Call the appropriate FN handler.
        jmp short @@oldn                ; Chain without zeroing force count.

@@old:  mov [int_xxh_fcount],0          ; Zero int xxh force counter.

@@oldn: pop ds bx ax
        jmp [dword cs:old_int_14h]      ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑ INT 1xH HANDLER ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

RESIDENTDATA begin

Align 4
old_int_10h     rmdw <0, 0>             ;
old_int_15h     rmdw <0, 0>             ; Original vector values.

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Proc    int_10h_handler                 ; BIOS video functions handler.
        mov [cs:int_xxh_fcount],0       ; Zero int xxh force counter.
        jmp [dword cs:old_int_10h]      ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    int_15h_handler                 ; BIOS AT Services handler.
        cmp ax,5305h                    ; APM function: CPU idle called?
        je short @@old                  ; No.

        mov [cs:int_xxh_fcount],0       ; Zero int xxh force counter.

@@old:  jmp [dword cs:old_int_15h]      ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;
;ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ IRQ HANDLERS ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐ;
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ;

RESIDENTDATA begin

Align 4
old_masterirqs  rmdw 8 dup (<0, 0>)     ; Original handlers of the hooked IRQs.

new_masterirqs  rmdw <irq_00_handler, @CODE16>, <irq_01_handler, @CODE16>
                rmdw <irq_02_handler, @CODE16>, <irq_03_handler, @CODE16>
                rmdw <irq_04_handler, @CODE16>, <irq_05_handler, @CODE16>
                rmdw <irq_06_handler, @CODE16>, <irq_07_handler, @CODE16>

RESIDENTDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


RESIDENTCODE begin

Align 16
Proc    irq_00_handler                  ; Handler for IRQ 0 (timer).
        or [cs:irq_flags],IRQ_00        ; Mark that IRQ 0 occurred.
        jmp [dword cs:old_masterirqs]   ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_01_handler                  ; Handler for IRQ 1 (keyboard).
        or [cs:irq_flags],IRQ_01        ; Mark that IRQ 1 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 4]  ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_02_handler                  ; Handler for IRQ 2 (slave PIC).
        or [cs:irq_flags],IRQ_02        ; Mark that IRQ 2 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 8]  ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_03_handler                  ; Handler for IRQ 3 (COM2).
        or [cs:irq_flags],IRQ_03        ; Mark that IRQ 3 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 12] ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_04_handler                  ; Handler for IRQ 4 (COM1).
        or [cs:irq_flags],IRQ_04        ; Mark that IRQ 4 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 16] ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_05_handler                  ; Handler for IRQ 5.
        or [cs:irq_flags],IRQ_05        ; Mark that IRQ 5 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 20] ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_06_handler                  ; Handler for IRQ 6.
        or [cs:irq_flags],IRQ_06        ; Mark that IRQ 6 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 24] ; Chain to old interrupt handler.
Endp


;----------------------------------------------------------------------------;


Proc    irq_07_handler                  ; Handler for IRQ 7.
        or [cs:irq_flags],IRQ_07        ; Mark that IRQ 7 occurred.

        mov [cs:int_xxh_fcount],0          ; Zero int xxh force counter.
        jmp [dword cs:old_masterirqs + 28] ; Chain to old interrupt handler.
Endp

RESIDENTCODE end



;ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป;
;บ ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ INITIALIZATION PART OF PROGRAM ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ บ;
;ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ;

include "_tsrinit.ah"
include "_cmdline.ah"
include "_console.ah"
include "_process.ah"
include "_test.ah"
include "_irq.ah"
include "_vcpi.ah"
include "_cpu.ah"


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


KERNEL_NAME   equ "CPUidle for DOS"     ; Name of the kernel.
KERNEL_FILE   equ "DOSidle"             ; Name of the .exe (compiled) kernel.
KERNEL_ID     equ 0deedh                ; ID number of this program.

SYS_RAW         = 01h                   ;
SYS_VCPI        = 02h                   ; Flags for PM hosts driving the
SYS_DPMI        = 04h                   ; system.


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


INITIALIZATIONSTACK begin
        db 2000 dup (?)                 ; Stack for initialization part.
INITIALIZATIONSTACK end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


INITIALIZATIONDATA begin

psp_seg         dw 0
env_seg         dw 0

dos_version     dw 0                    ; MS-DOS version.
apm_version     dw 0                    ; Advanced Power Management version.
apm_state       db OFF                  ; State of APM (enabled, disabled).

sys_type        db SYS_RAW              ; Type of system (Raw, VCPI, DPMI).

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

par_table       par_item <"-H", par_help>
                par_item <"-?", par_help>
                par_item <"-U", par_uninst>
                par_item <"-ON", par_on>
                par_item <"-OFF", par_off>
                par_item <"-CPU", par_cpu>
                par_item <"-HLT", par_hlt>
                par_item <"-APM", par_apm>
                par_item <"-FM0", par_noforce>
                par_item <"-FM1", par_weakforce>
                par_item <"-FM2", par_strongforce>
		par_item <0>            ; Marks end of par_table.

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

msg_intro       db KERNEL_NAME, " V2.10  [Build 0077]", NL
                db "Copyright (C) by Marton Balog, 1998.", NL,0
msg_help        db "Syntax:    ", KERNEL_FILE, " [Options]", NL
                db "--------", NL,0
msg_options_1   db "Standard   -On     Activate ", KERNEL_FILE, ".", NL
                db "Options:   -Off    Suspend ", KERNEL_FILE, ".", NL
                db "--------   -U      Uninstall ", KERNEL_FILE, ".", NL
                db "           -H, -?  Display this help message.", NL,0
msg_options_2   db "Advanced   -Cpu    Optimize processor for performance.", NL
                db "Options:   -Hlt    Select cooling method: HLT idle cycles (default).", NL
                db "--------   -Apm    Select cooling method: APM V1.00+ cycles.", 0
msg_options_3   db "           -Fm2    Select cooling strategy: Strong Forcing (default).", NL
                db "           -Fm1    Select cooling strategy: Weak Forcing.", NL
                db "           -Fm0    Select cooling strategy: No Forcing.", NL,0
msg_examples_1  db "Example:   ", KERNEL_FILE, "             Install and activate ", KERNEL_FILE, ".", NL
                db "--------   ", KERNEL_FILE, " -Off        Suspend ", KERNEL_FILE, " temporarily.", 0
msg_examples_2  db "           ", KERNEL_FILE, " -Fm2 -Apm   Enable Strong Forcing and use APM for cooling.",NL
                db "           ", KERNEL_FILE, " -Fm1 -Cpu   Enable Weak Forcing and optimize CPU.", NL,0

msg_inst        db NL, KERNEL_FILE, " installed successfully.",0
msg_uninst      db KERNEL_FILE, " uninstalled successfully.",0
msg_activate    db KERNEL_FILE, " is now activated.",0
msg_suspend     db KERNEL_FILE, " is now suspended.",0

msg_marci       db "Zsuzsmi",0
msg_nl          db NL, 0
msg_na          db "N/A",0

msg_detect      db "DETECTING...", 0
msg_cpudet      db "[Processor]:   ", 0
msg_apmdet      db "[Power/Man]:   ", 0
msg_osdet       db "[Op/System]:   ", 0
msg_pmdet       db "[32-bit mode]: ", 0

msg_apm         db "APM V",0
msg_apm_on      db " [Enabled].",0
msg_apm_off     db " [Disabled].",0
msg_msdos       db "MS-DOS V",0
msg_msdos_std   db " [Standard].",0
msg_msdos_win   db " [Windows 95/98].",0
msg_raw         db "16-bit MS-DOS interface.",0
msg_vcpi        db "32-bit VCPI interface.",0
msg_dpmi        db "32-bit DPMI interface.",0

msg_optimize    db NL, "OPTIMIZING...", 0
msg_cpuopt      db "[Processor]:   ", 0
msg_optnomod    db "No modifications made",0 

err_str         db "FATAL ",0
err_resize      db "[#10]: Failed to resize program memory.",0
err_notinst     db "[#20]: ", KERNEL_FILE, " is not installed.",0
err_inst        db "[#21]: ", KERNEL_FILE, " is already installed.",0
err_uninst      db "[#22]: Cannot uninstall ", KERNEL_FILE ,".",0
err_activate    db "[#23]: ", KERNEL_FILE, " is already activated.",0
err_suspend     db "[#24]: ", KERNEL_FILE, " is already suspended.",0
err_cpu         db "[#30]: A 386 CPU or better is required.",0
err_dos_vers    db "[#32]: MS-DOS 5.00 or later is required.",0
err_cmdln       db "[#40]: Invalid command-line switch.",0
err_v86         db "[#50]: CPU in V86 mode and no VCPI or DPMI host present.",0

INITIALIZATIONDATA end


;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ;


INITIALIZATIONCODE begin

Proc    error_exit                      ; Exits with error message.
	push si
	lea si,[err_str]                ;
	call con_writef                 ; Print "[FATAL]: "
	pop si

	sti
	call con_writeln                ; Print error message.
	exit 0                          ; Off we go...
	ret
Endp


;----------------------------------------------------------------------------;


Proc    init
	mov ax,cs                       ;
	mov ds,ax                       ; Set data segment.

	mov [psp_seg],es                ; Save PSP segment.
	mov ax,[es:2ch]                 ;
	mov [env_seg],ax                ; Save enviroment segment.

	lea si,[msg_intro]              ;
	call con_writeln                ; Display program name, copyright...

	call test_cpu                   ; Get CPU family number.
	lea si,[err_cpu]                ;
	cmp al,3                        ; Less than a 386 (286-)?
	jb error_exit                   ; Yep, error.

        mov ax,3000h                    ;
	int 21h                         ; Get DOS version.
        mov [dos_version],ax            ; Save it.

	lea si,[err_dos_vers]           ; Prepare for error.
	cmp al,5                        ; Is DOS new enough (5.00+)?
        jb error_exit                   ; No (4.99-) fail.

        mov ax,[psp_seg]                ; Shrink DOSidle's memory block to
        mov cx,1000h                    ; 64 KBs now, TSR will shrink more
        call mem_lresize                ; later...
	ret
Endp


;----------------------------------------------------------------------------;


Proc    par_help
	lea si,[msg_help]               ;
	call con_writeln                ; Display help message.

        lea si,[msg_options_1]          ;
        call con_writeln                ; Display options help part 1.

        lea si,[msg_options_2]          ;
        call con_writeln                ; Display options help part 2.

        lea si,[msg_options_3]          ;
        call con_writeln                ; Display options help part 3.

        lea si,[msg_examples_1]         ;
        call con_writeln                ; Display examples help part 1.

        lea si,[msg_examples_2]         ;
        call con_writeln                ; Display examples help part 2.
	exit 0
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_uninst
	mov dx,KERNEL_ID                ;
	call tsr_instcheck              ; Is kernel installed already?

	lea si,[err_notinst]            ;
	test ax,ax                      ; AX == 0 if not installed.
	je error_exit                   ; Nope, can't uninstall, error.

	mov dx,KERNEL_ID                ;
        call tsr_uninstall              ; Try to uninstall kernel.

        lea si,[err_uninst]             ; Prepare for error.
        test ax,ax                      ; Uninstallation failed?
        jz error_exit                   ; Yes, fail.

        lea si,[msg_uninst]             ;
	call con_writeln                ; Print success message.
	exit 0                          ; Quit.
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_on
	mov dx,KERNEL_ID                ;
	call tsr_instcheck              ; Is kernel installed already?

	test ax,ax                      ; AX == 0 if not installed.
        jnz short @@on                  ; It's installed, try to activate.

        ret                             ; Do normal install if it's 1st time.

@@on:   mov dx,KERNEL_ID                ;
        call tsr_reactivate             ; Try to reactivate int handlers.

        lea si,[err_activate]           ;
        test ax,ax                      ; Reactivation of ints failed?
        jz error_exit                   ; Yes, fail.

        lea si,[msg_activate]           ;
        call con_writeln                ; Print success message.
        exit 0                          ; Quit.
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_off
	mov dx,KERNEL_ID                ;
	call tsr_instcheck              ; Is kernel installed already?

	lea si,[err_notinst]            ;
	test ax,ax                      ; AX == 0 if not installed.
        jz error_exit                   ; Nope, can't suspend, error.

        mov dx,KERNEL_ID                ;
        call tsr_suspend                ; Try to suspend interrupt handlers.

        lea si,[err_suspend]            ;
        test ax,ax                      ; Suspension of ints failed?
        jz error_exit                   ; Yes, fail.
        
        lea si,[msg_suspend]            ;
        call con_writeln                ; Print success message.
        exit 0                          ; Quit.
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_cpu
        or [mode_flags],MODE_OPTIMIZE   ; Request CPU optimization.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_apm
        or [mode_flags],MODE_APM        ;
        and [mode_flags],not MODE_HLT   ; Set APM MODE in config flags.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_hlt
        or [mode_flags],MODE_HLT        ; 
        and [mode_flags],not MODE_APM   ; Set HLT MODE in config flags.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_noforce
        or [mode_flags],MODE_NOFORCE    ; Set NO FORCE in config flags.

        and [mode_flags],not MODE_WFORCE
        and [mode_flags],not MODE_SFORCE
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_weakforce
        or [mode_flags],MODE_WFORCE     ; Set WEAK FORCE in config flags.

        and [mode_flags],not MODE_SFORCE
        and [mode_flags],not MODE_NOFORCE
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    par_strongforce
        or [mode_flags],MODE_SFORCE     ; Set STRONG FORCE in config flags.

        and [mode_flags],not MODE_WFORCE
        and [mode_flags],not MODE_NOFORCE
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    read_cmdln
	mov di,80h                      ;
	mov es,[psp_seg]                ; ES:DI = ptr to command-line.
	lea si,[par_table]              ; DS:SI = ptr to parameter_table.

	call parse_cmdln                ; Process the command line.
	lea si,[err_cmdln]              ;
	jc error_exit                   ; Quit if invalid command-line switch.
	ret
Endp


;----------------------------------------------------------------------------;


Proc    init_modes
        test [mode_flags],MODE_NOFORCE  ; FORCE MODE disabled?
        jz short @@sfor                 ; No.

        mov [byte int_xxh_forcehlt],0c3h  ; Disable FORCE HLT/APM procedure.

@@sfor: test [mode_flags],MODE_SFORCE   ; STRONG FORCE MODE enabled?
        jz short @@apm                  ; No.

        cmp [sys_type],SYS_DPMI         ; Running under DPMI (possibly Win95)?
        jne short @@apm                 ; No

        cmp [byte dos_version],7        ; Windows MS-DOS 7.00+ (Win95)?
        jb short @@apm                  ; No.

        or [mode_flags],MODE_WFORCE     ; Set WEAK FORCE in config flags.
        and [mode_flags],not MODE_SFORCE
        and [mode_flags],not MODE_NOFORCE

@@apm:  test [mode_flags],MODE_APM      ; APM usage requested?
        jz short @@cpu                  ; No.

        and [mode_flags],not MODE_APM   ; Assume APM is disabled/disengaged.

        cmp [apm_state],OFF             ; APM disabled/disengaged?
        je short @@cpu                  ; Yes, no APM.

        mov ax,5304h                    ;
        xor bx,bx                       ;
        int 15h                         ; Disconnect Real-mode APM interface.
        jc short @@cpu                  ; Call failed, no APM.

        mov ax,5301h                    ;
        xor bx,bx                       ;
        int 15h                         ; Connect Real-mode APM interface.
        jc short @@cpu                  ; Call failed, no APM.

        mov ax,530eh                    ;
        xor bx,bx                       ;
        mov cx,[apm_version]            ;
        xchg cl,ch                      ; Connect appropriate version of APM
        int 15h                         ; BIOS (needed for V1.00+).
        jc short @@cpu                  ; Call failed, no APM.

        mov ax,5305h                    ;
        int 15h                         ; Call APM FN to put the CPU idle.
        jc short @@cpu                  ; Call failed, no APM.
        
        or [mode_flags],MODE_APM        ; It's safe to use APM...

@@cpu:  test [mode_flags],MODE_OPTIMIZE ; CPU optimization requested?
        jz short @@done                 ; No.

        lea si,[msg_optimize]           ;
        call con_writeln                ; Print optimization message.

        lea si,[msg_cpudet]             ;
        call con_writef                 ; Print message for CPU detection.

        call cpu_optimize               ; Optimize CPU.
        lea si,[msg_optnomod]           ; Assume optimization failed.
        jc short @@prnt                 ; Go..

        call cpu_getname                ;
        lea si,[cpu_name]               ; Get full name of CPU.

@@prnt: call con_writef                 ; Print results of optimization.

        mov al,'.'                      ;
        call con_writech                ; Period.

        mov al,CR                       ;
        call con_writech                ;
        mov al,LF                       ;
        call con_writech                ; New line.

@@done: ret
Endp


;----------------------------------------------------------------------------;


Proc    check_system
	call test_vcpi                  ; Running under VCPI server?
	jne short @@dpmi                ; No.

        mov [sys_type],SYS_VCPI         ; Mark that running under VCPI.
        jmp short @@done

@@dpmi: call test_dpmi                  ; Running under DPMI host?
	jne short @@v86                 ; No.

        mov [sys_type],SYS_DPMI         ; Mark that running under DPMI.
        jmp short @@done

@@v86:  call test_v86                   ; Running in V86 mode without PM host?
	lea si,[err_v86]                ; Yes, can't execute HLT instruction,
	je error_exit                   ; fail program.
@@done: ret
Endp


;----------------------------------------------------------------------------;


Proc    hook_ints
	xor ax,ax			;
	mov gs,ax			; GS = segment of IVT.

        ;-  -  -  -  -  -  -  -  -  -  -;
        mov eax,[gs:(10h * 4)]          ;
        mov [dword old_int_10h],eax     ; Get and save original int 10h.

        mov eax,[gs:(15h * 4)]          ;
        mov [dword old_int_15h],eax     ; Get and save original int 15h.

        mov eax,[gs:(14h * 4)]          ;
        mov [dword old_int_14h],eax     ; Get and save original int 14h.

        mov eax,[gs:(16h * 4)]          ;
	mov [dword old_int_16h],eax     ; Get and save original int 16h.

	mov eax,[gs:(21h * 4)]          ;
	mov [dword old_int_21h],eax     ; Get and save original int 21h.

        mov eax,[gs:(2fh * 4)]          ;
        mov [dword old_int_2fh],eax     ; Get and save original int 2fh.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
        mov ax,@CODE16                  ;
        shl eax,16                      ; High WORD of EAX = CODE16.

        mov bl,10h                      ; BL = int number of video handler.
        mov ax,offset int_10h_handler   ; EAX = new handler for int 10h.
        call tsr_hookint                ; Hook int 10h.

        mov bl,15h                      ; BL = int number of AT services.
        mov ax,offset int_15h_handler   ; EAX = new handler for int 15h.
        call tsr_hookint                ; Hook int 15h.

        mov bl,14h                      ; BL = int number of BIOS COM handler.
        mov ax,offset int_14h_handler   ; EAX = new handler for int 14h.
        call tsr_hookint                ; Hook int 14h.

        mov bl,16h                      ; BL = int number of keyboard handler.
        mov ax,offset int_16h_handler   ; EAX = new handler for int 16h.
        call tsr_hookint                ; Hook int 16h.

        mov bl,21h                      ; BL = int number of DOS FNs handler.
        mov ax,offset int_21h_handler   ; EAX = new handler for int 21h.
        call tsr_hookint                ; Hook int 21h.

        mov bl,2fh                      ; BL = int # of DOS Multiplex handler.
        mov ax,offset int_2fh_handler   ; EAX = new handler for int 2fh.
        call tsr_hookint                ; Hook int 2fh.
        ;-  -  -  -  -  -  -  -  -  -  -;
        ret
Endp


;----------------------------------------------------------------------------;


Proc    hook_irqs
	xor ax,ax			;
	mov gs,ax			; GS = segment of IVT.

        ;-  -  -  -  -  -  -  -  -  -  -;
@@vcpi: cmp [sys_type],SYS_VCPI         ; Running under VCPI?
        jne short @@dpmi                ; No.

        call vcpi_getpic                ; Get VCPI IRQ mappings.
        jmp short @@hook

@@dpmi: cmp [sys_type],SYS_DPMI         ; Running under DPMI?
        jne short @@raw                 ; No.

        call irq_getpic                 ; Assume RM IRQ settings (should work).
        jmp short @@hook

@@raw:  call irq_getpic                 ; Get IRQ mappings.
        ;-  -  -  -  -  -  -  -  -  -  -;

        ;-  -  -  -  -  -  -  -  -  -  -;
@@hook: movzx ebx,bl                    ; EBX = base int # for master PIC.
        mov cx,8                        ; CX = number of IRQ in master PIC.
        xor di,di                       ; DI = index to irq arrays.

@@mstr: mov eax,[gs:(ebx * 4)]               ;
        mov [dword old_masterirqs + di],eax  ; Get and save old IRQ handler.

        mov eax,[dword new_masterirqs + di]  ; Get new handler of IRQ.
        call tsr_hookint                ; Hook IRQ.
        inc bl                          ; BL = next interrupt # for IRQ.
        add di,4                        ; DI = next IRQ number.
        loop @@mstr
        ;-  -  -  -  -  -  -  -  -  -  -;
@@done: ret
Endp


;----------------------------------------------------------------------------;


Proc    detect_cpu
        lea si,[msg_cpudet]             ;
        call con_writef                 ; Print message for CPU detection.

        call cpu_getname                ; Get full name of CPU.
        lea si,[cpu_name]               ;
        call con_writef                 ; Print it.

        mov al,'.'                      ;
        call con_writech                ; Period.

        lea si,[msg_nl]                 ;
        call con_writef                 ; New Line.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    detect_os
        lea si,[msg_osdet]              ;
        call con_writef                 ; Print message for OS detection.

        lea si,[msg_msdos]              ;
        call con_writef                 ; Print "MS-DOS V"

        movzx eax,[byte dos_version]    ;
        call con_writedec               ; Print major version number.

        mov al,'.'                      ;
        call con_writech                ; Put a decimal point for version.

        movzx eax,[byte dos_version+1]  ;
        call con_writedec               ; Print minor version number.

        lea si,[msg_msdos_std]          ; Assume MS-DOS V6.22-
        cmp [byte dos_version],7        ; Is it V7.00+ (for Win95/98)?
        jb short @@osok                 ; No.

        lea si,[msg_msdos_win]          ; It's V7.00+ (for Win95/98).

@@osok: call con_writeln                ; Print DOS type.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    detect_apm
        lea si,[msg_apmdet]             ;
        call con_writef                 ; Print message for APM detection.

        stc                             ;
        mov ax,5300h                    ;
        mov bx,0                        ;
        int 15h                         ; APM V1.00+: installation check.
        jnc short @@cont                ; Yes, it's installed, continue.
        
        lea si,[msg_na]                 ;
        call con_writeln                ; No APM, print N/A message.
        jmp short @@done                ; Finish.

@@cont: xchg al,ah                      ; AL = major version, AH = minor.
        mov [apm_version],ax            ; Save APM version.

        lea si,[msg_apm]                ;
        call con_writef                 ; Print "APM V"

        movzx eax,[byte apm_version]    ;
        call con_writedec               ; Print major version number.

        mov al,'.'                      ;
        call con_writech                ; Put a decimal point for version.

        movzx eax,[byte apm_version+1]  ;
        call con_writedec               ; Print minor version number.

        test cx,18h                     ; Is the APM disabled/disengaged?
        jnz short @@off                 ; Yes.

@@on:   mov [apm_state],ON              ; Mark that APM is unusable.
        lea si,[msg_apm_on]             ; It's enabled.
        jmp short @@done                ; Finish up.

@@off:  mov [apm_state],OFF             ; Mark that APM is unusable.
        lea si,[msg_apm_off]            ; It's disabled.

@@done: call con_writeln                ; Print APM state.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    detect_pm
        lea si,[msg_pmdet]              ;
        call con_writef                 ; Print message for PM detection.

        lea si,[msg_raw]                ; Assume raw DOS.
        test [sys_type],SYS_RAW         ; Running under raw DOS?
        jnz short @@pmok                ; Yes, done.

        lea si,[msg_vcpi]               ; Assume VCPI.
        test [sys_type],SYS_VCPI        ; Running under VCPI?
        jnz short @@pmok                ; Yes, done.

        lea si,[msg_dpmi]               ; Now it's DPMI for sure.
@@pmok: call con_writeln                ; Print PM system.
        ret
Endp

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

Proc    info_detect
        lea si,[msg_detect]             ;
        call con_writeln                ; Print detection message.

        call detect_cpu                 ; Print CPU detection results.
        call detect_apm                 ; Print APM detection results.
        call detect_os                  ; Print OS detection results.
        call detect_pm                  ; Print PM host detection results.
        ret
Endp


;----------------------------------------------------------------------------;


Proc    install_kernel
	lea si,[msg_inst]               ;
	call con_writeln                ; Print success message.

        mov cx,RESIDENT_END             ;
	mov dx,KERNEL_ID                ;
	mov bx,[psp_seg]                ;
	mov ax,[env_seg]                ;
	call tsr_install                ; Make kernel TSR.
Endp


;----------------------------------------------------------------------------;


Proc    main
	call init                       ; Do general startup work.
	call read_cmdln                 ; Read cmd-ln params (maybe uninstall).

	mov dx,KERNEL_ID                ;
	call tsr_instcheck              ; Is kernel installed already?

	lea si,[err_inst]               ;
	cmp ax,1                        ; AX == 1 if installed.
	je error_exit                   ; Yes, quit now (don't install twice).

        call check_system               ; Check for VCPI, DPMI, etc.
        call info_detect                ; Detect CPU, system, APM, etc.
        call init_modes                 ; Init FORCE, Test and other modes.
        
        call hook_ints                  ; Hook needed interrupts.
        call hook_irqs                  ; Hook needed IRQs.

        call cpu_powersave              ; Enable power saving features.
        call install_kernel             ; Make kernel TSR.
Endp

INITIALIZATIONCODE end
End     main


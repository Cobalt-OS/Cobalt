;************************************************************************
;
;  KLUDGE0.ASM
;
;    Some CD programs will only run if the CD is unit 0 on the CD driver.
;      This Kludge makes a CD appear as unit zero on a new driver CD0.
;
;  Use:  KLUDGE0 [/D:drivername[,unit]]  [/U] [/?]
;        default drivername is SHSU-CDN
;        default unit on drivername that is to be unit 0 on CD0
;
;  Assemble using MASM 6.0 and link as an .exe file.
;
;  KLUDGE0 is a copyright-reserved, free use program.
;  (c)John H. McCoy, 1995, Sam Houston St. Univ., TX 77341-2206
;************************************************************************

.model small, os_dos

option nokeyword:<length name>
option expr16

fptr  typedef  far ptr word
nptr  typedef  near ptr word


devHdr  struct
  NextDriver   dword     -1
  Attributes   word      0C800h        ; for a cdrom
  Strategy     nptr      ?
  Interrupt    nptr      ?
  DeviceName   byte      '        '    ; 8 bytes
               word      0             ; CDROM reserved
               byte      0             ; CDROM drive letter
  SubUnits     byte      1             ; number of CD drives on this device
devHdr ends

rh    struc
   Length      byte ?          ; header size in bytes
   SubUnit     byte ?          ; cd drive unit
   Command     byte ?          ; device command code
   Status      word ?          ; device command status
   Reserved    byte 8 dup(?)
rh    ends

rhIOCTL   struc
                     byte size rh dup(?) ;rh common
   MediaDesc         byte ?
   CBPtr             fptr ?
   BytesToTransfer   word ?
   StartSector       word ?
   VolIdPtr          fptr ?
rhIOCTL   ends

rhcmdIOCTL_In          equ 3
rhcmdOpen              equ 0Dh
rhcmdClose             equ 0Eh
rhcmdPreFetch          equ 82h

IOCtl_In_RDHACmd          equ  0

IoCB_RDHA     struc
   IoctlCommand           byte IOCtl_In_RDHACmd
   DeviceHeaderAddress    fptr ?
IoCB_RDHA ends

DeviceError            equ  8000h
DeviceDone             equ  0100h
DE_UnknownUnit         equ    01h   ; OR these with DeviceError
DE_DeviceNotReady      equ    02h
DE_UnknownCommand      equ    03h


AsciiNul                equ     0
cr                      equ     13
lf                      equ     10
ht                      equ     09
QMark                   equ     '?'
T                       equ     0
F                       equ     -1

Display   MACRO  msg
         mov     dx,offset msg
         mov     ah,9
         int     21h
ENDM

GetDriverInfo     proto near

.code

   assume cs:@code,  ds:@code

;  dos device header with CDROM extension fields
;  DO NOT MAKE THE DEVICE DRIVER NAME THE SAME AS THE FILE NAME

KludgeHdr  devHdr  {-1,0C800h,Strategy,Interrupt,'CD0     ',0,0,1}

rhAddr        label far ptr
  rhOffset      word       ?
  rhSegment     word       ?

; redirection info
DrvrName       byte     'SHSU-CDN'
DrvrHeader     fptr     ?
DrvrStrategy   fptr     ?
DrvrInterrupt  fptr     ?
DrvrUnits      byte     ?
DrvrUnit       byte     0
_DS            word     ?
Author           byte     "John H. McCoy"

;************************************************************************
;  Driver Strategy routine
;************************************************************************

Strategy    proc  far

   ;  es:bx contains request header pointer.  save it.
      mov    cs:rhOffset, bx
      mov    cs:rhSegment, es
      ret

Strategy    endp

;************************************************************************
;  Driver Interrupt routine
;************************************************************************

Interrupt    proc  far

   ;  process command
      les      bx, cs:rhAddr                ; make sure we have rh addr
      mov      al, es:[bx].rh.SubUnit
      .if   (al != 0)
            mov   ax,(DeviceDone OR DeviceError OR DE_UnknownUnit)
            mov   es:[bx].rh.Status, ax
            ret
      .endif
   ;  save ds and setup ds addressing
      mov    cs:_DS,ds
      push   cs
      pop    ds

      mov      al, es:[bx].rh.command
      mov      al, es:[bx].rh.command
      .if  (al == rhcmdIOCTL_In)
        les      bx, es:[bx].rhIOCTL.CBPtr
        mov      al, byte ptr es:[bx]           ; 1st byte of dta is subcommand
        .if      al == IOCtl_In_RDHACmd
          lea   ax, KludgeHdr
          mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress, ax
          mov   ax, ds
          mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress+2, ax
          mov   ax, DeviceDone
          les   bx, rhAddr                ; restore rh ptr
          jmp   Xit
        .else
          ;  Restore rhptr pass it on to driver
          les   bx, rhAddr                ; restore rh ptr
          jmp   CallDriver
        .endif
      .elseif   ( (al == rhcmdOpen)||(al ==rhcmdClose) )
            mov   ax,(DeviceDone)
            mov   es:[bx].rh.Status, ax
            jmp   Xit
      .endif
  CallDriver:
        mov   al,DrvrUnit
        mov   es:[bx].rh.SubUnit,al
        call  DrvrStrategy
        les   bx, cs:rhAddr                ; reload rh addr
        call  cs:DrvrInterrupt
        les   bx, cs:rhAddr                ; reload rh addr
        mov   es:[bx].rh.SubUnit,0         ; restore original subunit

Xit:
        mov   ds,cs:_DS
        ret
Interrupt   endp

     byte "End of KLUDGE0"

LastByte               label byte
;============================================================================
;  everything below this line is discarded after installing the driver

ArgumentNotFound        EQU     2       ; Unrecognized argument
NoArgumentsFound        EQU     1       ; No argument in command line
ArgumentFound           EQU     0       ; Ok argument in command line

IOCtlInBuf       byte     5 dup(?)

UnInstall               equ     2
DontInstall             equ     1
Install                 equ     0

InstallFlag             byte    DontInstall
PSP                     word    ?
_SP                     word    ?
_SS                     word    ?

InstallMsg  db cr,lf,"Kludge Installed.",cr,lf
            db "Copyright 1995, John H. McCoy.","$"
UnInstallMsg       db " Kludge Uninstalled and memory freed",cr,lf,"$"
CouldNotRemoveMsg  db "Can't un-install Kludge.","$"
NotInstalledMsg db "Kludge not installed.","$"
InvalidDriverUnitMsg db "Driver Unit specified does not exist",cr,lf,"$"
HelpMsg db  cr,lf,"Usage:  KLUDGE0 [/D:drivername[,unit]]  [/U] [/?]",cr,lf
        db  "Installs as CD0 with drivername,unit as unit 0",cr,lf
        db  "Default drivername,unit is SHSU-CDN,0",cr,lf,lf,"$"


Init  proc far
      mov      cs:PSP, ds            ; save PSP
      mov      ax, cs                ; now set DS to CS
      mov      ds, ax

      mov      InstallFlag, Install
      call ParseCommandLine
      .if InstallFlag == Install
          mov    InstallFlag, DontInstall
          invoke GetDriverInfo
          .if InstallFlag== DontInstall
              jmp Dont
          .endif
          mov  al, DrvrUnit
          .if (al > DrvrUnits)
              Display  InvalidDriverUnitMsg
              jmp Dont
          .endif
          mov    InstallFlag, DontInstall
          call Link
          .if (InstallFlag == DontInstall)
              jmp Dont
          .endif
              Display  InstallMsg
          mov    ax, PSP
          mov    ds, ax
          mov    ax, ds:[2Ch]           ; find environment and release it
          mov    es, ax
          mov    ah, 49h
          int    21h
          sub    ax, ax
          mov    ds:[2Ch], ax           ; zero the evironment ptr

          if  (LastByte-KludgeHdr+1) mod 16
              roundup = 1
          else
              roundup = 0
          endif

          mov     dx,((LastByte-KludgeHdr)+1+100h)/16+roundup; para to keep
          mov     ah,31h        ; stay resident and
          int     21h             ; exit
      .elseif InstallFlag == UnInstall
          call UnInstallKludge              ; es points to our psp
          jmp   Xit
      .endif

   Dont:
      Display  NotInstalledMsg
   XIT:
      .exit

Init endp

GetDriverInfo  proc near uses es
      ; Open device driver and use IOCTL Input to get
      ;   the device header address.
      mov    ax, ds
      mov    es, ax
         lea      dx, DrvrName
         sub      al, al                     ; read only
         mov      ah, 3Dh                    ; open
         int      21h
         jnc      @f                         ; error when carry set
         jmp      CantFindCd
      @@:mov      bx, ax                     ; move handle to bx
         mov      ax, 4402h                  ; IOCTL input-get devhdr addr
         mov      cx, 5                      ; dta has cmd code plus a fptr
         lea      dx, IOCtlInBuf             ;   to device header
         int      21h
         mov      ah, 3eh                    ; close file handle in bx
         int      21h
         jnc      @f                         ; error when carry set
         jmp      CantFindCd                 ; shouldn't, but just in case
      @@:
         mov      InstallFlag, Install
         les      bx, dword ptr IoctlInBuf[IoCB_RDHA.DeviceHeaderAddress]
         mov      word ptr DrvrHeader[si], bx
         mov      word ptr DrvrHeader[2], es
         mov      ax, es:devHdr.Strategy[bx]
         mov      word ptr DrvrStrategy, ax
         mov      word ptr DrvrStrategy[2], es
         mov      ax, es:devHdr.Interrupt[bx]
         mov      word ptr DrvrInterrupt, ax
         mov      word ptr DrvrInterrupt[2], es
         mov      ah, es:[bx+21]
         jmp   @F
       CantFindCD:
         mov      ah, 0
      @@:
         mov   DrvrUnits, ah
     ret
GetDriverInfo  endp


Link   proc near uses es
      mov      ax, 5200h               ; get list of list
      int      21h                     ; we assume DOS 3.1 or later
      add      bx, 22h                 ; es:bx[22] is NUL device header
      mov      ax, es:[bx]               ; put NUL.next in our header
      mov      word ptr KludgeHdr.NextDriver, ax
      mov      ax, es:[bx+2]
      mov      word ptr KludgeHdr.NextDriver+2, ax
      mov      ax, 0                   ; then point NUL header at us
      mov      es:[bx], ax
      mov      es:[bx+2], cs
      mov      InstallFlag, Install
      ret
Link   endp

UnInstallKludge proc near

   local _bx,_es:word

      push     es                      ; save our psp address
      mov      ax, 5200h               ; get list of list
      int      21h                     ; we assume DOS 3.1 or later
      add      bx, 22h                 ; es:bx[22] is NUL (1st) device header
                                       ; es:bx now pointing at NUL header
   TryNext:
      cld
      mov      _bx, bx                 ; save current header addr
      mov      _es, es
      les      bx, es:[bx]             ; load next header addr into es:bx
      mov      ax, es
      cmp      bx, 0FFFFh              ; end of drivers?
      je       DriverNotInstalled
      mov      cx, 8
      lea      di, devHdr.DeviceName[bx]       ; es:di is chained device name
      mov      si, offset KludgeHdr.DeviceName   ; ds:si is our device name
      repe     cmpsb                   ; if equ its the one we are looking for
      jne      TryNext
      push     ds
      mov      ax, es                  ; es:bx is addr of driver being removed
      mov      ds, ax                  ; put it into ds:si
      mov      si, bx                  ;
      mov      cx, 4                   ;
      mov      es, _es                 ;
      mov      di, _bx                 ; previous header now in es:di
      rep      movsb                   ; move address ds:si -> es:di
      mov      es, ax                  ; es now points at unlinked driver
      pop      ds                      ; cs=ds=@code
      mov      ax, es                   ; locate the
      sub      ax, 10h                  ; psp of installed driver
      mov      es, ax                   ;
      mov      bx, 16h                  ; installed drivers parent psp pointer
      pop      ax                       ; our psp address(pushed es above)
      mov      es:[bx],ax               ; make us parent of TSR
      lea      ax, UnInstallExit        ; set TSRs
      mov      bx, 0Ah                  ; terminate address
      mov      es:[bx], ax              ; to come back to
      mov      ax, cs
      mov      es:[bx]+2, ax            ; us
      mov      bx, es                   ; now make TSRs psp the
      mov      ah, 50h                  ; current psp
      int      21h

      push     bp
      mov      _SS, ss                  ; save stack info
      mov      _SP, sp
      mov      ah, 4Ch                  ; terminate TSR and
      int      21h                      ; come back to next

    UnInstallExit:
      mov      ax, cs
      mov      ds, ax                   ; reestablish addressing
      mov      sp, _SP                  ; and stack info
      mov      ss, _SS
      pop      bp

      display UnInstallMsg
      ret

  DriverNotInstalled:
      Display  NotInstalledMsg
      ret

UnInstallKludge endp

ParseCommandLine       proc near  uses es
   ;* If driver is loaded from config.sys using device=drivername parms
   ;* then rhINIT points to the first character following the drivername
   ;* and a CR follows the last parm.  When loaded by executing, the
   ;* command line is available in the PSP.(len +80h, 1st ch +81h, no CR)

      sub      ch, ch
      mov      di, 80h            ; command line length psp +80h
      mov      cl, es:[di]
      mov      al, 'U'            ; /U unInstall driver
      call     GetParm
      .if ax == ArgumentFound
          mov      InstallFlag, UnInstall
          jmp      ParseExit
      .endif
      sub      ch, ch
      mov      di, 80h            ; command line length psp +80h
      mov      cl, es:[di]
      mov      al, '?'            ; /? help
      call     GetParm
      .if ax == ArgumentFound
          Display HelpMsg
          mov      InstallFlag, DontInstall
          jmp      ParseExit
      .endif

      mov      di, 80h                ; command line length at psp +80h
      sub      ch, ch
      mov      cl, es:[di]
      mov      al, 'D'            ; /D:drivername
      call     FindParm
     .if   ax == ArgumentFound
         xor     si,si
         mov     dx, sizeof DrvrName
         call    MoveName
         mov al, es:[di]
         .if (cx == 0)||(al != ',')               ; end of command line or no drive unit
             jmp  ParseExit
         .endif
         dec    cx
         jcxz   ParseExit
         inc    di
         mov al, es:[di]
         .if (al >= '0' && al <= '9')
                sub    al, '0'
                mov ah, es:[di+1]                   ; is it two digits?
               .if (ah >= '0' && ah <= '9')         ; ignore if not a digit
                   shl    al,1                      ; mul 1st digit by 10
                   mov    ah, al
                   shl    al,1
                   shl    al,1
                   add    al, ah
                   mov    ah, es:[di+1]             ; and add in 2nd digit
                   sub    ah, '0'
                   add    al, ah
                   dec    cx
                   inc    di
               .endif
                mov    [si+DrvrUnit], al
         .endif
     .endif
  ParseExit: ret

ParseCommandLine       endp

MoveName proc near
      sub   bx, bx                                 ; es:di points to 1st char
      .repeat                                      ; cx chars left on cmd line
          mov al, es:[di]                          ; dx is length of name field
          .if ((al == ',') || (cx == 0) || (al==' ') || (al == '/'))
              mov    byte ptr [si+bx+DrvrName], ' '
          .else
             .if (al >= 'a' && al <= 'z')
                 and    al, 11011111y           ; upper case it
              .endif
              mov    byte ptr [si+bx+DrvrName], al
              inc    di
              dec    cx
          .endif
          inc    bx
      .until bx == dx
      ret
MoveName endp

FindParm proc near

   ; al      parm code we are to find       /X: or -X:
   ; es:di   first char on command line -1
   ; cx      number of characters left on command line

 GetNext:                             ; this code allows us to handle names
      call     GetParm                ; like   -C:NET-CD
      cmp      ax, ArgumentFound
      jne      NotFound
      inc      di                     ; found /X or -X, is next char a ':' ?
      dec      cl
      mov      al, es:[di]
      cmp      al, ':'
      je       FoundIt
      loop     GetNext
      mov      ax, ArgumentNotFound
      ret

  FoundIt:
      inc   di                           ; /X:name  make di point @ name
      dec   cl
      mov   ax, ArgumentFound
  NotFound:
   ret

FindParm endp

;* GetParm - Scans command line for argument of form /X or -X  where
;* X = specified ASCII character. Presumes that argument is preceded
;* by a '/' or a '-'. Comparisons are case insensitive.
;*
;* Params: ES:DI = Address of CommandLine -1
;*         AL    = Paramater character to scan for
;*         CX    = command line length
;*
;* Return: AX    = One of the following codes:
;*                 NoArgumentsFound  if empty command line
;*                 ArgumentFound  if argument found
;*                 ArgumentNotFound if argument not as specified
;*         ES:DI = Pointer to found argument
;*         CX    = chars left on command line including arg or 0

GetParm PROC NEAR

        mov     ah, NoArgumentsFound    ; assume no /X style arguments
        jcxz    exit
        .if (al >= 'a' && al <= 'z')
            and    al, 11011111y           ; Make character upper case
        .endif

; Find start of argument

loop1:
        inc     di                      ;
        mov     dl, es:[di]             ; Get character from argument list
        cmp     dl, '/'                 ; Find option prefix '/'
        je      analyze
        cmp     dl, '-'                 ;   or option prefix '-'
        je      analyze

        loop    loop1

        jmp     exit

; '/' or '-' prefix found. Compare command-line character
; with character specified in AL.
analyze:
        inc     di
        dec     cl
        jcxz    exit
        mov     ah, ArgumentFound         ; Assume argument is okay
        mov     dl, es:[di]
        .if (dl >= 'a' && dl <= 'z')
            and    dl, 11011111y           ; Make character upper case
        .endif
        cmp     dl, al
        je      exit                    ; specified char
        mov     ah, ArgumentNotFound    ; Else signal bad argument,
        loop    loop1             ;   continue scan

exit:
        mov     al, ah
        cbw                             ; AX = return code
        ret

GetParm ENDP


.stack
        byte "This is here to keep loadhigh happy if NETX is loaded."


        end  Init

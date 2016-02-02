;************************************************************************
;
;  CDN.ASM
;    An executable, driver for networking MSCDEX to a remote
;      CDNET drive using NETBIOS.  Execute before MSCDEX.
;    Requires CdServer be active on server before activating.
;
;    UsageMsg:
;      SHSUCDN [/?][/C:ClientName][/S:ServerName][/D:DriverName][/Q][/U]
;
;  Assemble using MASM 6.0 and link as an .exe file.
;  Requires:  NETBIOS.INC
;
;  CDN is a copyright-reserved, free use program.
;  (c)John H. McCoy, 1993, Sam Houston St. Univ., TX 77341-2206
;************************************************************************

.model tiny, os_dos

option nokeyword:<length name>
option expr16

fptr  typedef  far ptr word
nptr  typedef  near ptr word

rh    struc
   Length      byte ?          ; header size in bytes
   SubUnit     byte ?          ; cd drive number
   Command     byte ?          ; device command code
   Status      word ?          ; device command status
   Reserved    byte 8 dup(?)
rh    ends

rhINIT   struc
                     byte size rh dup(?) ;rh common
   NumberUnits       byte  ?
                     dword ?
                     dword ?
                     byte  ?
rhINIT   ends

rhIOCTL   struc
                     byte size rh dup(?) ;rh common
   MediaDesc         byte ?
   CBPtr             fptr ?
   BytesToTransfer   word ?
   StartSector       word ?
   VolIdPtr          fptr ?
rhIOCTL   ends

IOCtl_RDHACommand       equ  0

IoCB_RDHA     struc                     ; Ioctl_0  command block structure
   IoctlCommand           byte IOCtl_RDHACommand
   DeviceHeaderAddress    fptr ?
IoCB_RDHA ends

rhTransfer  struc
                     byte size rh dup(?) ;rh common
                     byte   ?
   DtaPtr            fptr   ?
   SectorCount       word   ?
   StartSector       Dword  ?
   ReadMode          byte   ?     ; we support cooked mode only
                     byte   ?
                     byte   ?
rhTransfer ends


LastDosDriverCommand      equ  14    ; varies with DOS version
FirstCDExtendedCommand    equ  128

DeviceError            equ  8000h
DeviceDone             equ  0100h
DE_UnknownCommand      equ    03h    ; OR with DeviceError
DE_ReadError           equ    0Bh
DE_GeneralFailure      equ    0Ch

AsciiNul                equ     0
cr                      equ     13
lf                      equ     10
QMark                   equ     '?'

Display   MACRO  msg
         mov     dx,offset msg
         mov     ah,9
         int     21h
ENDM

; ** NetBIOS.INC prototypes

    NetAddName proto near syscall Name:nptr
    NetCall    proto near syscall LocalName:nptr, RemoteName:nptr
    NetDelName proto near syscall Name:nptr
    NetReceive proto near syscall DTA:fptr, Length:word, Session:byte
    NetHangup  proto near syscall Session:byte
    NetSend    proto near syscall DTA:fptr, Length:word, Session:byte

.code

   assume cs:@code,  ds:@code

;  dos device header with CDROM extension fields

DevHeader  label word
  NextDriver     dword     -1
  Attributes     word      0C800h
                 nptr      Strategy
                 nptr      Interrupt
  DeviceName     byte      'SHSU-CDN'
                 word      0                     ; CDROM reserved
                 byte      0                     ; 1st CDROM drive letter
  SubUnits       byte      1                     ; number of drives

LocalSession     byte      0

;  strategy call saves req hdr addr here for use by interrupt call

rhAddr        label far ptr
  rhOffset      word       ?
  rhSegment     word       ?

rhBuf         byte size rhINIT, (size rhINIT -1) dup (0)

;  work space for oversize readlong

DtaPtrSave          Dword      ?
SectorCountSave     word       ?
StartSectorSave     Dword      ?
SectorsRead         word       ?

;  CB size


IoCB_InSize          byte  5,6,1,1,9,130,5,4,5,2,7,7,11,13,11,11
IoCB_OutSize         byte  1,2,1,9,130,1

ClientNameFlag   byte       0

ClientName       byte       'CD-CLIENT       '
                 byte       "$"

ServerName       byte       'SHSU-CD-SERVER  '
                 byte       "$"

DriverName       byte       'SHSU-CDN'
                 byte       "$"

Author           byte       "John H. McCoy"

;************************************************************************
;  Driver Strategy routine
;************************************************************************

Strategy    proc     far

   ;  es:bx contains request header pointer.  save it.
      mov    cs:rhOffset, bx
      mov    cs:rhSegment, es
      ret

Strategy    endp

;************************************************************************
;  Driver Interrupt routine
;************************************************************************

Interrupt    proc  far uses ax bx cx dx si di ds es

   ;  setup ds addressing
      push   cs
      pop    ds
   ;  process command
      mov      al, es:[bx].rh.command
      xor      ah, ah
      .if      al >= FirstCDExtendedCommand
          sub      al, 128
          .if      al > 8          ; 128 thru 136 valid CDROM Commands
              jmp   UnknownCommand
          .endif
          shl      ax, 1       ; convert to word offset for jmp table
          mov      si, ax
          jmp      CDExtendedCommandTable[si]

          CDExtendedCommandTable   label word
                 nptr offset ReadLong
                 nptr offset UnknownCommand
                 nptr offset ReadPrefetch
                 nptr offset Seek
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand

      .elseif   al <= LastDosDriverCommand

          shl      ax, 1       ; convert to word offset for jmp table
          mov      si, ax
          jmp      DosDriverCommandTable[si]

          DosDriverCommandTable label word
                 nptr offset UnknownCommand        ; we do init when loading
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset IoctlInput
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset UnknownCommand
                 nptr offset IoctlOutput
                 nptr offset DevOpen
                 nptr offset DevClose
                 nptr offset UnknownCommand

      .endif

IoctlInput:
      les      bx, es:[bx].rhIOCTL.CBPtr
      mov      al, byte ptr es:[bx]           ; 1st byte of dta is subcommand
      .if  (al == 2) || (al > 15) || (LocalSession == 0)
      ;  invalid sub-command or not conected to remote CD
         mov   ax, (DeviceDone OR DeviceError OR DE_UnknownCommand)
         jmp   ExitRestoreRH
      .endif
      .if      al == IOCtl_RDHACommand
         lea   ax, DevHeader
         mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress, ax
         mov   ax, ds
         mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress+2, ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .endif
    ;  valid command and connected.  Lookup IoCB_InSize since some
    ;    callers apparently don't tell us.  Do it here since we need
    ;    it to bring back the CB.
      lea      bx, IoCB_InSize
      xlat
      cbw
      mov      dx, ax
      les      bx, rhAddr
      mov      es:[bx].rhIOCTL.BytesToTransfer, ax
    ;  send rh and then control block to remote CD
    ;  first the rh
      INVOKE NetSend, es::bx, es:[bx].rh.length, LocalSession
      .if      al != NB_Ok
          jmp     IoctlError
      .endif
    ;  then the CB
      les      bx, es:[bx].rhIOCTL.CBPtr
      INVOKE NetSend, es::bx, dx, LocalSession
      les      bx, rhAddr
      .if      al != NB_Ok
          jmp     IoctlError
      .endif
    ;  rh comes back first
      INVOKE NetReceive, es::bx, es:[bx].rh.length, LocalSession
      .if      al != NB_Ok
          jmp     IoctlError
      .endif
    ;  then the CB if device done
      .if      es:[bx].rh.Status == DeviceDone
         les     bx, es:[bx].rhIOCTL.CBPtr
         INVOKE NetReceive, es::bx, dx, LocalSession
         les      bx, rhAddr
         .if      al == NB_Ok
            jmp     Exit0          ; returns device status to MSCDEX
         .endif
      .endif
      jmp     IoctlError

IoctlOutput:
      les      bx, es:[bx].rhIOCTL.CBPtr
      mov      al, byte ptr es:[bx]           ; 1st byte of dta is subcommand
      .if     (al > 5) || (LocalSession == 0)
      ;  invalid sub-command or not conected to remote CD
         mov      ax, (DeviceDone OR DeviceError OR DE_UnknownCommand)
         jmp      ExitRestoreRH
      .endif
    ;  valid command and connected.  Lookup IoCB_OutSize since some
    ;    callers don't tell us.
      lea      bx, IoCB_OutSize
      xlat
      cbw
      mov      dx, ax                ; keep size, we need it to send dta
      les      bx, rhAddr
      mov      es:[bx].rhIOCTL.BytesToTransfer, ax
    ;  some programs also lie about the rhlength
      mov       al, 26
      mov       es:[bx].rh.length, al
    ;  send rh and then control block to remote CD
      INVOKE NetSend, es::bx, es:[bx].rh.length, LocalSession
      .if      al != NB_Ok
          jmp     IoctlError
      .endif
    ;  then the CB
      les      bx, es:[bx].rhIOCTL.CBPtr
      INVOKE NetSend, es::bx, dx, LocalSession
      les      bx, rhAddr
      .if      al != NB_Ok
          jmp     IoctlError
      .endif
    ;  rh is only thing coming back
      INVOKE NetReceive, es::bx, es:[bx].rh.length, LocalSession
      .if      al == NB_Ok
          jmp     Exit0          ; returns device status to MSCDEX
      .endif

IoctlError:
      mov      ax, (DeviceDone OR DeviceError OR DE_ReadError)
      jmp      ExitWithStatus

DevOpen:
   ;  When MSCDEX loads it has DOS do Open, use IOCTL input to get the
   ;  device header address and then closes the device.  It then opens all
   ;  subunits and gets a status from each.
   ;  Subsequent calls are direct to strategy and/or interrupt address obtained
   ;  from the device header.

      .if LocalSession == 0
         INVOKE NetCall, offset ClientName,        ; call server
                         offset ServerName
         .if  al != NB_Ok
            jmp  OpenError
         .endif
         mov   LocalSession, ah
      .endif
    ;  send the rh                        ; get # of CD's attached
      mov   bx, offset rhBuf              ; init uses a local rh
      INVOKE NetSend, ds::bx, ds:[bx].rh.length, LocalSession
      .if   al == NB_Ok
         INVOKE NetReceive, ds::bx, ds:[bx].rh.length, LocalSession
         .if   al == NB_Ok
             mov   al, ds:[bx].rhINIT.NumberUnits       ; save in header for
             mov   SubUnits, al                        ; MSCDEX to use
             mov   ax, DeviceDone
             jmp   ExitRestoreRH
         .endif
      .endif
      les      bx, rhAddr
  OpenError:
      .if ClientNameFlag != NB_DuplicateLocalName
          INVOKE NetDelName, offset ClientName
      .endif
      mov      ax, (DeviceDone OR DeviceError OR DE_GeneralFailure)
      jmp   ExitWithStatus

DevClose:                                     ; just acknowledge
      mov      ax, DeviceDone
      jmp      ExitWithStatus

ReadLong:
    ;  save sector count
       mov    cx, es:[bx].rhTransfer.SectorCount
       mov    SectorCountSave, cx
    ;  send the rh to remote CD
       INVOKE NetSend, es::bx, es:[bx].rh.length, LocalSession
    ;  and get it back
      .if  al == NB_Ok
        INVOKE NetReceive, es::bx, es:[bx].rh.length, LocalSession
      .endif
      .if  al != NB_Ok
        jmp    NetworkErrorExit
      .endif
    ;  now get the dta
       mov    cx, es:[bx].rhTransfer.SectorCount
       .if (cx == 0) || (es:[bx].rh.Status != DeviceDone)
         jmp    Exit0           ; rh.status tells all
       .endif
       mov    ax, cx
       mov    cx, 11
       shl    ax, cl           ;  bytes to xfer := sectors x 2048
       mov    cx,ax
       INVOKE NetReceive, es:[bx].rhTransfer.DtaPtr, cx, LocalSession
       .if  al != NB_Ok
         jmp    NetworkErrorExit
       .endif
       mov    cx, es:[bx].rhTransfer.SectorCount
       .if cx == SectorCountSave
         jmp    Exit0          ; usual requests will return here
       .endif
     ;  request to big, bring back the rest of the pieces
     ;  first initialize work space and save rest of the rh info
       mov    ax, word ptr es:[bx].rhTransfer.StartSector
       mov    word ptr StartSectorSave, ax
       mov    ax, word ptr es:[bx+2].rhTransfer.StartSector
       mov    word ptr StartSectorSave+2, ax
       mov    ax, word ptr es:[bx].rhTransfer.DtaPtr
       mov    word ptr DtaPtrSave, ax
       mov    ax, word ptr es:[bx+2].rhTransfer.DtaPtr
       mov    word ptr DtaPtrSave+2, ax
  ReadNextChunk:
       mov    SectorsRead, cx
       mov    ax, SectorCountSave
       sub    ax, cx                               ; sectors left to read
       mov    cx, es:[bx].rhTransfer.SectorCount   ; sectors in previous read
       mov    es:[bx].rhTransfer.SectorCount, ax   ; count for next read
       mov    ax, word ptr es:[bx].rhTransfer.StartSector
       add    ax, cx                               ; start sector for next read
       mov    word ptr es:[bx].rhTransfer.StartSector, ax
       .if    carry?                ; then increment highorder byte
         mov    ax, word ptr es:[bx+2].rhTransfer.StartSector
         inc    ax
         mov    word ptr es:[bx+2].rhTransfer.StartSector, ax
       .endif

       mov    ax, cx                     ; sectors in previous read
       mov    cl, 7
       shl    ax, cl                     ; ParaRead := 128 * Sectors Read
       add    ax, word ptr es:[bx+2].rhTransfer.DtaPtr
       mov    word ptr es:[bx+2].rhTransfer.DtaPtr, ax

    ;  send the rh to remote CD
       INVOKE NetSend, es::bx, es:[bx].rh.length, LocalSession
    ;  and get it back
      .if  al == NB_Ok
        INVOKE NetReceive, es::bx, es:[bx].rh.length, LocalSession
      .endif
      .if  al != NB_Ok
        jmp    OverSizeReadError
      .endif
       mov    cx, es:[bx].rhTransfer.SectorCount
       .if (cx == 0) || (es:[bx].rh.Status != DeviceDone)
         jmp    OverSizeReadExit           ; rh.status tells all
       .endif
    ;  now get the dta
       mov    ax, cx
       mov    cl, 11
       shl    ax, cl           ;  bytes to xfer := sectors x 2048
       mov    cx, ax
       INVOKE NetReceive, es:[bx].rhTransfer.DtaPtr, cx, LocalSession
       .if  al != NB_Ok
         jmp    OverSizeReadError
       .endif
       mov    cx, es:[bx].rhTransfer.SectorCount
       add    cx, SectorsRead
       .if cx < SectorCountSave
         jmp  ReadNextChunk
       .endif
       mov    SectorsRead, cx
       jmp    OverSizeReadExit

  OverSizeReadError:
   ;   call network error a device error before restoring rh info and exiting
       mov    ax, (DeviceDone OR DeviceError OR DE_ReadError)
       mov    es:[bx].rh.Status, ax

  OverSizeReadExit:
     ;  restore rh info before exiting (this may be unneccessary)
       mov    ax,word ptr DtaPtrSave
       mov    word ptr es:[bx].rhTransfer.DtaPtr, ax
       mov    ax, word ptr DtaPtrSave+2
       mov    word ptr es:[bx+2].rhTransfer.DtaPtr, ax
       mov    ax, word ptr StartSectorSave
       mov    word ptr es:[bx].rhTransfer.StartSector, ax
       mov    ax, word ptr StartSectorSave+2
       mov    word ptr es:[bx+2].rhTransfer.StartSector, ax
       mov    ax, SectorsRead
       mov    word ptr es:[bx].rhTransfer.SectorCount, ax
       jmp    Exit0

  NetworkErrorExit:
   ;  network error occurred, call it a device error
       mov    ax, (DeviceDone OR DeviceError OR DE_ReadError)
       jmp    ExitWithStatus

ReadPrefetch:
    ;  no data transfer, process like a seek
Seek:
    ;  send the rh to remote CD
      INVOKE NetSend, es::bx, es:[bx].rh.length, LocalSession
    ;  and get it back
      .if      al == NB_Ok
          INVOKE NetReceive, es::bx, es:[bx].rh.length, LocalSession
      .endif
      .if      al == NB_Ok
          jmp     Exit0           ; status returned in rh.status
      .else
          mov     ax, (DeviceDone OR DeviceError OR DE_ReadError)
          jmp     ExitWithStatus
      .endif

UnknownCommand:
      mov      ax, (DeviceDone OR DeviceError OR DE_UnknownCommand)
      jmp      ExitRestoreRH


ExitRestoreRH:

      les      bx, rhAddr                ; restore rh ptr

ExitWithStatus:

      mov   es:[bx].rh.Status, ax

Exit0:
      ret

Interrupt   endp

include netbios.inc

     byte "End of CDNET"

LastByte               label byte
;============================================================================
;  everything below this line is discarded after installing the driver

ClientParm              equ     'C'     ; command line parms /C:, etc
DriverParm              equ     'D'
ServerParm              equ     'S'
ArgumentNotFound        EQU     2       ; Unrecognized argument
NoArgumentsFound        EQU     1       ; No argument in command line
ArgumentFound           EQU     0       ; Ok argument in command line

UnInstall               equ     2
DontInstall             equ     1
Install                 equ     0
QuietInstall            equ     2

InstallFlag             byte    0
QuietFlag               word    0

_SS    word ?
_SP    word ?


Init  proc far
                                 ; fix regs for Install/uninstall
      push     ds                ; point to psp so we can access command line
      pop      es                ; using es or for uninstall
      push     cs                ; now set DS to CS
      pop      ds

      call ParseCommandLine

      .if InstallFlag == Install
           call VerifyClientName
          .if InstallFlag == Install
              call Link
              .if (InstallFlag == Install && QuietFlag != QuietInstall)
                  Display  InstallMsg
                  Display  ClientMsg
                  Display  ClientName
                  Display  ServerMsg
                  Display  ServerName
                  Display  DriverMsg
                  Display  DriverName
                  Display  ActivateMsg
              .endif
              push   es                     ; reset ds to psp
              pop    ds

              mov    ax, ds:[2Ch]           ; find environment and release it
              mov    es, ax
              mov    ah, 49h
              int    21h
              sub    ax, ax
              mov    ds:[2Ch], ax           ; zero the evironment ptr

              if  (LastByte-DevHeader+1) mod 16
                  roundup = 1
              else
                  roundup = 0
              endif

              mov     dx,((LastByte-DevHeader)+1+100h)/16+roundup; para to keep
              mov     ah,31h        ; stay resident and
              int     21h             ; exit

          .endif
      .elseif InstallFlag == UnInstall
          call UnInstallDriver              ; es points to our psp

      .endif

      .exit

Init endp

Link   proc near uses es
      mov      ax, 5200h               ; get list of list
      int      21h                     ; we assume DOS 3.1 or later
      add      bx, 22h                 ; es:bx[22] is NUL device header
      mov      ax, es:[bx]               ; put NUL.next in our header
      mov      word ptr NextDriver, ax
      mov      ax, es:[bx+2]
      mov      word ptr NextDriver+2, ax
      mov      ax, 0                   ; then point NUL header at us
      mov      es:[bx], ax
      mov      es:[bx+2], cs
      ret
Link   endp

UnInstallDriver proc near

   local _bx,_es:word

      push     es                      ; save our psp address
      mov      ax, 5200h               ; get list of list
      int      21h                     ; we assume DOS 3.1 or later
      add      bx, 22h                 ; es:bx[22] is NUL (1st) device header
                                       ; es:bx now pointing at NUL header
   TryNext:
      mov      _bx, bx                 ; save current header addr
      mov      _es, es
      les      bx, es:[bx]             ; load next header addr into es:bx
      mov      ax, es
      cmp      ax, 0FFFFh              ; end of drivers?
      je       DriverNotInstalled
      mov      cx, 8
      lea      di, DeviceName[bx]      ; es:di is chained device name
      mov      si, offset DeviceName   ; dx:si is our device name
      repe     cmpsb                   ; if equ its the one we are looking for
      jne      TryNext
      lea      di, LocalSession[bx]    ; get the session number of driver
      mov      dl, byte ptr es:[di]    ; being uninstalled
      push     ds
      mov      ax, es                  ; es:bx is addr of driver being removed
      mov      ds, ax                  ; put it into ds:si
      mov      si, bx                  ;
      mov      cx, 4                   ;
      mov      es, _es                 ;
      mov      di, _bx                 ; previous header now in es:di
      rep      movsb                   ; move address ds:si -> es:di
      mov      es, ax                  ; es now points at unlinked driver
      pop      ds                      ; cs=ds=@code -- need this for net
      .if   dl != 0
          INVOKE NetHangup, dl
      .endif
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

      display DriverName                ; tell the world we did it
      display UnInstallMsg
      ret

  DriverNotInstalled:
      display  DriverName
      Display  DriverNotFoundMsg
      ret

UnInstallDriver endp

VerifyClientName proc near

      INVOKE NetAddName, offset ClientName
      .if      (al != NB_Ok && al != NB_DuplicateLocalName)
          Display  ClientMsg
          Display  ClientName
          .if al == NB_NameAlreadyClaimed
              Display   NotInstallMsg
              Display   NameClaimedMsg
          .else
              Display   NotInstallMsg
              Display   NetBIOSNotReadyMsg
          .endif
         mov   al,DontInstall
         mov   InstallFlag, al
      .elseif al == NB_DuplicateLocalName
         mov   ClientNameFlag, al
      .endif
      ret
VerifyClientName endp

ParseCommandLine       proc near  uses es

      ;* If driver is loaded from config.sys using device=drivername parms
      ;* then rhINIT points to the first character following the drivername
      ;* and a CR follows the last parm.  When loaded by executing, the
      ;* command line is available in the PSP.(len +80h, 1st ch +81h, no CR)

      sub      ch, ch
      mov      di, 80h            ; command line length @ +80h
      mov      cl, es:[di]

      mov      al, '?'
      call     GetParm
      .if       ax == NoArgumentsFound
         jmp   CopyDeviceName
      .elseif   ax == ArgumentFound       ; /?  user needs help on usage
         Display  UsageMsg
         mov   al,DontInstall
         mov   InstallFlag, al
         jmp   exit
      .endif

      sub      ch, ch
      mov      di, 80h            ; command line length @ +80h
      mov      cl, es:[di]
      mov      al, 'Q'            ; /Q  quiet installation
      call     GetParm
      .if ax == ArgumentFound
           mov      QuietFlag, QuietInstall
      .endif

      sub      ch, ch
      mov      di, 80h            ; command line length @ +80h
      mov      cl, es:[di]
      mov      al, 'U'            ; /U unInstall driver
      call     GetParm
      .if ax == ArgumentFound
          mov      InstallFlag, UnInstall
      .endif

      mov      al, ClientParm
      call     FindParm
      .if   ax == ArgumentFound
         mov      dx, sizeof ClientName
         lea      si, ClientName
         call     MoveName
      .endif

      mov      al, ServerParm
      call     FindParm
     .if   ax == ArgumentFound
         mov      dx, sizeof ServerName
         lea      si, ServerName
         call     MoveName
      .endif

      mov      al, DriverParm
      call     FindParm
     .if   ax == ArgumentFound
         mov      dx, sizeof DeviceName
         lea      si, DeviceName
         call     MoveName
      .endif

  CopyDeviceName:                               ; so we can display it

      mov   cx, 8
      push  ds
      pop   es
      lea   si, DeviceName
      lea   di, DriverName
      cld
      rep    movsb


Exit: ret

ParseCommandLine       endp

UsageMsg    db cr, lf,"Usage:  CDNET"
            db " [/?] [/C:ClientName] [/S:ServerName] [/D:DriverName]"
            db " [/Q]"," [/U]",cr,lf,"$"

InstallMsg  db cr,lf,"Net Work CD Driver Installed.",cr,lf
            db "Copyright 1993, John H. McCoy.","$"
ClientMsg   db cr,lf,"Client Name:  ","$"
ServerMsg   db cr,lf,"Server Name:  ","$"
DriverMsg   db cr,lf,"Driver Name:  ","$"
ActivateMsg db cr,lf,"Link to server will occur when MSCDEX is loaded.",cr,lf,"$"
UnInstallMsg       db cr,lf,"Uninstalled and memory freed",cr,lf,"$"
NotInstallMsg      db cr,lf,"Net Work Driver NOT installed.","$"
NameClaimedMsg     db cr,lf,"Client name already in use on network.",cr,lf,"$"
NetBIOSNotReadyMsg db cr,lf,"Net BIOS not ready.",cr,lf,"$"
DriverNotFoundMsg  db " CD Network Driver not installed.",cr,lf,"$"

MoveName proc near
      sub   bx, bx                                 ; es:di points to 1st char
      .repeat                                      ; cx chars left on cmd line
          mov al, es:[di]
          .if (al == '/' || al == ' '|| cx <= 0)
              mov    byte ptr [si+bx], ' '
              inc    bx
          .else
             .if (al >= 'a' && al <= 'z')
                 and    al, 11011111y           ; upper case it
              .endif
              mov    byte ptr [si+bx], al
              inc    bx
              inc    di
              dec    cx
          .endif
      .until bx == dx

MoveName endp

FindParm proc near

   ; al has parm code we are to find       /X: or -X:

      mov      di, 80h                ; command line length @ +80h
      sub      ch, ch
      mov      cl, es:[di]

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
  NotFound: ret

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
        mov     ah, ArgumentFound         ; Assume argument is okay
        inc     di
        dec     cl
        mov     dl, es:[di]
        .if (dl >= 'a' && dl <= 'z')
            and    dl, 11011111y           ; Make character upper case
        .endif
        cmp     dl, al
        je      exit                    ; specified char
        mov     ah, ArgumentNotFound    ; Else signal bad argument,
        inc     bx
        loop    loop1             ;   continue scan

exit:
        mov     al, ah
        cbw                             ; AX = return code
        ret

GetParm ENDP

            end  Init

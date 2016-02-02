;************************************************************************
;
;  SHSUCDHD.ASM  May 1996 Version 2.0
;
;    makes cached CD image appear as unit 0 on shsu-cdh
;
;  Use:  CDHD [/F:imagepathname1 [/F:imagepathname2 ...]] [/U] [/?]
;
;  Assemble using MASM 6.0 and link as an .exe file.
;
;  SHSUCDHD is a copyright reserved, free use program.  Use at your own risk.
;
;  (c)John H. McCoy, 1996, Sam Houston St. Univ., TX 77341-2206
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
  Units        byte      1             ; number of CD drives on this device
devHdr ends

rh    struc
   Length      byte ?          ; header size in bytes
   Unit        byte ?          ; cd drive unit
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

rhTransfer  struc
                     byte size rh dup(?) ;rh common
                     byte   ?
   DtaPtr            fptr   ?
   SectorCount       word   ?
   StartSector       dword  ?
   ReadMode          byte   ?     ; we support cooked mode only
                     byte   ?
                     byte   ?
rhTransfer ends

rhcmdIOCTL_In          equ 3
rhcmdOpen              equ 0Dh
rhcmdClose             equ 0Eh
rhcmdPreFetch          equ 82h
rhcmdReadLong          equ 80h

IOCtl_In_RDHACmd          equ  0
IOCtl_ReadDriveBytes      equ  5
IOCtl_DevStatCmd          equ  6
IOCtl_ReturnSectorSize    equ  7
IOCtl_ReturnVolumeSize    equ  8    ; total sectors on disk
IOCtl_MediaChangedCmd     equ  9

IoCB_RDHA     struc
   IoctlCommand           byte IOCtl_In_RDHACmd
   DeviceHeaderAddress    fptr ?
IoCB_RDHA ends

DeviceError            equ  8000h
DeviceDone             equ  0100h
DE_UnknownUnit         equ    01h   ; OR these with DeviceError
DE_DeviceNotReady      equ    02h
DE_UnknownCommand      equ    03h
DE_SectorNoFound       equ    08h
DE_ReadError           equ    0Bh
DE_GeneralFailure      equ    0Ch

MediaChanged_No           equ   00
MediaChanged_Yes          equ   0ffh
MediaChanged_DontKnow     equ   01
DriveNormalReady          equ   0
DriveDoorOpen             equ   01h      ; bit is zero if closed
DriveDoorUnlocked         equ   02h      ; bit is zero if locked
DriveDataAndAudio         equ   10h      ; bit is zero if data only
DrivePrefetching          equ   80h      ; bit is zero if no prefetch
DriveRedBookAddressing    equ   200h     ; bit is zero if HSG only
DriveEmpty                equ   800h     ; bit is zero if loaded
                                         ; best guess observation
DriveEntry      struct
   MediaChange     byte   MediaChanged_No
   Status          dword  DriveEmpty
   ImageFile       byte   33 dup (0)
DriveEntry      ends

AsciiNul                equ     0
cr                      equ     13
lf                      equ     10
ht                      equ     09
QMark                   equ     '?'
T                       equ     0
F                       equ     -1

ReadImage         proto near C
StringEq?         proto near C S1:fptr, S2:fptr
ToHex             proto near C Num:word
MsgOut            proto near C msg:nptr
.code

   assume cs:@code,  ds:@code

;  dos device header with CDROM extension fields
;  DO NOT MAKE THE DEVICE DRIVER NAME THE SAME AS THE FILE NAME

MaxDrives      equ     5

CDHDHdr  devHdr  {-1,0C800h,Strategy,Interrupt,'SHSU-CDH',0,0,0}

Author          byte     "Copyright 1996, John H. McCoy"

Drive          DriveEntry  MaxDrives dup(<>)

DriveUnit      byte    0
DriveOffset    word    0             ; re-calculated on entry

InDOS           byte ?
InDOSp          fptr ?

rhAddr        label far ptr
  rhOffset      word       ?
  rhSegment     word       ?

ImageHandle    word     ?
PSP            word     ?
_SS            word     ?
_SP            word     ?
DOS_SS         word     ?
DOS_SP         word     ?
_temp1         word     ?
_DS            word     ?

SectorSize     equ     800h      ;make it an equ so we don't change it
StartSector    Dword    ?
StartOffset    word     ?
StartPosition  Dword    ?
BytesToRead    word     ?

DTAp              fptr    ?
kount             word    0
MySDAp            fptr    ?
MySDASize         word    ?
MySDASave         byte    600h dup ("v")
MyReadBuffer      byte    96 dup ("r")
HS                byte    "CDROM"      ; PVD 10-14
ISO               byte    "CD001"      ; PVD 2-6

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
   push  bp
   mov   bp,sp
   push  cx
   push  dx
   push  di
   push  si
   push  ds

   ;  process command
      les      bx, cs:rhAddr                ; make sure we have rh addr
      mov      al, es:[bx].rh.Unit
      .if   (al >= MaxDrives)
            mov   ax,(DeviceDone OR DeviceError OR DE_UnknownUnit)
            jmp   xit
      .endif

      push cs
      pop  ds
      mov  _DS, ds

      mov    DriveUnit, al
      sub    ah, ah
      mov    cx, sizeof DriveEntry
      mul    cx
      mov    DriveOffset, ax
      mov    al, es:[bx].rh.command
      .if  (al == rhcmdIOCTL_In)
        les      bx, es:[bx].rhIOCTL.CBPtr
        mov      al, byte ptr es:[bx]         ; 1st byte of dta is subcommand
        .if      al == IOCtl_In_RDHACmd
           lea   ax, CDHDHdr
           mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress, ax
           mov   ax, ds
           mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress+2, ax
           mov   ax, DeviceDone
        .elseif   al == IOCtl_ReadDriveBytes
           mov   ax, 0
           mov   word ptr es:[bx+1], ax
           mov   ax, DeviceDone
        .elseif   al == IOCtl_DevStatCmd
           mov   si, DriveOffset
           mov   ax, word ptr Drive[si].Status
           mov   word ptr es:[bx+1], ax
           mov   ax, word ptr Drive[si+2].Status
           mov   word ptr es:[bx+3], ax
           mov   ax, DeviceDone
        .elseif   al == IOCtl_ReturnSectorSize
           mov   ax, 0
           mov   word ptr es:[bx+1], ax
           mov   ax, 2048
           mov   word ptr es:[bx+2], ax
           mov   ax, DeviceDone
        .elseif   al == IOCtl_ReturnVolumeSize
           ; might could avoid read and worry about cd type by
           ;   seeking to end of the file and calculating next block
           mov   ax, 10h
           mov   word ptr StartSector, ax
           mov   word ptr StartSector+2, 0
           mov   ax, offset MyReadBuffer
           mov   word ptr DTAp, ax
           mov   ax, ds
           mov   word ptr DTAp+2, ax
           mov   StartOffset, 0
           mov   BytesToRead, sizeof MyReadBuffer
           invoke ReadImage
           ;    if buffer 2-6 = "CD001" size offset 80-83
           mov  cx, sizeof ISO
           invoke StringEq?,addr ISO,addr MyReadBuffer[1] ;eq if cx=0 on ret
           cmp  cx,0
           jne  NotISO
           mov   ax, word ptr MyReadBuffer[80]
           mov   word ptr es:[bx+1], ax
           mov   ax, word ptr MyReadBuffer[82]
           mov   word ptr es:[bx+3], ax
           jmp   VolSizeXit
         NotISO:
           ;    if buffer 10-14= "CDROM" size offset 88-91
           mov  cx, sizeof HS
           invoke StringEq?,addr HS,addr MyReadBuffer[9] ;eq if cx=0 on ret
           cmp  cx,0
           jne  NotHS
           mov   ax, word ptr MyReadBuffer[88]
           mov   word ptr es:[bx+1], ax
           mov   ax, word ptr MyReadBuffer[91]
           mov   word ptr es:[bx+3], ax
           jmp   VolSizeXit
          NotHS:
           mov   word ptr es:[bx+1], 0
           mov   word ptr es:[bx+3], 0
          VolSizeXit:
           mov   ax, DeviceDone
        .elseif   al == IOCtl_MediaChangedCmd
           mov   byte ptr es:[bx+1], 1    ; 0 - don't know
           mov   ax, DeviceDone           ; 1 - no change; 0ffh changed
        .else   ; all other ioctlin sub commands
           mov   ax,(DeviceDone OR DeviceError OR DE_UnknownCommand)
        .endif
      .elseif   ((al == rhcmdOpen)||(al ==rhcmdClose))
          mov   ax, DeviceDone
      .elseif   (al == rhcmdReadLong)
          les   bx, cs:rhaddr
          mov   ax,word ptr es:[bx].rhTransfer.StartSector
          mov   word ptr StartSector, ax
          mov   ax,word ptr es:[bx].rhTransfer.StartSector+2
          mov   word ptr StartSector+2, ax
          mov   ax, word ptr es:[bx].rhTransfer.DtaPtr
          mov   word ptr DTAp, ax
          mov   ax, word ptr es:[bx].rhTransfer.DtaPtr+2
          mov   word ptr DTAp+2, ax
          mov   StartOffset, 0
       @@:
          mov   ax, SectorSize
          mul   es:[bx].rhTransfer.SectorCount
          cmp   dx,0
          je    @f
          sub   es:[bx].rhTransfer.SectorCount,1
          jmp   @b
        @@:
          mov   BytesToRead, ax
          Invoke ReadImage
          .if  (ax!=0)
             mov   es:[bx].rhTransfer.SectorCount,0
          .endif
          or   ax,(DeviceDone)
      .elseif   (al == rhcmdPreFetch)
          mov   ax,(DeviceDone OR DeviceError OR DE_UnknownCommand)
      .else
          mov   ax,(DeviceDone OR DeviceError OR DE_UnknownCommand)
      .endif

Xit:
       pop   ds
       pop   si
       pop   di
       pop   dx
       pop   cx
       pop   bp
       les   bx, cs:rhAddr                   ; restore rh ptr
       mov   es:[bx].rh.Status, ax
       ret

Interrupt   endp

ReadImage  proc near C uses bx cx dx es ds

   .if (BytesToRead == 0)
      xor  ax, ax
      ret
   .endif

  ; get InDOS flag
   les   bx, InDOSp
   mov   ah, byte ptr es:[bx]
   mov   byte ptr InDOS, ah

.if inDOS

  ; save the SDA        ; how much depends - we don't have the dos sp
                        ; so save everything
   mov   es, _DS
   mov   cx, MySDASize
   mov   di, offset MySDASave
   lds   si, MySDAp
   cld
   rep   movsb               ; ds:si=>es:di

   mov   ds, cs:_DS

   ;change to DOS's SS, SP

   mov   _SS, ss
   mov   _SP, sp
   mov   ss, DOS_SS
   mov   sp, DOS_SP
   ;open the image file
   mov   cl,40h
   mov   dx, offset Drive.ImageFile
   add   dx, DriveOffset
   mov   ax, 1226h
   int   2fh               ; handle returned in ax if carry not set
   mov   ds, cs:_DS
   jnc   @f
   ; restore incoming stack and SDA since we are terminating
   mov   ss, _SS
   mov   sp, _SP
   les   di, MySDAp
   mov   cx, MySDASize
   mov   si, offset MySDASave
   cld
   rep movsb                   ; ds:si=>es:di
   mov   ax, (DeviceError OR DE_DeviceNotReady)
   ret
   @@:
.else    ; not in DOS
   ;open the  image file
   mov   dx, offset Drive.ImageFile
   add   dx, DriveOffset
   mov   ax, 3d00h
   int   21h              ; handle returned in ax if carry not set
   jnc   @f
   mov   ax, (DeviceError OR DE_DeviceNotReady)
   ret
  @@:
.endif

   mov   ImageHandle, ax
   ; calc file pointer position
   mov   ax, SectorSize
   mov   dx, word ptr StartSector
;   dec   dx
   mul   dx
   mov   _temp1,ax              ; save low order word temporarily
   mov   cx, dx                 ; hi order word
   mov   dx, word ptr StartSector+2
   jz    @F
   mov   ax, SectorSize
   mul   dx
   add   cx, ax                 ; high order word in cx for set
 @@:
   mov   dx, _temp1             ;low order word goes in dx for set
   add   dx, StartOffset


.if inDOS
   ;set file pointer position
   mov   _temp1, bp               ; save over dos call
   mov   bp, 4200h
   mov   bx, ImageHandle
   mov   ax, 1228h
   int   2fh
   mov   ds, cs:_DS
   mov   bp, _temp1            ; restore bp
   jnc   @f
   mov   cx, (DeviceError OR DE_SectorNoFound)
   jmp   InDOSXit
 @@:
   ; read cd sector
   mov   bx, ImageHandle
   mov   cx, BytesToRead
   lds   dx, DTAp
   mov   ax, 1229h
   int   2fh
   mov   ds, cs:_DS
   jnc   @F
    mov   cx, (DeviceError OR DE_ReadError)
   jmp   InDOSXit
 @@:
   xor cx,cx        ; no error
   ;  close file
 InDOSXit:
   mov   bx, ImageHandle
   mov   ax, 1227h
   int   2fh
   mov   ds, cs:_DS
   ; restore incoming stack and SDA
   mov   ss, _SS
   mov   sp, _SP
   les   di, MySDAp
   mov   cx, MySDASize
   mov   si, offset MySDASave
   cld
   rep   movsb                   ; ds:si=>es:di
   mov   ax, cx
   ret

.else                   ; not in DOS

   ;set file pointer position
   mov   bx, ImageHandle
   mov   ax, 4200h
   int   21h
   jnc   @f
   mov   cx, (DeviceError OR DE_SectorNoFound)
   jmp   NotInDOSXit
 @@:
   ;read cd sector
   mov   bx, ImageHandle
   mov   cx, BytesToRead
   lds   dx, DTAp
   mov   ax, 3f00h
   int   21h
   mov   ds,cs:_DS
   jnc   @f
   mov   ah, 0ffh
   mov   cx, (DeviceError OR DE_ReadError)
   jmp   NotInDOSXit
 @@:
   xor   cx, cx        ; no error
   ;  close the file
 NotInDOSXit:
   mov   bx, ImageHandle
   mov   ax, 3e00h
   int   21h
   mov   ax, cx
   ret
.endif


ReadImage  endp

ToHex proc near C public uses ax bx cx dx, Num:word
      mov   cl, 4
      mov   ch, 4
      mov   ah, 02h
      mov   dx, Num
      .while ch > 0
          rol   dx, cl
          mov   bx, dx
          and   dx, 0fh
          .if dl < 0Ah
              add  dl, '0'
          .else
              add  dl, 'A' - 0Ah
          .endif
          int   21h
          mov   dx, bx
          dec   ch
      .endw
      ret
ToHex    endp

StringEq? proc near C uses  ds es di si, S1:fptr,S2:fptr
    ; cx is string length on entry
    mov   ax, word ptr S1
    mov   si, ax
    mov   ax, word ptr S1+2
    mov   ds, ax
    mov   ax, word ptr S2
    mov   di, ax
    mov   ax, word ptr S2+2
    mov   es, ax
    repe  cmpsb
    ret
    ; returns with cx = 0 if strings match
StringEq? endp

MovString proc near C uses   ds es di si, S1:fptr,S2:fptr
    ; cl has number of chars to move
    mov   ax, word ptr S1
    mov   si, ax
    mov   ax, word ptr S1+2
    mov   ds, ax
    mov   ax, word ptr S2
    mov   di, ax
    mov   ax, word ptr S2+2
    mov   es, ax
    cld
    rep movsb                   ; ds:si=>es:di
    ret
MovString endp

MsgOut    proc  near C public uses ax bx dx, msg:nptr
      mov      ah, 02h       ; display ch function
      mov      bx, msg
      mov      dl, ds:[bx]
      .while (dl != '$' && dl != 0)
         int      21h
         inc      bx
         mov      dl, ds:[bx]
      .endw
      ret
MsgOut    endp


     byte "End of CDHD"

LastByte               label byte

   assume ds:nothing
;============================================================================
;  everything below this line is discarded after installing the driver

ArgumentNotFound        EQU     2       ; Unrecognized argument
NoArgumentsFound        EQU     1       ; No argument in command line
ArgumentFound           EQU     0       ; Ok argument in command line

IOCtlInBuf       byte     5 dup(?)

FName            byte     sizeof DriveEntry.ImageFile dup (0)
                 byte     128 dup(0)
DOffset          word     0

DontInstall             equ     1
Install                 equ     0
InstallFlag             byte    DontInstall

InstallMsg  db cr,lf,"Copyright 1996, John H. McCoy."
            db cr,lf,"SHSU-CDH CD HardDisk cache driver version 2.0 Installed."
            db cr,lf, "$"
UnInstallMsg       db " SHSUCDHD Uninstalled and memory freed",cr,lf,"$"
CouldNotRemoveMsg  db "Can't un-install SHSUCDHD.",cr,lf,"$"
NotInstalledMsg db "SHSUCDHD not installed.",cr,lf,"$"
FileNotFoundMsg db "Can't open any cache image file.",cr,lf,"$"
InvalidImageFileMsg db "Image File Name invalid or doesn't exist",cr,lf,"$"
HelpMsg db  cr,lf,"Usage:  SHSUCDHD /F:imagefilename  [/U] [/?]"
        db  cr,lf,"Installs as SHSU-CDH.  Use with SHSUCDX."
        db  cr,lf,"Attempting to use MSCDEX will crash your system.","$"
NewLine db  cr,lf,"$"
Init  proc far

local _cx,_di,_es : word

      mov      cs:PSP, ds            ; save PSP
      mov      ax, cs                ; now set DS to CS
      mov      ds, ax

   ;* If driver is loaded from config.sys using device=drivername parms
   ;* then rhINIT points to the first character following the drivername
   ;* and a CR follows the last parm.  When loaded by executing, the
   ;* command line is available in the PSP.(len +80h, 1st ch +81h, no CR)

   ; check for uninstall
      sub      ch, ch
      mov      di, 80h            ; command line length psp +80h
      mov      cl, es:[di]
      mov      al, 'U'            ; /U unInstall driver
      call     GetParm
      .if ax == ArgumentFound
          call UnInstallCDHD
          jmp   Xit
      .elseif ax == NoArgumentsFound
          jmp   Dont
      .endif
    ; check for use help
      sub      ch, ch
      mov      di, 80h            ; command line length psp +80h
      mov      cl, es:[di]
      mov      al, '?'            ; /? help
      call     GetParm
      .if ax == ArgumentFound
          invoke MsgOut, addr HelpMsg
          jmp   Xit
      .endif

      mov      di, 80h                ; command line length at psp +80h
      sub      ch, ch
      mov      cl, es:[di]
      mov      al, 'F'            ; /F:filename
      call     FindParm
     .if   ax == ArgumentFound
         call    MoveName
         mov    _cx, cx
         mov    _di, di
         mov    _es, es

          ; canonicalize filename
          mov   ax, cs
          mov   es, ax
          mov   di, offset FName
          mov   si, offset FName
          mov   ah, 60h
          int   21h

         ;open the file  see if it exists
          mov   dx, offset FName
          mov   ax, 3d00h
          int   21h
          jc    @f          ; file didn't open
          ; close the file
          mov   bx, ax
          mov   ax,3e00h
          int   21h
          invoke msgout,addr FName
          invoke msgout,addr NewLine
          mov cl,sizeof Drive.ImageFile
          invoke MovString,addr FName, addr Drive.ImageFile
          mov   ax, DriveNormalReady
          mov   word ptr Drive.Status, ax
          add   DOffset, sizeof DriveEntry
          inc   byte ptr CDHDHdr.Units
        @@:
          mov   cx, _cx
          mov   di, _di
          mov   es, _es

          mov   al, 'F'            ; /F:filename
          call  FindParm
          .if  ax==ArgumentFound
             call    MoveName
             mov    _cx, cx
             mov    _di, di
             mov    _es, es

            ; canonicalize filename
             mov   ax, cs
             mov   es, ax
             mov   di, offset FName
             mov   si, offset FName
             mov   ah, 60h
             sub   al, al
             int   21h
            ;open the file  see if it exists
             mov   dx, offset FName
             mov   ax, 3d00h
             int   21h
             jc    @f      ; file not found
             ; close the file
             mov   bx, ax
             mov   ax,3e00h
             int   21h
             invoke msgout,addr FName
             invoke msgout,addr NewLine
             mov cl,sizeof Drive.ImageFile
             mov   bx, DOffset
             invoke MovString,addr FName, addr Drive[bx].ImageFile
             mov   ax, DriveNormalReady
             mov   word ptr Drive[bx].Status, ax
             add   DOffset, sizeof DriveEntry
             inc   byte ptr CDHDHDR.Units
           @@:
          mov   cx, _cx
          mov   di, _di
          mov   es, _es
          mov   al, 'F'            ; /F:filename
          call  FindParm
          .if  ax==ArgumentFound
             call    MoveName
             mov    _cx, cx
             mov    _di, di
             mov    _es, es

            ; canonicalize filename
             mov   ax, cs
             mov   es, ax
             mov   di, offset FName
             mov   si, offset FName
             mov   ah, 60h
             sub   al, al
             int   21h
            ;open the file  see if it exists
             mov   dx, offset FName
             mov   ax, 3d00h
             int   21h
             jc    @f      ; file not found
             ; close the file
             mov   bx, ax
             mov   ax,3e00h
             int   21h
             invoke msgout,addr FName
             invoke msgout,addr NewLine
             mov cl,sizeof Drive.ImageFile
             mov   bx, DOffset
             invoke MovString,addr FName, addr Drive[bx].ImageFile
             mov   ax, DriveNormalReady
             mov   word ptr Drive[bx].Status, ax
             add   DOffset, sizeof DriveEntry
             inc   byte ptr CDHDHDR.Units
           @@:
          mov   cx, _cx
          mov   di, _di
          mov   es, _es
          mov   al, 'F'            ; /F:filename
          call  FindParm
          .if  ax==ArgumentFound
             call    MoveName
             mov    _cx, cx
             mov    _di, di
             mov    _es, es

            ; canonicalize filename
             mov   ax, cs
             mov   es, ax
             mov   di, offset FName
             mov   si, offset FName
             mov   ah, 60h
             sub   al, al
             int   21h
            ;open the file  see if it exists
             mov   dx, offset FName
             mov   ax, 3d00h
             int   21h
             jc    @f      ; file not found
             ; close the file
             mov   bx, ax
             mov   ax,3e00h
             int   21h
             invoke msgout,addr FName
             invoke msgout,addr NewLine
             mov cl,sizeof Drive.ImageFile
             mov   bx, DOffset
             invoke MovString,addr FName, addr Drive[bx].ImageFile
             mov   ax, DriveNormalReady
             mov   word ptr Drive[bx].Status, ax
             add   DOffset, sizeof DriveEntry
             inc   byte ptr CDHDHDR.Units
           @@:
          mov   cx, _cx
          mov   di, _di
          mov   es, _es
          mov   al, 'F'            ; /F:filename
          call  FindParm
          .if  ax==ArgumentFound
             call    MoveName
             mov    _cx, cx
             mov    _di, di
             mov    _es, es

            ; canonicalize filename
             mov   ax, cs
             mov   es, ax
             mov   di, offset FName
             mov   si, offset FName
             mov   ah, 60h
             sub   al, al
             int   21h
            ;open the file  see if it exists
             mov   dx, offset FName
             mov   ax, 3d00h
             int   21h
             jc    @f      ; file not found
             ; close the file
             mov   bx, ax
             mov   ax,3e00h
             int   21h
             invoke msgout,addr FName
             invoke msgout,addr NewLine
             mov cl,sizeof Drive.ImageFile
             mov   bx, DOffset
             invoke MovString,addr FName, addr Drive[bx].ImageFile
             mov   ax, DriveNormalReady
             mov   word ptr Drive[bx].Status, ax
             add   DOffset, sizeof DriveEntry
             inc   byte ptr CDHDHDR.Units
           @@:
          .endif
          .endif
          .endif
          .endif

         .if CDHDHDR.Units==0
            jmp FileNotFound
         .endif
         ;get the SDA ptr
          push  ds
          mov   ax, 5d06h
          int   21h
          mov   ax, ds
          .if  (byte ptr ds:[+4]==0)   ; don't have to save it all
              mov  cx, 5B8h            ; DOS 3.1-3.3
          .else
              mov  cx, 600h            ; DOS 4+
          .endif
          pop   ds
          mov   MySDASize,cx
          mov   word ptr MySDAp+2, ax ; segment is SDA seg
          mov   DOS_SS, ax            ; segment is DOS's seg
          mov   word ptr InDOSp+2, ax ; InDOS flag seg
          mov   word ptr InDOSp, si
          inc   word ptr InDOSp       ; InDos flag SDA +1
          mov word ptr MySDAp, si
          add   si, cx
          mov   DOS_SP, si

          mov    InstallFlag, DontInstall
             call Link
          .if (InstallFlag == DontInstall)
             jmp Dont
          .endif

          invoke MsgOut, addr InstallMsg

          mov    ax, PSP
          mov    ds, ax
          mov    ax, ds:[2Ch]           ; find environment and release it
          mov    es, ax
          mov    ah, 49h
          int    21h
          sub    ax, ax
          mov    ds:[2Ch], ax           ; zero the evironment ptr

          if  (LastByte-CDHDHdr+1) mod 16
              roundup = 1
          else
              roundup = 0
          endif

          mov     dx,((LastByte-CDHDHdr)+1+100h)/16+roundup; para to keep
          mov     ah,31h        ; stay resident and
          int     21h             ; exit
      .else
          jmp   FileNotFound
      .endif

   FileNotFound:
      invoke MsgOut, addr Drive.ImageFile
      invoke MsgOut, addr  FileNotFoundMsg
   Dont:
      invoke MsgOut, addr  NotInstalledMsg
   XIT:
      .exit

Init endp

Link   proc near uses es
      mov      ax, 5200h               ; get list of list
      int      21h                     ; we assume DOS 3.1 or later
      add      bx, 22h                 ; es:bx[22] is NUL device header
      mov      ax, es:[bx]               ; put NUL.next in our header
      mov      word ptr CDHDHdr.NextDriver, ax
      mov      ax, es:[bx+2]
      mov      word ptr CDHDHdr.NextDriver+2, ax
      mov      ax, 0                   ; then point NUL header at us
      mov      es:[bx], ax
      mov      es:[bx+2], cs
      mov      InstallFlag, Install
      ret
Link   endp

UnInstallCDHD proc near

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
      je       DriverNotFound
      mov      cx, 8
      lea      di, devHdr.DeviceName[bx]       ; es:di is chained device name
      mov      si, offset CDHDHdr.DeviceName   ; ds:si is our device name
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

      invoke MsgOut, addr UnInstallMsg
      ret

  DriverNotFound:
      pop es
      invoke MsgOut, addr  CouldNotRemoveMsg
      invoke MsgOut, addr  NotInstalledMsg
      ret

UnInstallCDHD endp

MoveName proc near
      mov     dx, sizeof FName
      xor  si,si                                   ; es:di points to 1st char
      .repeat                                      ; cx chars left on cmd line
          mov al, es:[di]
          .if ((al==' ') || (al==0) ||(al=='/')||(cx==0))
              .if cx!= 0
                 dec cx
              .endif
              mov   al, 0
              .repeat
                 mov    byte ptr FName[si], al
                 inc    si
              .until (si==dx)
          .else
             .if (al >= 'a' && al <= 'z')
                 and    al, 11011111y           ; upper case it
              .endif
              mov    byte ptr FName[si], al
              inc    di
              dec    cx
              inc    si
          .endif
      .until (si==dx)
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
        byte 80 dup(?)
        byte "This is here to keep loadhigh happy if NETX is loaded."


        end  Init

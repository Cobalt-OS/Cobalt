;***************************************************************************
;
; SHSUCDX Version 1.4b
; (c) John H. McCoy, October 2000
; csc_hm@shsu.edu
;
;
;  Version 1.4b fixed a problem with findfirst that caused a problem when
;      a program expected getting the volid would also set it up for
;      scanning the root directory with findnext.
;
;  Version 1.4a changed the dos vesion check so that it will rund under MEDos with
;     set ver.  MEDos is version 8
;  Version 1.4 increased the allowed length of directories from 64K bytes to
;     64K sectors (128 M bytes) to solve the problem of lost entries that
;     occurred when a directory exceeded 32 sectors.
;
;  SHSUCDX Version 1.1a
;  (c) John H. McCoy, May 1996
;  Sam Houston St. Univ., TX 77341-2206
;

;     SHSUCDX is an un-loadable CD-ROM redirector substitute for MSCDEX.
;        Version 1.1a supports up to 10 CD drives.  Each drive is single
;        sector buffered and the last 10 directory entries are cached.
;        This version finally fixes(I hope) a problem with handling some
;        directory entries.  It has also been changes to handle lower case
;        characters in directory and file names.  LC characters are not
;        valid, but some NT/Win95 mastering programs put them in.
;
;        Approx 17K of RAM is needed to install SHSUCDX.  The resident size
;        for a single drive is less than 11K.  Each additional drive increases
;        the resident size by 2500 bytes.  Multiple drivers are supported.
;        The driver name, drive letter, drive unit and number of drives from
;        each driver can be specified on the command line.
;
;        SHSUCDX does not attempt to read the CD ROM until an access request
;        is made.  Thus, the CD drive does not have to be ready when the
;        redirector is loaded.  If more than 7 seconds elapse between access
;        requests a media check is made.  The buffers and cache are flushed
;        and the CD is re-read only if the driver reports a media change.
;        This reduces network traffic but can result in missing a "fast"
;        media change on a local drive.
;
;        When SHSUCDX unloads it marks the drives it used as invalid.
;
;     SHSUCDX has been run with MS-DOS 4, 5, 6 and 7 stand-alone, under
;        Windows 3.1, and in a specific DOS window under OS2.
;
;     A CD-ROM driver which supports the CD-ROM extensions must be loaded
;        before loading SHSUCDX.  By default, SHSUCDX looks for a driver
;        named SHSU-CDN.
;
;     usage:  SHSUCDX [/D:DriverName[,[Drive][,[Unit][,[MaxDrives]]]]]
;
;        DriverName  1 to 8 characters.
;        Drive       First drive letter to assign to drives attached to
;                       this driver.
;        Unit        First drive unit on this driver to be assigned to a
;                       drive letter.  (Allowed range 0 to 99)
;        MaxDrives   Maximum number of drives on this driver that are to
;                       be assigned to drive letters.
;
;        Note:  The drive letter assigned to units of a second driver will
;          always be higher than those assigned to the first driver and
;          those assigned to a third driver will be higher than those
;          assigned to the second.
;
;        example: SHSUCDX
;
;        SHSUCDX finds the first available drive letter and assigns it
;        to device unit 0 of the default driver SHSU-CDN.  If there is a
;        second and/or third CD drive they are assigned to the next avail-
;        able letters in sequence.  Drive letters in use are skipped.  The
;        first CD supported by a driver is device unit 0 regardless of its
;        SCSI address.
;
;        example: SHSUCDX /D:CD001,F,,1  /D:SHSU-CDN,,1
;
;        SHSUCDX assigns drive F to device unit 0 of the driver CD001.
;        Units 1 and 2 of driver SHSU-CDN are then assigned to the next
;        available letters.
;
;        example: SHSUCDX /D:CD001,,1,1  /D:CD001,,4,1
;
;        SHSUCDX assigns the first available drive letter to device unit 1
;        of the driver CD001 and drive unit 4 to the next.  This allows
;        access to non-contiguous drive units without having to support
;        un-needed units.
;
;     unload: SHSUCDX [-u|/u]
;
;     The following INT 2F, 15h functions are supported:
;
;        00      Get number of CD-ROM drives
;        01      Get CD-ROM drive device list
;        02      Get Copyright File ID
;        03      Get Abstract File ID
;        04      Get Bibliographic File ID
;        05      Read VTOC (1st only)
;        08      Absolute disk read
;        0B      CDROM drive check
;        0C      MSCDEX version
;        0D      Get CD-ROM drive letters
;        0F      Get directory Entry
;        10      Send device request
;
;   SHSUCDX is a copyright reserved, free use program.
;
;   (c)John H. McCoy, 1994 - 2000 Sam Houston St. Univ., TX 77341-2206
;
; ***************************************************************************
;
;    Microsoft has not documented the redirector functions.  I have borrowed
;      from and am particularly indebted to the authors of:
;
;      A CD-ROM redirector for HighSierra and ISO 9660 disks.
;         Jim Harper, DDJ, March 1993
;      Inside the ISO-9660 Filesystem Format
;         William and Lynne Jolitz, DDJ, December 1992
;      Undocumented DOS, Chapter 4.
;         Andrew Schulman, et. al, Addison Wesley, 1990
;
;    Written for MASM 6.0b.  C functions compiled with MSC 5.1
; ***************************************************************************
;
; Modifications 3-4-94
;   test redir not network bit on call
;   set drive flags for physical network redir on install
;   test for physical network redir before clearing root
;
; Modifications 9-8-2000
;   changed cmds.c to findfirst/findnext
;

option nokeyword:<name type length >
option expr16

; make offsets group relative instead of segment relative
.model small

fptr  typedef  far ptr
nptr  typedef  near ptr

include undoc.inc

True              equ      1h
False             equ      0h
AsciiNul          equ      0
AsciiA            equ      'A'
cr                equ      0dh
lf                equ      0ah
QMark             equ      '?'
CDUNKNOWN         equ      0FFFFh

;  redirector equates
REDIR             equ      11h
InstallChk        equ      00h
UnInstallCmd      equ      04h
ChDir             equ      05h
Close             equ      06h
Flush             equ      07h
Read              equ      08h
Write             equ      09h
GetSpace          equ      0ch
SetAttr           equ      0eh
GetAttr           equ      0fh
Open              equ      16h
FindFirst         equ      1Bh
FindNext          equ      1Ch
Seek              equ      21h
PathName          equ      23h
TOF               equ      2dh         ; truncate open file
EOpen             equ      2eh

;  CDS offsets
RootSlashOff      equ      7
DriveOff          equ      2

;  MSCDEX equates
MSCDEX            equ      15h
MSCDEX_Q          equ      0DADAh
MSCDEX_R          equ      0ADADh

;

MAXDRIVES         equ      10
CACHESIZE         equ      10
SECTORSIZE        equ      2048

; declare protos, publics and externals

   DoChDir           proto near C
   DoRead            proto near C SFTp:fptr
   DoGetSpace        proto near C
   DoGetAttr         proto near C
   DoOpen            proto near C SFTp:fptr
   DoClose           proto near C SFTp:fptr
   DoFindFirst       proto near C
   DoFindNext        proto near C
   DoSeek            proto near C SFTp:fptr
   InitDrive         proto near C
   ForUs             proto near C DriveLetter: byte
   SetDDD            proto near C DriveLetter: byte
   MsgOut            proto near C msg:near ptr char
   CdReadLong        proto near C IOBuf:fptr,BlkNo:dword,NumBlks:word
   CDMediaChanged    proto near C
   PathLook          proto near C Dp:fptr, Pathp:fptr
   GetCABID          proto near C ActionCode:byte, XBufp:fptr
   ToHex             proto near C Num:word
   Main2F            proto near
   ParseCommandLine  proto near
   ClrRoot           proto near
   SetRoot           proto near C CDSx:fptr

   PUBLIC C FN1p, DTApp, PSPp, SAttrp
   PUBLIC C DosDp, SDBp
   PUBLIC C DriveNo, DriveIndex, DeviceUnit, NoDrives
   PUBLIC C CDSBase, CDSLen, CDSp
   PUBLIC C Drive, IODatap
   PUBLIC C _FLAGS,_AX,_BX,_CX,_DX,_SI,_DI,_ES
   PUBLIC _DirCache, _IOData
DGROUP   group   _TEXT, _DATA, _BSS, C_COMMON, CONST, _INIT

; this is the way C wants it

_TEXT    segment word PUBLIC 'CODE'
         assume cs:DGROUP, ds:DGROUP
_TEXT    ends

_DATA    segment word PUBLIC 'DATA'
         assume ds:DGROUP
_DATA    ends

_BSS     segment word PUBLIC 'BSS'
         assume ds:DGROUP
_BSS     ends

C_COMMON     segment word PUBLIC 'BSS'
             assume ds:DGROUP
C_COMMON     ends

CONST    segment word PUBLIC 'CONST'
         assume ds:DGROUP
CONST    ends

_INIT    segment word PUBLIC 'INIT'
         assume cs:DGROUP, ds:DGROUP
_INIT    ends


EndOfCDX segment para STACK
   byte "This STACK SEGMENT is here to satisfy loadhigh when using netx"
EndOfCDX ends

_DATA    segment

         byte "(c)John H. McCoy, 1996, csc_jhm@shsu.edu"

align 2

local_stack       word     128 dup ('ss')
top_stack         word     DGROUP:$

DevHeader         fptr     ?
DevStrategy       fptr     ?
DevInterrupt      fptr     ?
Old2F             fptr     ?
DataSeg           word     ?
_FLAGS            word     ?
_PSP              word     ?
_SP               word     ?
_SS               word     ?
_AX               word     ?
_BX               word     ?
_CX               word     ?
_DX               word     ?
_SI               word     ?
_DI               word     ?
_DS               word     ?
_ES               word     ?
PSPp              fptr     ?
FN1p              fptr     ?
SAttrp            fptr     ?
DosDp             fptr     ?
SDBp              fptr     ?
DTApp             fptr     ?
CDSBase           fptr     ?
CdsLen            word     ?       ; for this DOS version
CDSp              fptr     ?       ; must be recalced for current drive
DrvEntLen         word     sizeof DrvEnt
IODatap           word     _IOData
DirCachep         word     _DirCache

align 1

Active            byte     0h
FirstDriveNo      byte     0
NoDrives          byte     MAXDRIVES
DriveNo           byte     ?
DeviceUnit        byte     ?
DriveIndex        byte     ?
ChainFlag         byte     False

rh_hdr             byte  27 dup(?);

; ioctl in control blocks

IoCB_MediaChange     byte   9
     MediaChange     byte   ?   ; 0    not changed
                                ; 1    dont't know
                                ; 0FFh media changed

IoCB_Status          byte   6   ; ioctl get status subcommand
                     dword  ?   ; status  bit 0  0 door closed
                                ;                1 door open
                                ;         bit 1  0 door locked
                                ;                1 door unlocked
                                ;         bit 4  0 data read only
                                ;                1 data read and play audio
                                ;         bit 7  0 no prefetching
                                ;                1 supports prefetching
                                ;         bit 9  0 HSG addressing only
                                ;                1 HSG and RedBook audio addr
                                ;         bit 11 1 no CD in drive (best guess)

; ioctl out control blocks

IoCB_EjectCD         byte  0   ; ioctl out eject subcommand
  ; Note!!  It may be necessary to unlock a drive before the CD can be ejected.

IoCB_LockCD          byte  1   ; ioctl out lock/unlock subcommand
                     byte  1   ; lock code

IoCB_UnLockCD        byte  1   ; ioctl out lock/unlock subcommand
                     byte  0   ; unlock code


_DATA    ends

_TEXT    segment

New2F    proc    far

   push     ax
   inc      cs:Active

   ; is this call for us?

   .if (((ah != REDIR) && (ah != MSCDEX)) || (cs:Active > 1))
      dec      cs:Active
      pop      ax
      jmp      cs:Old2F                   ; chain out
   .endif

   pop      ax

   ; Handle REDIR install checks now, others after saving regs
   .if ah == REDIR
      .if al == InstallChk
         push bp
         mov  bp, sp
         .if [bp].frame.fr_Parm1 == MSCDEX_Q
            mov      [bp].frame.fr_Parm1, MSCDEX_R
         .endif
         mov      ax,0ffh
         pop      bp
         jmp      Fexit
      .elseif al == UnInstallCmd
         .if bx == MSCDEX_Q
            invoke  ClrRoot
            mov   es, cs:_PSP                ; tsr's psp
            mov   es:[16h], cx               ; make unloader parent
            mov   es:[0ah], dx               ; set terminate address
            mov   dx, ds
            mov   es:[0ah+2h], dx            ; in psp
            mov   ah, 50h                    ; make TSR psp current psp
            mov   bx, es
            int   21h
            mov   dx, word ptr cs:Old2F       ; restore vector
            mov   ds, word ptr cs:Old2F[2]
            mov   ax, 252Fh
            int   21h
            mov   ah,4ch                     ; normal terminate
            int   21h                        ; will take us back to unloader
         .else
            dec      cs:Active
            jmp      cs:Old2F                   ; chain out
         .endif
      .endif
   .endif

   ; set up data addressing
   mov      cs:_DS, ds
   push     cs
   pop      ds

   ;  save registers and switch to local stack
   mov      _AX, ax
   mov      _BX, bx
   mov      _CX, cx
   mov      _DX, dx
   mov      _SI, si
   mov      _DI, di
   mov      _ES, es

   push      bp
   mov       bp, sp
   mov       ax, [bp].frame.fr_Flags
   mov      _FLAGS, ax
   mov      _SP, sp
   mov      _SS, ss

   cli
   push     cs
   pop      ss
   mov      sp, top_stack
   sti

   INVOKE   Main2F

   ;  switch back to callers stack and restore registers

   cli
   mov      ss, _SS
   mov      sp, _SP
   sti

   mov      bp, sp
   mov      ax, _FLAGS
   mov      [bp].frame.fr_Flags, ax
   mov      ax, _AX
   mov      bx, _BX
   mov      cx, _CX
   mov      dx, _DX
   mov      si, _SI
   mov      di, _DI
   mov      es, _ES
   mov      ds, _DS
   pop      bp


   .if   cs:ChainFlag
      dec      cs:Active
      jmp      cs:Old2F
   .endif

Fexit:
   dec      cs:Active
   iret

New2F endp

Main2F   proc  near


   mov      ChainFlag, False
   mov      ax, _AX

   .if (ah == MSCDEX)
      ; Handle the MSCDEX calls
      .if al == 00h     ; get number of drive letters
         mov   al,NoDrives
         cbw
         mov   _BX,ax
         mov   al,FirstDriveNo
         cbw
         mov   _CX,ax
         and   _FLAGS, 0FFFEh                ; A Ok
         jmp   MExit
      .elseif al == 01h ; get drive device list
         sub      si, si
         sub      cx, cx
         .while (cl< NoDrives)
             mov   ch, [si+Drive.Unit]
             mov   byte ptr es:[bx], ch
             mov   ax, word ptr [si+Drive.DevHdrp]
             mov   word ptr es:[bx+1], ax
             mov   ax, word ptr [si+Drive.DevHdrp+2]
             mov   word ptr es:[bx+3], ax
             add   si, sizeof DrvEnt
             add   bx, 5
             inc   cl
         .endw
         and   _FLAGS, 0FFFEh                ; A Ok
         jmp   MExit
      .elseif (al == 02h || al == 03h || al == 04h)
      ; Get Copyright, Abstract or Biblio File Name
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  ForUs, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 015h        ; error, invalid drive
         or     _FLAGS, 01h
         jmp    MExit
     @@:
         mov    ax, _AX
         mov    bx, _BX
         mov    es, _ES
         invoke GetCABID, al, es::bx
         and    _FLAGS, 0FFFEh                ; A Ok
         jmp    MExit
      .elseif al == 05h ; Read VTOC   not complete!   we only read first one
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  ForUs, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 015h        ; error, invalid drive
         or     _FLAGS, 01h
         jmp    MExit
     @@:
         mov   es, _ES
         mov   bx, _BX
         .if (dx==0)                                               ;;1-10-96
            invoke CdReadLong, es::bx, 10h, 1
            .if  (ax == 100h)             ; normal device done
               mov   _AX, 01
               and   _FLAGS, 0FFFEh
            .else
               mov      _AX, 021h          ; drive not ready
               or     _FLAGS, 01h
            .endif
                                                                   ;;1-10-96
         .else
            invoke CdReadLong, es::bx, 11h, 1
            .if  (ax == 100h)             ; normal device done
               mov   _AX, 0FFh
               and   _FLAGS, 0FFFEh
            .else
               mov      _AX, 021h          ; drive not ready
               or     _FLAGS, 01h
            .endif
         .endif
                                                                   ;;1-10-96
         jmp      MExit
      .elseif al == 08h ; Absolute disk read
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  ForUs, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 015h        ; error, invalid drive
         or     _FLAGS, 01h
         jmp    MExit
     @@:
         .if (_DX == 0)
            mov ax, 100h
         .else
            mov   si, _SI
            mov   di, _DI
            mov   es, _ES
            mov   bx, _BX
            invoke CdReadLong, es::bx,si::di,_DX
         .endif
         .if  (ax == 100h)
            mov   _AX, 01
            and   _FLAGS, 0FFFEh
         .else
            mov      _AX, 021h          ; drive not ready
            or     _FLAGS, 01h
         .endif
         jmp      MExit
      .elseif al == 0Bh ; CDROM check
         mov      _BX, MSCDEX_R
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  ForUs, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 0h          ; drive not supported by REDIR
         jmp    MExit
     @@:
         mov     _AX, 0FFh
         jmp      MExit
      .elseif al == 0Ch ; MSCDEX version
         mov   _BX, 0215h                 ; report ver 2.21
         and   _FLAGS, 0FFFEh               ; A Ok
         jmp   MExit
      .elseif al == 0Dh ; drive letters
         mov      ah, 0ffh
         sub      si, si
         sub      cx, cx
         .while (cl< NoDrives)
             mov   ch, byte ptr [si+Drive.No]
             mov   byte ptr es:[bx], ch
             add   si, sizeof DrvEnt
             inc   bx
             inc   cl
         .endw
         and   _FLAGS, 0FFFEh                ; A Ok
         jmp   MExit
      .elseif al == 0Fh ; Get Directory Entry
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  ForUs, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 015h        ; error, invalid drive
         or     _FLAGS, 01h
         jmp    MExit
     @@:
         mov    es, _ES
         mov    bx, _BX
         mov    si, _SI
         mov    di, _DI
         and   _FLAGS, 0FFFEh
         invoke PathLook,si::di,es::bx
         jmp   MExit
      .elseif al == 10h ; device request
         .if ( cl < 26 )
             add     cl, 'A'
             invoke  SetDDD, cl    ; sets DriveNo, DeviceUnit and DriveIndex
            .if   !(ax)
              jmp @F
            .endif
         .endif
         mov    _AX, 015h        ; error, invalid drive
         or     _FLAGS, 01h
         jmp    MExit
     @@:
         mov    ax, sizeof DrvEnt
         mul    DriveIndex
         mov    bx, ax
         mov    ax, word ptr [bx+Drive.Strategyp]
         mov    word ptr DevStrategy, ax
         mov    ax, word ptr [bx+Drive.Strategyp+2]
         mov    word ptr DevStrategy+2, ax
         mov    ax, word ptr [bx+Drive.Interruptp]
         mov    word ptr DevInterrupt, ax
         mov    ax, word ptr [bx+Drive.Interruptp+2]
         mov    word ptr DevInterrupt+2, ax
         push   ds
         mov    es, _ES                ; restore rh ptr
         mov    bx, _BX
         mov    ah, DeviceUnit
         mov    byte ptr es:rh.SubUnit[bx], ah
         call   cs:DevStrategy
         call   cs:DevInterrupt
         pop    ds                   ; in case device driver wiped out ds
         mov    es, _ES                ; restore rh ptr
         mov    bx, _BX
         .if    (es:rh.Command[bx] == rhcmdIOCTL_In)   ; on some drives we
            les  bx,es:rhIOCTL.CBPtr[bx]               ; will miss media change
            .if  (byte ptr es:[bx] == 09h)           ; if direct access checks
               .if (byte ptr es:[bx+1] != 1)         ; so, if media has changed
                  mov    ax, sizeof DrvEnt           ; set type to unknown to
                  mul    DriveIndex                  ; force re-read on next
                  mov    bx, ax                      ; non-direct access
                  mov    word ptr [bx+Drive.Type],CDUNKNOWN
               .endif
            .endif
         .endif
         and   _FLAGS, 0FFFEh        ; A Ok from our perspective
         jmp    MExit
      .else                               ; we don't support it
         mov   _AX,01h                    ; call it an error
         or    _FLAGS, 01h
         jmp   MExit
      .endif
   .endif

   ;  handle redirector calls

   .if (al == PathName)
       mov    ChainFlag, True
       jmp    MExit
   .elseif  (al == FindFirst || al == Open || al == EOpen ||    \
             al == ChDir || al == GetAttr ||  al == TOF)
       les     di, FN1p
       .if  ((byte ptr es:[di+DriveOff] < 'A') ||                        \
             (byte ptr es:[di+DriveOff] > 'Z')  )
           mov    ChainFlag, True
           jmp    MExit
       .endif
       invoke  ForUs, byte ptr es:[di+DriveOff]

       .if   (ax == 0)
          mov    ax, _AX                 ; restores ax
       .else
          .if (ax == 1)
             mov    ChainFlag, True
          .else
             mov    _AX, 015h              ; drive not ready
          .endif
          jmp    MExit
       .endif
       .if al == FindFirst
          invoke   DoFindFirst
       .elseif al == Open || al == EOpen
           mov   al, DriveNo
           cbw
           mul      CDSLen
           les      bx, CDSBase
           add      ax, bx
           mov      word ptr CDSp, ax
           mov      ax, es
           mov      word ptr CDSp+2, ax
           mov      di, _DI
           mov      bx, _BX
           mov      es, _ES
           invoke   DoOpen, es::di
       .elseif al == ChDir
           invoke   DoChDir
       .elseif  al == GetAttr
           invoke   DoGetAttr
           jmp    MExit
       .elseif  al == TOF             ; haven't got a clue about what to do
           and   _FLAGS, 0FFFEh         ; just say we did it
           mov    _AX, 0h             ; and flash 'em a smile
           jmp    MExit
       .endif
   .elseif al == FindNext
       les      di, SDBp                   ; get SDBp
       mov      al, byte ptr es:[di]       ; SDBp->DriveLet (A - 0)
       test     al, 40h                    ; redir not network when set
       jnz      @F
       mov      ChainFlag, True            ; not for us, chain out
       jmp      MExit
   @@: and      al, 01fh                   ; this part is actual drive letter
       add      al, 'A'
       invoke ForUs, al
       .if   (ax == 0)
          mov    ax, _AX
       .else
          .if (ax == 1)
             mov    ChainFlag, True
          .else
             mov    _AX, 015h              ; drive not ready
          .endif
          jmp    MExit
       .endif
       invoke   DoFindNext
   .elseif (al == Close || al == Read ||  al == Seek)
       mov      ax, es:[di].SFT.Flags
       test     al, 40h                    ; redir not network when set
       jnz      @F
       mov      ChainFlag, True            ; not for us, chain out
       jmp      MExit
   @@: and      ax, 01fh
       add      al, 'A'
       invoke ForUs, al
       .if   (ax == 0)
          mov    ax, _AX
          mov      es, _ES
          mov      di, _DI
       .else
          .if (ax == 1)
             mov    ChainFlag, True
          .else
             mov    _AX, 015h              ; drive not ready
          .endif
          jmp    MExit
       .endif
       .if al == Read
          invoke   DoRead, es::di
       .elseif al == Seek
          invoke   DoSeek, es::di
          jmp    MExit
       .elseif al == Close
          invoke   DoClose, es::di
       .endif
   .elseif al == GetSpace                ; es:di is curr CDSp
       .if ((byte ptr es:[di+DriveOff] < 'A') ||                  \
             (byte ptr es:[di+DriveOff] > 'Z')  )
           mov    ChainFlag, True
           jmp    MExit
       .endif
       invoke  ForUs, es:[di+DriveOff]
       .if   (ax == 0)
           invoke DoGetSpace
       .else
          .if (ax == 1)
             mov    ChainFlag, True
          .else
             mov    _AX, 015h              ; drive not ready
          .endif
       .endif
       jmp    MExit                ; this calls not for me
   .else
       mov    ChainFlag, True      ; just chain out.  Novell doesn't like
       jmp    MExit                ; it if you call it an error
   .endif
   mov      _AX, ax

MExit:

   ret

Main2F  endp

CdReadLong  proc near C uses ds es bx si di, IOBufp: fptr,BlkNo: dword,NumBlks: word
         mov   ax, sizeof DrvEnt
         mul   DriveIndex
         mov   bx, ax
         mov   ax, word ptr [bx+Drive.Strategyp]
         mov   word ptr DevStrategy, ax
         mov   ax, word ptr [bx+Drive.Strategyp+2]
         mov   word ptr DevStrategy+2, ax
         mov   ax, word ptr [bx+Drive.Interruptp]
         mov   word ptr DevInterrupt, ax
         mov   ax, word ptr [bx+Drive.Interruptp+2]
         mov   word ptr DevInterrupt+2, ax
         lea   bx, rh_hdr
         mov   rh.Length[bx], 27
         mov   ah, DeviceUnit
         mov   rh.SubUnit[bx], ah
         mov   rh.Command[bx], rhcmdReadLong         ; read long
         mov   ax, word ptr IOBufp
         mov   word ptr rhReadLong.Bufp[bx], ax
         mov   ax, word ptr IOBufp[2]
         mov   word ptr rhReadLong.Bufp[bx+2], ax
         mov   ax, word ptr BlkNo
         mov   word ptr rhReadLong.StartBlk[bx], ax
         mov   ax, word ptr BlkNo[2]
         mov   word ptr rhReadLong.StartBlk[bx+2], ax
         mov   ax, NumBlks
         mov   word ptr rhReadLong.Count[bx], ax
         xor   ax, ax                          ; zero for
         mov   rhReadLong.AddrMode[bx], al  ; hsc addressing
         mov   rhReadLong.ReadMode[bx], al  ; cooked mode
         mov   rhReadLong.ISize[bx], al
         mov   rhReadLong.ISkip[bx], al
         push  cs
         pop   es
         call  cs:DevStrategy
         call  cs:DevInterrupt
         mov   ax, word ptr cs:rh_hdr+rh.status
         ret
CdReadLong  endp

CdMediaChanged  proc near C uses ds es bx si di
         mov   ax, sizeof DrvEnt
         mul   DriveIndex
         mov   bx, ax
         mov   ax, word ptr [bx+Drive.Strategyp]
         mov   word ptr DevStrategy, ax
         mov   ax, word ptr [bx+Drive.Strategyp+2]
         mov   word ptr DevStrategy+2, ax
         mov   ax, word ptr [bx+Drive.Interruptp]
         mov   word ptr DevInterrupt, ax
         mov   ax, word ptr [bx+Drive.Interruptp+2]
         mov   word ptr DevInterrupt+2, ax
         mov   bx, offset rh_hdr
         mov   rh.Length[bx], 26
         mov   ah, DeviceUnit
         mov   rh.SubUnit[bx], ah
         mov   rh.Command[bx], rhcmdIOCTL_In
         mov   ax, OFFSET IoCB_MediaChange
         mov   word ptr rhIOCTL.CBPtr[bx], ax
         mov   ax, SEG IoCB_MediaChange
         mov   word ptr rhIOCTL.CBPtr[bx+2], ax
         mov   word ptr rhIOCTL.BytesToTransfer[bx], size IoCB_MediaChange
         xor   ax, ax                         ;  zero out other fields
         mov   rhIOCTL.MediaDesc[bx], al
         mov   rhIOCTL.StartSector[bx], ax
         mov   word ptr rhIOCTL.VolidPtr[bx], ax
         mov   word ptr rhIOCTL.VolidPtr[bx+2], ax
         push  cs
         pop   es
         call  cs:DevStrategy
         call  cs:DevInterrupt
         mov   al, cs:MediaChange       ; use cs in case driver wiped out ds
         cbw
         ret
CdMediaChanged  endp

ClrRoot     proc near uses es bx ax cx dx si
       sub      si, si
       sub      cx, cx
       .while (cl< cs:NoDrives)
           mov   ch, byte ptr cs:[si+Drive.No]
           mov   al, ch
           cbw
           mul      cs:CDSLen
           les      bx, cs:CDSBase
           add      bx, ax
           test     es:[bx].CDS.Flags, 0C080h ; physical net redir drive ?
           jz      @F                         ; couldn't be our drive
           mov   ax, 0
           mov   es:[bx].CDS.Flags, ax        ; clear drive flags
           mov   al, ch
           add   al, 'A'
           mov   es:[bx].CDS.CurrPath, al
           mov   al, ':'
           mov   es:[bx+1].CDS.CurrPath, al
           mov   al, '\'
           mov   es:[bx+2].CDS.CurrPath, al
           mov   al, 0
           mov   es:[bx+3].CDS.CurrPath, al
       @@: add   si, sizeof DrvEnt
           inc   cl
       .endw
            ret
ClrRoot     endp

_TEXT    ends

_INIT     segment


ToHex        proc near C public uses ax bx cx dx, Num:word
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


; set up for one drive minimum
; drive table, cache and buffers are set up dynamically here at run time

align 2
Drive             DrvEnt   1 dup (<>)         ;MAXDRIVES dup (<>)
_DirCache         DirEnt   CACHESIZE dup (<>) ;MAXDRIVES*CACHESIZE dup (<>)
_IOData           byte     SECTORSIZE dup('B')

; insert filler so we don't wipe out INIT when whe init drive table and
;   link up dir cache

if (MAXDRIVES-1)*(sizeof DrvEnt+CACHESIZE*sizeof DirEnt)-SECTORSIZE GT 0
 byte (MAXDRIVES-1)*(sizeof DrvEnt+CACHESIZE*sizeof DirEnt)-SECTORSIZE dup (0)
endif

;============================================================================
;  everything below this line is discarded after installing
;    the redirector

;     MSC pacifier

     PUBLIC C  _acrtused

_acrtused     dw    1

; Credits

CopyrightMsg   byte    cr, lf
               byte    'SHSUCDX Version 1.4b',cr,lf
               byte    '(c)John H. McCoy, October 2000, '
               byte    'Sam Houston State University'
               byte    cr,lf, '$'

DrivesAssigned byte    cr, lf
               byte    "SHSUCDX Installed.",cr,lf
               byte    "  Drives Assigned",cr,lf
               byte    "Drive  Driver   Unit",cr,lf,'$'

UnInstalledMsg byte    cr, lf
               byte    'SHSUCDX un-installed and memory freed.'
               byte    cr, lf, '$'

CantUnInstallMsg  byte    cr, lf
                  byte    'SHSUCDX can''t un-install.'
                  byte    cr, lf, '$'

CantInstallMsg byte    '  SHSUCDX can''t install.',cr,lf,'$'

AlreadyInstalledMsg  byte   cr, lf, 'SHSUCDX or MSCDEX is already installed.$'

NoDrivesAvailMsg  byte  cr, lf,'Need More Drive Letters.$'

HighDriveMsg    byte    cr, lf,'Drive letter to high.$'

HighUnitMsg    byte    cr, lf,'Units specified don''t exist.$'

WrongDOSMsg    byte    cr, lf,'Must be DOS 3.3 - 7.xx.$'

CantFindCdMsg  byte    'Can''t open CD driver $'

NotEnoughMemMsg byte   'Not enough memory.  $'

CRLF           byte    cr, lf, '$'


DrvrEnt        struct
   Name        byte    'SHSU-CDN'
               byte    0
   DrvrAddr    fptr    ?
   Drive       byte    0
   Unit        byte    0
   NoWanted    byte    0
DrvrEnt        ends

NoDrivers        byte    ?
Drivers          DrvrEnt  MAXDRIVES dup (<>)  ; one driver per drive is max
DriverIndex      byte     ?
FirstDeviceUnit  byte     ?
NoDeviceUnits    byte     ?
NoAvailUnits     byte     ?
NoUnitsWanted    byte     ?
LastDOSDrive     byte     ?      ; 1 base
KeepSize         word     ?

ArgumentNotFound EQU     2       ; Unrecognized argument
NoArgumentsFound EQU     1       ; No argument in command line
ArgumentFound    EQU     0       ; Ok argument in command line

UnInstallIt      equ     2
DontInstallIt    equ     1
InstallIt        equ     0

InstallFlag      byte    0
QuietFlag        word    0

DosVer           label    word
   osMajor       byte     ?
   osMinor       byte     ?
IoctlInBuf       byte     5 dup (0)      ; get devhdr addr
IoctlOutBuf      byte     2              ; reset CD

MsgOut    proc  near C public uses ax bx dx, msg:near ptr char
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

DisplayDrives     proc near
      invoke MsgOut, addr DrivesAssigned
      sub      si, si
      sub      cx, cx
      .while (cl< cs:NoDrives)
         mov      ah, 02h       ; display ch function
         mov   dl, ' '
         int      21h
         int      21h
         mov   dl, byte ptr [si+Drive.Letter]
         int      21h
         mov   dl, ':'
         int      21h
         mov   dl, ' '
         int      21h
         int      21h
         int      21h
         sub   bx, bx
         .while (bx < 8)
            mov      dl, [si+bx+Drive.DriverName]
            int      21h        ; output driver name
            inc      bx
         .endw
         mov   dl, ' '
         int      21h
         int      21h
         mov   dl, byte ptr [si+Drive.Unit]
         .if (dl > 9)            ; output drive unit
            xor   dh,dh
            .while (dl > 9)
               inc  dh
               sub  dl, 10
            .endw
            xchg    dl, dh
            add     dl, 30h
            int     21h          ; tens digit
            xchg    dl, dh
          .endif
         add   dl, 30h
         int      21h            ; units digit
         add   si, sizeof DrvEnt
         inc   cl
         invoke MsgOut, addr CRLF
      .endw
            ret
DisplayDrives  endp

SetRoot     proc near C uses es ax bx cx di si , CDSx: fptr
            les   bx, CDSx
            mov   ax, 0C080h              ; set physical network & redir bits
            mov   es:[bx].CDS.Flags, ax
       ; we don't set redirector address.  Should we???
       ; we do now 10-2000
            mov   word ptr es:[bx].CDS.Redir,offset New2F
            mov   word ptr es:[bx].CDS.Redir+2,cs
            mov   es:[bx].CDS.RootOff,RootSlashOff ; root \ in curr_path
       ; Set to CDS to CD root form \\D.\U.
            mov   al, '\'
            mov   es:[bx].CDS.CurrPath, al
            mov   es:[bx+1].CDS.CurrPath, al
            mov   al, cs:DriveNo
            add   al, 'A'
            mov   es:[bx+2].CDS.CurrPath, al
            mov   al, '.'
            mov   es:[bx+3].CDS.CurrPath, al
            mov   al, '\'
            mov   es:[bx+4].CDS.CurrPath, al
            mov   al, cs:DeviceUnit
            add   al, 'A'
            mov   es:[bx+5].CDS.CurrPath, al
            mov   al, '.'
            mov   es:[bx+6].CDS.CurrPath, al
            mov   al, 0
            mov   es:[bx+7].CDS.CurrPath, al
            mov    ax, sizeof DrvEnt
            mul    DriveIndex
            mov    bx, ax
            mov    al, DriveNo
            mov    [bx+Drive.No], al
            add    al, 'A'
            mov    [bx+Drive.Letter], al
            mov    al, DeviceUnit
            mov    [bx+Drive.Unit], al
            mov    ax, word ptr DevHeader
            mov    word ptr [bx+Drive.DevHdrp], ax
            mov    ax, word ptr DevHeader+2
            mov    word ptr [bx+Drive.DevHdrp+2], ax
            mov    word ptr [bx+Drive.Strategyp+2], ax
            mov    word ptr [bx+Drive.Interruptp+2], ax
            mov    ax, word ptr DevStrategy
            mov    word ptr [bx+Drive.Strategyp], ax
            mov    ax, word ptr DevInterrupt
            mov    word ptr [bx+Drive.Interruptp], ax
            lea    di, [bx+Drive.DriverName]
            mov    ax, ds
            mov    es, ax
            lea    si, [si+Drivers.Name]
            mov    cx, 4
            cld
            rep movsw               ; ds:si->es:di
            ret
SetRoot     endp

InitDrive   proc near C uses ax bx di si es
   local    DrvEntryp:nptr
   local    DirEntryp:nptr
   local    Bufp:nptr
   local    Ticks:word

   ; fill in drive entry first
      mov   ax, 0040h             ; get clock ticks from 0040006Ch
      mov   es, ax
      mov   ax, es:[06Ch]
      mov   Ticks, ax
      push  ds
      pop   es
      mov   al, DriveIndex
      cbw
      mov   bx, SectorSize
      mul   bx
      add   ax, word ptr IODatap
      mov   Bufp, ax              ; addr of buffer for this drive
      mov   al, DriveIndex
      cbw
      mov   bx, CACHESIZE
      mul   bx
      mov   bx, sizeof DirEnt
      mul   bx
      add   ax, DirCachep         ; DirCache base ptr
      mov   DirEntryp, ax         ; addr of first cache entry for this drive
      mov   ax, sizeof DrvEnt     ; assume < 256
      mul   DriveIndex
      add   ax, offset Drive
      mov   bx, ax
      mov   ax, Bufp
      mov   [bx + DrvEnt.Bufp], ax
      mov   ax, CDUNKNOWN
      mov   DrvEnt.Type[bx], ax
      mov   ax, 0FFFFh
      mov   word ptr DrvEnt.BufBlkNo[bx], ax
      mov   word ptr DrvEnt.BufBlkNo[bx+2], ax
      mov   ax, Ticks
      mov   DrvEnt.LastAccess[bx], ax
   ; link up cache for dir entries
      mov   ax, DirEntryp
      mov   DrvEnt.RootEnt.Forw[bx], ax
      mov   di, ax
      mov   si, ax
      add   si, sizeof DirEnt
      mov   DirEnt.Forw[di], si
      lea   ax, DrvEnt.RootEnt[bx]
      mov   DirEnt.Back[di], ax
      mov   cx, CACHESIZE
      dec   cx
      .while (cx > 0)
         mov   DirEnt.Back[si], di
         mov   di, si
         add   si, sizeof DirEnt
         mov   DirEnt.Forw[di], si
         dec   cx
      .endw
      lea   ax, DrvEnt.RootEnt[bx]
      mov   DirEnt.Forw[di], ax           ; last entry points forw to root
      mov   DrvEnt.RootEnt.Back[bx], di   ; root points back to last entry

      ret
InitDrive   endp

Init:  PUBLIC Init

   ;  set DS for addressing, move stack and save our psp address
      mov      ax, cs
      mov      ds, ax
      mov      ss, ax
      mov      sp, top_stack
      mov      DataSeg, ds
      mov      _PSP, es

   ;  get command line parameters

      invoke ParseCommandLine

      .if (InstallFlag == UnInstallIt)
         jmp   UnInstall
      .endif

   ;  see if we are already installed
      mov      ax, MSCDEX_Q
      push     ax
      mov      ah, REDIR
      mov      al, InstallChk
      int      2fh
      pop      bx
      .if al == 0ffh        ; redir installed, is it MSCDEX?
         .if bx == MSCDEX_R
            jmp      AlreadyInstalled
         .endif
      .endif


   ;  get DOS version
      mov      ah, 30h
      int      21h
      mov      DosVer, ax

   ;  get list of lists address thereby getting DOS seg
      mov      ah, 52h
      int      21h                        ; es:bx is LOLp
      mov      word ptr DTApp+2, es       ; set seg of DOS ptrs
      mov      word ptr PSPp+2, es
      mov      word ptr FN1p+2, es
      mov      word ptr SDBp+2, es
      mov      word ptr DosDp+2, es
      mov      word ptr SAttrp+2, es
      mov      ax, word ptr es:[bx].DOS_LOL.CDS
      mov      word ptr CDSBase, ax
      mov      ax, word ptr es:[bx].DOS_LOL.CDS+2
      mov      word ptr CDSBase+2, ax

   ;  set version specific DOS parameters
      .if osMajor ==3 && osMinor >= 30
         mov      CdsLen, 51h
         mov      si, 02ceh                    ; version 3.3+ offset
         mov      ax, si
         add      ax, 0ch
         mov      word ptr DTApp, ax
         mov      ax, si
         add      ax, 10h
         mov      word ptr PSPp, ax
         mov      ax, si
         add      ax, 92h
         mov      word ptr FN1p, ax
         mov      ax, si
         add      ax, 192h
         mov      word ptr SDBp, ax
         mov      ax, si
         add      ax, 1a7h
         mov      word ptr DosDp, ax
         mov      ax, si
         add      ax, 23ah
         mov      word ptr SAttrp, ax

      .elseif  osMajor >= 4       ; assume subsequent versions are the same
         mov      CdsLen, 58h     ; offsets for version 4+
         mov      si, 0320h
         mov      ax, si
         add      ax, 0ch
         mov      word ptr DTApp, ax
         mov      ax, si
         add      ax, 10h
         mov      ax, si
         add      ax, 9eh
         mov      word ptr FN1p, ax
         mov      ax, si
         add      ax, 19eh
         mov      word ptr SDBp, ax
         mov      ax, si
         add      ax, 1b3h
         mov      word ptr DosDp, ax
         mov      ax, si
         add      ax, 24dh
         mov      word ptr SAttrp, ax
      .else
         jmp      WrongDOS
      .endif

   ;  take advantage of the photo op

      invoke MsgOut, addr CopyrightMsg

   ; find last available drive letter
      mov      ch, es:[bx].DOS_LOL.LastDrive
      dec      ch                  ; lol use 1 for 'A'
      mov      LastDOSDrive, ch

    ; assign drives
      sub      ax, ax
      mov      DriveIndex, al
      mov      DriverIndex, al
      mov      DriveNo, al
      mov      ah, NoDrivers
      .while  (al < ah)                ; while DriverIndex < (NoDrivers)
      ; Open device driver and use IOCTL Input sub-command 0 to get
      ;   the device header address.
         mov      ax, sizeof DrvrEnt
         mul      DriverIndex
         add      ax, offset Drivers.Name
         mov      dx, ax
         sub      al, al                     ; read only
         mov      ah, 3Dh
         int      21h
         jnc      @f                         ; error when carry set
         jmp      CantFindCd
      @@:mov      bx, ax                     ; move handle to bx
         mov      ax, 4402h                  ; IOCTL input-get devhdr addr
         mov      cx, 5                      ; dta has cmd code plus a fptr
         lea      dx, IoctlInBuf             ;   to device header
         int      21h
;        mov      ax, 4403h                  ; IOCTL output-reset CD
;        mov      cx, 1                      ; dta has cmd code only (1 byte)
;        lea      dx, IoctlOutBuf            ;   to device header
;        int      21h
         mov      ah, 3eh                    ; close file handle in bx
         int      21h
         jnc      @f                         ; error when carry set
         jmp      CantFindCd
      @@:
         les      bx, dword ptr IoctlInBuf+1
         mov      word ptr DevHeader, bx
         mov      word ptr DevHeader+2, es
         mov      ax, es:[bx+6]
         mov      word ptr DevStrategy, ax
         mov      word ptr DevStrategy+2, es
         mov      ax, es:[bx+8]
         mov      word ptr DevInterrupt, ax
         mov      word ptr DevInterrupt+2, es
         mov      ax, sizeof DrvrEnt
         mul      DriverIndex
         mov      si, ax
         mov      al, [si + Drivers.Unit]       ; first unit wanted
         mov      ah, es:[bx+21]
         mov      NoDeviceUnits, ah
        .if  (ah <= al)
             invoke MsgOut, addr HighUnitMsg
             jmp InitErrorExit
         .endif
         sub      ah, al
         mov      NoAvailUnits, ah
         mov      DeviceUnit, al
         mov      al, [si + Drivers.NoWanted]   ; units asked for
         .if (al == 0) || ( al > NoAvailUnits)
            mov   al, NoAvailUnits
         .endif
         mov      NoUnitsWanted, al
         mov      cl,[si + Drivers.Drive]
         .if  (cl < DriveNo)
             mov  cl, DriveNo
         .endif
         mov      DriveNo, cl
         mov      ah, DriveIndex
     ; ah is DriveIndex   al is NoUnitsWanted
         .while (ah < MAXDRIVES) && (al > 0)
            mov      cl, DriveNo
            call     FindAvailDrive       ; also sets CDSp for us
            .if  (cl != ch)
               invoke MsgOut, addr NoDrivesAvailMsg
               jmp  InitErrorExit
            .endif
            mov      DriveNo, cl
            invoke SetRoot,CDSp           ; uses CDSp, DevHeader, DriveIndex,
                                          ; DriveNo and DeviceUnit
            inc     DriveIndex
            inc     DriveNo
            inc     DeviceUnit
            dec     NoUnitsWanted
            mov      ah, DriveIndex
            mov      al, NoUnitsWanted
         .endw
         inc      DriverIndex
         mov      al, DriverIndex
         mov      ah, NoDrivers
      .endw

      mov   al, DriveIndex
      .if (al == 0)                ; then no drives were assigned
         invoke MsgOut, addr HighDriveMsg
         jmp   InitErrorexit
      .endif
      mov      NoDrives, al

  ;  relocate buffers and init drive structs
      mov      cx, sizeof DrvEnt      ; find Drive Table space needed
      mov      al, NoDrives
      cbw
      mul      cx
      add      ax, offset Drive
      mov      DirCachep, ax          ; relocate dir cache
      mov      cx, CACHESIZE          ; find cache space needed
      mov      al, NoDrives
      cbw
      mul      cx
      mov      cx, ax
      mov      ax, sizeof DirEnt
      mul      cx
      add      ax, DirCachep
      mov      IODatap, ax            ; relocate IOData buffers
      mov      cx, SECTORSIZE         ; find buffer space needed
      mov      al, NoDrives
      cbw
      mul      cx
      add      ax, IODatap            ; last byte to keep now in ax
      add      ax, 10Fh               ; add 100h for psp and roundup
      mov      cl, 4
      shr      ax, cl                 ; program paragraphs to keep
      mov      KeepSize, ax

  ;  get more space if we need it for buffers
      mov     bx, seg EndOfCDX
      sub     bx, _PSP
      .if     ax > bx
          mov  bx, ax
          mov  es, _PSP
          mov  ah, 04Ah       ; modify block size
          int  21h
          jnc  @F
             invoke MsgOut, addr NotEnoughMemMsg
             jmp InitErrorExit
       @@:
       .endif

  ;  initialize drive table and link up dir cache

      xor      al, al
      mov      ah, NoDrives
      mov      DriveIndex, 0
      .while   al < ah
         invoke InitDrive
         inc DriveIndex
         inc al
      .endw


  ;  Display drive assignments

      mov      al, Drive.No[0]
      mov      FirstDriveNo, al
      invoke DisplayDrives

  ;  capture 2F vector

      mov      ax, 352Fh
      int      21h
      mov      word ptr Old2F ,bx
      mov      word ptr Old2F[2],es
      mov      dx, offset New2F
      mov      ax, 252Fh
      int      21h

 ;  release excess space and tsr

      mov      es, _PSP
      sub      ax, ax                 ;
      xchg     ax, es:[2Ch]           ; zap evironment pointer in psp
      or       ax, ax                 ; ax := environment ptr
      jz       @f                     ; no environment if zero
      mov      es, ax
      mov      ah, 49h
      int      21h                    ; and release environment
  @@:
      mov      dx, KeepSize
      mov      ax,3100h
      int      21h                    ; go TSR
      mov      ah, 4ch                ; emergency exit in case we don't TSR
      int      21h

UnInstall:
      push  bp
      mov   _SS, ss
      mov   _SP, sp
      mov   bx, MSCDEX_Q
      mov   cx, _PSP                ; our  psp
      mov   dx, offset UnInstalled  ; uses ds:dx for successful return
      mov   ah, REDIR
      mov   al, UnInstallCmd
      int   2fh
      jmp   CantUnInstall           ; comes back here if unsuccessful
UnInstalled:                        ; or, here if successful
      push   cs
      pop    ds
      mov   ss, _SS
      mov   sp, _SP
      pop   bp
      invoke MsgOut, addr UnInstalledMsg
      jmp   Initexit

CantUnInstall:
      pop    bp
      push   cs
      pop    ds
      invoke MsgOut, addr CantUnInstallMsg
      jmp   Initexit

AlreadyInstalled:
      invoke MsgOut, addr AlreadyInstalledMsg
      invoke MsgOut, addr CantInstallMsg
      jmp   Initexit

CantFindCd:
      invoke MsgOut, addr CantFindCdMsg
      mov      ax, sizeof DrvrEnt
      mul      DriverIndex
      add      ax, offset Drivers.Name
      invoke MsgOut, ax
      invoke MsgOut, addr CRLF
      jmp   InitErrorExit

WrongDOS:
      invoke MsgOut, addr WrongDOSMsg
      invoke MsgOut, addr CantInstallMsg
      jmp   InitExit

InitErrorExit:
   .if DriveIndex > 0
      mov      al, DriveIndex
      mov      NoDrives, al
      invoke ClrRoot
   .endif
   invoke MsgOut, addr CantInstallMsg
   jmp   InitExit

InitExit:
   mov   ah,4ch                     ; normal terminate
   int   21h

FindAvailDrive         proc near uses es bx

;  Finds first available drive letter.
;
;  Entry     cl     Starting letter for search
;
;  Exit      cl == ch   Drive in cl is available
;            cl != ch   No drive available starting at specified drive


      mov      al, cl
      cbw
      mul      CDSLen
      les      bx, CDSBase
      add      bx, ax             ; es:bx FirstDriveNo CDSp
      xor      ch, ch
      .while  (cl <= LastDOSDrive)
         test     es:[bx].CDS.Flags, 0C000h ; drive in use ?
         jnz      @F
         mov      ch, cl
         mov      word ptr CDSp, bx          ; set CDSp
         mov      word ptr CDSp+2, es
         .break
   @@:   inc      cl
         add      bx, CDSLen
      .endw
      ret

FindAvailDrive         endp

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
          mov      InstallFlag, UnInstallIt
          jmp      ParseExit
      .endif

      mov      di, 80h                ; command line length at psp +80h
      sub      ch, ch
      mov      cl, es:[di]
      mov      DriverIndex, 0
      mov      ax, ArgumentFound
   .while  ax == ArgumentFound
      mov      al, 'D'            ; /D:drivername
      call     FindParm
     .if   ax == ArgumentFound
         mov      ax, sizeof DrvrEnt
         mul      DriverIndex
         mov      si, ax
         mov      dx, 8
         call     MoveName
         mov al, es:[di]
         .if   cx == 0
             jmp     @F
         .endif
         .while (al != ',' && al != ' ')
            inc    di
            mov al, es:[di]
            dec    cx
            .if   cx == 0  || al == '/'
                jmp     @F
            .endif
         .endw
         dec    cx
         .if   cx == 0
             jmp     @F
         .endif
         inc    di
         mov al, es:[di]
         .if al == ' '
             jmp @F
         .elseif al != ','                         ; check first driveno
            .if (al >= 'a' && al <= 'z')
                and    al, 11011111y           ; upper case it
            .endif
            .if (al >= 'A' && al <= 'Z')
                sub    al, 'A'
            .else
                jmp @F
            .endif
            mov  [si+Drivers.Drive], al
            dec     cx
            jcxz    @F
            inc    di
            mov al, es:[di]
            .if al != ','
               jmp @F
            .endif
         .endif
         dec    cx
         jcxz     @F
         inc    di
         mov al, es:[di]
         .if al != ','                              ; check firstdeviceunit
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
                mov    [si+Drivers.Unit], al
            .else
                jmp @F
            .endif
            dec     cx
            jcxz     @F
            inc    di
         .endif
         dec     cx
         jcxz     @F
         inc    di
         mov al, es:[di]
         .if al != ','
            .if (al >= '0' && al <= '9')
                sub    al, '0'
                mov    [si+Drivers.NoWanted], al
            .else
                jmp @F
            .endif
         .endif
   @@:  inc    DriverIndex
        mov    ax, ArgumentFound
        dec    di                    ; find parm needs ptr -1
     .endif
   .endw
    mov   al, DriverIndex
    .if  al ==0
       inc al
    .endif
    mov   NoDrivers, al
ParseExit:
    ret

ParseCommandLine       endp

MoveName proc near
      sub   bx, bx                                 ; es:di points to 1st char
      .repeat                                      ; cx chars left on cmd line
          mov al, es:[di]                          ; dx is length of name field
          .if ((al == ',') || (cx == 0) || (al==' ') || (al == '/'))
              mov    byte ptr [si+bx+Drivers.Name], ' '
          .else
             .if (al >= 'a' && al <= 'z')
                 and    al, 11011111y           ; upper case it
              .endif
              mov    byte ptr [si+bx+Drivers.Name], al
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

_INIT        ends

            end   Init


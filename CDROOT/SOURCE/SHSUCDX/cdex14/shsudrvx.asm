;************************************************************************
;
;  SHSUDRVX.ASM  May 1996 Version 1.1
;
;    An installable dummy CD driver for testing purposes.  Emulates
;       2 CD drives with ISO9660 format CD roms.
;
;    Updated to handle multi-block reads.
;
;    UsageMsg:
;      SHSUDRVX [/?][/D:DriverName][/U]
;      default driver name is CD001
;
;  Assemble using MASM 6.0 and link as an .exe file.
;
;  SHSUDRVX is a copyright-reserved, free use program.  Use at your own risk.
;  (c)John H. McCoy, 1993, 1995, Sam Houston St. Univ., TX 77341-2206
;************************************************************************

.model small, os_dos

option nokeyword:<length name>
option expr16

fptr  typedef  far ptr word
nptr  typedef  near ptr word

; DOS FileFlags -- not CDrom dir entry flags
_A_NORMAL       equ   00h
_A_RDONLY       equ   01h       ; all cdrom files
_A_HIDDEN       equ   02h
_A_SYSTEM       equ   04h
_A_VOLID        equ   08h
_A_SUBDIR       equ   10h
_A_ARCH         equ   20h

; CD flags
_CD_NORMAL      equ   00h
_CD_HIDDEN      equ   01h
_CD_SUBDIR      equ   02h

rh    struc
   Length      byte ?          ; header size in bytes
   SubUnit     byte ?          ; cd drive number
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

IOCtl_RDHACmd          equ  0
IOCtl_ReadDriveBytes   equ  5
IOCtl_DevStatCmd       equ  6
IOCtl_ReturnSectorSize equ  7
IOCtl_ReturnVolumeSize equ  8        ;   total sectors on disk
IOCtl_MediaChangedCmd  equ  9

IoCB_RDHA     struc
   IoctlCommand           byte IOCtl_RDHACmd
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


DeviceError            equ  8000h
DeviceDone             equ  0100h
DE_UnknownUnit         equ    01h   ; OR with DeviceError
DE_DeviceNotReady      equ    02h
DE_UnknownCommand      equ    03h
DE_SectorNoFound       equ    08h
DE_ReadError           equ    0Bh
DE_GeneralFailure      equ    0Ch

AsciiNul                equ     0
cr                      equ     13
lf                      equ     10
ht                      equ     09
QMark                   equ     '?'

Display   MACRO  msg
         mov     dx,offset msg
         mov     ah,9
         int     21h
ENDM
ToHex             proto near C Num:word

.code

   assume cs:@code,  ds:@code

;  dos device header with CDROM extension fields

DevHeader  label word
  NextDriver     dword     -1
  Attributes     word      0C800h
                 nptr      Strategy
                 nptr      Interrupt
  DeviceName     byte      'CD001   '
                 word      0                     ; CDROM reserved
                 byte      0                     ; 1st CDROM drive letter
  SubUnits       byte      2                     ; number of drives

;  strategy call saves req hdr addr here for use by interrupt call

rhAddr        label far ptr
  rhOffset      word       ?
  rhSegment     word       ?


DriverName       byte       'CD001   '
                 byte       "$"

Author           byte       "John H. McCoy"

DeviceUnit       byte       ?
DrvDoorOpen      equ        01h             ; bit is zero if closed
DrvDoorUnlocked  equ        02h             ; bit is zero if locked
DriveStatus      dword      0000h           ; drive 0
                 dword      0000h           ; drive 1
dta              label      fptr
dta_off          word       ?
dta_seg          word       ?
Sector0          byte       2048 dup (0)

;--- ISO9660 Drive 0 --------------------

iso0PVD            label byte                            ; block 10h
CDType            byte     1                             ; 1
ID                byte     "CD001"                       ; 2-6
Version           byte     1                             ; 7
                  byte     0                             ; 8
SysID             byte     32 dup (0)                    ; 9-40   a-chars
VolID             byte     "SHSU_ISO00", 22 dup(0)       ; 41-72  d-chars
                  byte     8 dup (0)                     ; 73-80
VolSizeLSB        dword    15h                           ; 81-84
VolSizeMSB        dword    ?                             ; 85-88
                  byte     32 dup (0)                    ; 89-120
SetSizeLSB        word     1
SetSizeMSB        word     ?
SetSeqLSB         word     0
SetSeqMSB         word     ?
BlkSizeLSB        word     2048
BlkSizeMSB        word     ?
PathTabSizeLSB    dword    iso013Len-iso0PathTable
PathTabSizeMSB    dword    ?
PathTabLocLSB     dword    13h                           ; 141-144
PathTabOptLocLSB  dword    ?                             ; 145-148
PathTabLocMSB     dword    ?                             ; 149-152
PathTabOptLocMSB  dword    ?                             ; 152-156
RootDirEnt        label    byte                          ; 157-190
   RDELen_Dr        byte      34
   RDEEAttrRecLen   byte      0
   RDEExtLocLSB     dword     12h
   RDEExtLocMSB     dword     0
   RDEDataLenLSB    dword     iso012Len-iso0RootDir
   RDEDataLenMSB    dword     0
   RDEDate          byte      93,7,10,0,0,0,0
   RDEFlags         byte      _CD_SUBDIR
   RDEFileUnitSize  byte      0
   RDEInterLeave    byte      0
   RDEVolSeqNoLSB   word      0
   RDEVolSeqNoMSB   word      0
   RDEFIDLen        byte      1
   RDEFileID        byte      0
VolSetID          byte     128 dup (0)                   ; 191-318 d-chars
PubID             byte     "SAM HOUSTON STATE UNIVERSITY",100 dup (' ') ;a-chars
PrepID            byte     128 dup (' ')                 ; a-chars
AppID             byte     128 dup (' ')                 ; a-chars
CopyRightFileID   byte     37 dup (' ')                  ; d-chars
AbstractFileID    byte     "Readme.Txt",27 dup (0)       ; d-chars
BibloFileID       byte     "BiblioLabel",26 dup (0)      ; d-chars
CreateDate        byte     "1993071100160000",0          ; 814-830
ModDate           byte     16 dup ('0'),0                ; 831-847
ExpDate           byte     16 dup ('0'),0
EffDate           byte     16 dup ('0'),0
StdVer            byte     1                             ; 882
                  byte     0                             ; 883 (373h)
                  byte     (2048-($-iso0PVD)) dup (0)
iso010Len          word     $-iso0PVD           ; details only assemble to 373h

iso0VDSTerm        label    byte                         ; block 011h
                  byte     255                           ; type
                  byte     "CD001"                       ; id
                  byte     1                             ; version
                  byte     0
iso011Len          word     $-iso0VDSTerm

iso0RootDir        label    byte                         ; block 012h
RDDot             label    byte
  RDDotRecLen         byte        RDDotEnd-RDDot
  RDDotEAttribLen     byte        0
  RDDotExtLocLSB      dword       12h
  RDDotExtLocMSB      dword       0
  RDDotDataLenLSB     dword       iso012Len-iso0RootDir
  RDDotDataLenMSB     dword       0
  RDDotDate           byte        93,7,10,0,0,0,0
  RDDotFlags          byte        _CD_SUBDIR
  RDDotFileUnitSize   byte        0
  RDDotInterLeave     byte        0
  RDDotVolSeqNoLSB    word        0
  RDDotVolSeqNoMSB    word        0
  RDDotFIDLen         byte        1
  RDDotFileID         byte        0
  RDDotEnd            label byte
RDDDot            label    byte
  RDDDotRecLen        byte        RDDDotEnd-RDDDot
  RDDDotEAttribLen    byte        0
  RDDDotExtLocLSB     dword       12h
  RDDDotExtLocMSB     dword       0
  RDDDotDataLenLSB    dword       iso012Len-iso0RootDir
  RDDDotDataLenMSB    dword       0
  RDDDotDate          byte        93,7,10,0,0,0,0
  RDDDotFlags         byte        _CD_SUBDIR
  RDDDotFileUnitSize  byte        0
  RDDDotInterLeave    byte        0
  RDDDotVolSeqNoLSB   word        0
  RDDDotVolSeqNoMSB   word        0
  RDDDotFIDLen        byte        1
  RDDDotFileID        byte        1
  RDDDotEnd           label byte
RDF1              label    byte
  RDF1RecLen           byte        RDF1End-RDF1
  RDF1EAttribLen       byte        0
  RDF1ExtLocLSB        dword       14h
  RDF1ExtLocMSB        dword       0
  RDF1DataLenLSB       dword       iso014Len-iso0F1
  RDF1DataLenMSB       dword       0
  RDF1Date             byte        93,7,11,0,0,0,0
  RDF1Flags            byte        _CD_NORMAL
  RDF1FileUnitSize     byte        0
  RDF1InterLeave       byte        0
  RDF1VolSeqNoLSB      word        0
  RDF1VolSeqNoMSB      word        0
  RDF1FIDLen           byte        12
  RDF1FileID           byte        "README.TXT"
                       byte        0                        ; pad to even
  RDF1End              byte        0
iso012Len          word     $-iso0RootDir

iso0PathTable      label    byte                            ; block 013h
  RootDirPath     label    byte
    RDPLen             byte         1         ; len of DirID
    RDPEAttrib         byte         0
    RDPExtLoc          dword        12h
    RDPParent          word         1
    RDPDirID           byte         0
                       byte         0
iso013Len          word     $-iso0PathTable

iso0F1             label    byte                           ; block 014h
   byte  "SHSUDRVX emulates a CD driver/controller with 2 CD drives",cr,lf
   byte  "loaded with ISO ROMS.  Drive 0 has a single text file in",cr,lf
   byte  "the root directory.  This file is also listed in the Abstract",cr,lf
   byte  "field of the primary volume descriptor(PVD).",cr,lf,lf
   byte  "In the root directory of Drive 1 there is a copyright file",cr,lf
   byte  "and a sub-directory.  The copyright file is a text file and is",cr,lf
   byte  "listed in the PVD copyright field.  The HELLO.EXE in the HELLO",cr,lf
   byte  "sub-directory is executable.",cr,lf,lf
   byte  "IOCTL input functions supported:",cr,lf
   byte  "   Return address of Device Header",cr,lf
   byte  "   Read drive bytes",cr,lf
   byte  "   Device status",cr,lf
   byte  "   Return sector size",cr,lf
   byte  "   Return volume size",cr,lf
   byte  "   Media changed",cr,lf
   byte  "      reports -- not changed if drive door is closed and locked",cr,lf
   byte  "              -- don't know if door closed and unlocked",cr,lf
   byte  "              -- changed if door is open",cr,lf
   byte  "IOCTL output functions supported:",cr,lf
   byte  "   Eject disk  -- don't unless you can close it!!!",cr,lf
   byte  "   Lock/Unlock door",cr,lf
   byte  "   Reset drive",cr,lf
   byte  "   Close door",cr,lf,lf
   byte  "SHSUDRVX is a copyright reserved, free use program.",cr,lf
   byte  "(c) John H. McCoy  "
   byte  "August 1993,  Sam Houston State University, TX 77341-2206.",cr,lf

iso014Len          word     $-iso0F1

;--- ISO9660 Drive 1 --------------------

iso1PVD            label byte                             ; lbn 10h
CDType1            byte     1                             ; 1
ID1                byte     "CD001"                       ; 2-6
Version1           byte     1                             ; 7
                   byte     0                             ; 8
SysID1             byte     32 dup (0)                    ; 9-40   a-chars
VolID1             byte     "SHSU_ISO01", 22 dup(0)       ; 41-72  d-chars
                   byte     8 dup (0)                     ; 73-80
VolSizeLSB1        dword    18h                           ; 81-84
VolSizeMSB1        dword    ?                             ; 85-88
                   byte     32 dup (0)                    ; 89-120
SetSizeLSB1        word     1
SetSizeMSB1        word     ?
SetSeqLSB1         word     1
SetSeqMSB1         word     ?
BlkSizeLSB1        word     2048
BlkSizeMSB1        word     ?
PathTabSizeLSB1    dword    iso113Len-iso1PathTable
PathTabSizeMSB1    dword    ?
PathTabLocLSB1     dword    13h                           ; 141-144
PathTabOptLocLSB1  dword    ?                             ; 145-148
PathTabLocMSB1     dword    ?                             ; 149-152
PathTabOptLocMSB1  dword    ?                             ; 152-156
RootDirEnt1        label    byte                          ; 157-190
   RDELen_Dr1        byte      34
   RDEEAttrRecLen1   byte      0
   RDEExtLocLSB1     dword     12h
   RDEExtLocMSB1     dword     0
   RDEDataLenLSB1    dword     iso112Len-iso1RootDir
   RDEDataLenMSB1    dword     0
   RDEDate1          byte      93,7,10,0,0,0,0
   RDEFlags1         byte      _CD_SUBDIR OR _CD_HIDDEN
   RDEFileUnitSize1  byte      0
   RDEInterLeave1    byte      0
   RDEVolSeqNoLSB1   word      1
   RDEVolSeqNoMSB1   word      0
   RDEFIDLen1        byte      1
   RDEFileID1        byte      0
VolSetID1          byte     128 dup (0)                   ; 191-318 d-chars
PubID1             byte     "SAM HOUSTON STATE UNIVERSITY",100 dup (' ') ;a-chars
PrepID1            byte     128 dup (' ')                 ; a-chars
AppID1             byte     128 dup (' ')                 ; a-chars
CopyRightFileID1   byte     "COPYRITE.TXT",25 dup (0)     ; d-chars
AbstractFileID1    byte     37 dup (' ')                  ; d-chars
BibloFileID1       byte     37 dup (' ')                  ; d-chars
CreateDate1        byte     "1993072900160000",0          ; 814-830
ModDate1           byte     16 dup ('0'),0                ; 831-847
ExpDate1           byte     16 dup ('0'),0
EffDate1           byte     16 dup ('0'),0
StdVer1            byte     1                             ; 882
                   byte     0
                   byte     (2048-($-iso1PVD)) dup (0)
iso110Len          word     $-iso1PVD           ; details only assemble to 373h

iso1VDSTerm        label    byte                               ; lbn 11h
                  byte     255                           ; type
                  byte     "CD001"                       ; id
                  byte     1                             ; version
                  byte     0
iso111Len          word     $-iso1VDSTerm

iso1RootDir        label    byte                               ; lbn 12h
RDDot1             label    byte
  RDDotRecLen1         byte        RDDotEnd1-RDDot1
  RDDotEAttribLen1     byte        0
  RDDotExtLocLSB1      dword       12h
  RDDotExtLocMSB1      dword       0
  RDDotDataLenLSB1     dword       iso112Len-iso1RootDir
  RDDotDataLenMSB1     dword       0
  RDDotDate1           byte        93,7,10,0,0,0,0
  RDDotFlags1          byte        _CD_SUBDIR OR _CD_HIDDEN
  RDDotFileUnitSize1   byte        0
  RDDotInterLeave1     byte        0
  RDDotVolSeqNoLSB1    word        1
  RDDotVolSeqNoMSB1    word        0
  RDDotFIDLen1         byte        1
  RDDotFileID1         byte        0
  RDDotEnd1            label byte
RDDDot1            label    byte
  RDDDotRecLen1        byte        RDDDotEnd1-RDDDot1
  RDDDotEAttribLen1    byte        0
  RDDDotExtLocLSB1     dword       12h
  RDDDotExtLocMSB1     dword       0
  RDDDotDataLenLSB1    dword       iso112Len-iso1RootDir
  RDDDotDataLenMSB1    dword       0
  RDDDotDate1          byte        93,7,29,0,0,0,0
  RDDDotFlags1         byte        _CD_SUBDIR OR _CD_HIDDEN
  RDDDotFileUnitSize1  byte        0
  RDDDotInterLeave1    byte        0
  RDDDotVolSeqNoLSB1   word        1
  RDDDotVolSeqNoMSB1   word        0
  RDDDotFIDLen1        byte        1
  RDDDotFileID1        byte        1
  RDDDotEnd1           label byte
RDF11          label    byte
  RDF1RecLen1           byte        RDF11End-RDF11
  RDF1EAttribLen1       byte        0
  RDF1ExtLocLSB1        dword       14h
  RDF1ExtLocMSB1        dword       0
  RDF1DataLenLSB1       dword       iso114Len-iso1F1
  RDF1DataLenMSB1       dword       0
  RDF1Date1             byte        93,7,29,0,0,0,0
  RDF1Flags1            byte        _CD_NORMAL
  RDF1FileUnitSize1     byte        0
  RDF1InterLeave1       byte        0
  RDF1VolSeqNoLSB1      word        1
  RDF1VolSeqNoMSB1      word        0
  RDF1FIDLen1           byte        12
 ;RDF1FileID1           byte        "COPYRITE.TXT"
  RDF1FileID1           byte        "copyrite.txt"           ;test for lc
                        byte        0                        ; pad to even
  RDF11End     label    byte
RDSub10          label    byte
  RDS1RecLen1           byte        RDS10End-RDSub10
  RDS1EAttribLen1       byte        0
  RDS1ExtLocLSB1        dword       15h
  RDS1ExtLocMSB1        dword       0
  RDS1DataLenLSB1       dword       iso115Len-iso1SubDir1
  RDS1DataLenMSB1       dword       0
  RDS1Date1             byte        93,8,07,0,0,0,0
  RDS1Flags1            byte        _CD_SUBDIR
  RDS1FileUnitSize1     byte        0
  RDS1InterLeave1       byte        0
  RDS1VolSeqNoLSB1      word        1
  RDS1VolSeqNoMSB1      word        0
  RDS1FIDLen1           byte        5
  RDS1FileID1           byte        "HELLO"
  RDS10End     label    byte
                        byte        0
iso112Len          word     $-iso1RootDir

iso1PathTable      label    byte                              ; lbn 13h
  RootDirPath1     label    byte
    RDPLen1             byte         1
    RDPEAttrib1         byte         0
    RDPExtLoc1          dword        12h
    RDPParent1          word         1
    RDPDirID1           byte         0
                        byte         0
    SDPLen1             byte         5       ; len of dirid field
    SDPEAttrib1         byte         0
    SDPExtLoc1          dword        15h
    SDPParent1          word         1
    SDPDirID1           byte         "HELLO"
                        byte         0
iso113Len          word     $-iso1PathTable

iso1F1             label    byte                              ; lbn 14h
   byte "Copyright file for device 1 of John's CD Drive Emulator.",cr,lf,lf
   byte  "SHSUDRVX is a copyright reserved, free use program.",cr,lf
   byte  "(c) John H. McCoy  "
   byte  "August 1993,  Sam Houston State University, TX 77341-2206.",cr,lf
iso114Len          word     $-iso1F1

iso1SubDir1        label    byte                               ; lbn 15h
SDDot1             label    byte
  SDDotRecLen1         byte        SDDotEnd1-SDDot1
  SDDotEAttribLen1     byte        0
  SDDotExtLocLSB1      dword       15h
  SDDotExtLocMSB1      dword       0
  SDDotDataLenLSB1     dword       iso115Len-iso1SubDir1
  SDDotDataLenMSB1     dword       0
  SDDotDate1           byte        93,8,8,0,0,0,0
  SDDotFlags1          byte        _CD_SUBDIR
  SDDotFileUnitSize1   byte        0
  SDDotInterLeave1     byte        0
  SDDotVolSeqNoLSB1    word        1
  SDDotVolSeqNoMSB1    word        0
  SDDotFIDLen1         byte        1
  SDDotFileID1         byte        0
  SDDotEnd1            label byte
SDDDot1            label    byte
  SDDDotRecLen1        byte        SDDDotEnd1-SDDDot1
  SDDDotEAttribLen1    byte        0
  SDDDotExtLocLSB1     dword       15h
  SDDDotExtLocMSB1     dword       0
  SDDDotDataLenLSB1    dword       iso112Len-iso1RootDir
  SDDDotDataLenMSB1    dword       0
  SDDDotDate1          byte        93,7,29,0,0,0,0
  SDDDotFlags1         byte        _CD_SUBDIR
  SDDDotFileUnitSize1  byte        0
  SDDDotInterLeave1    byte        0
  SDDDotVolSeqNoLSB1   word        1
  SDDDotVolSeqNoMSB1   word        0
  SDDDotFIDLen1        byte        1
  SDDDotFileID1        byte        1
  SDDDotEnd1           label byte
SDF12          label    byte
  SDF2RecLen1           byte        SDF12End-SDF12
  SDF2EAttribLen1       byte        0
  SDF2ExtLocLSB1        dword       16h
  SDF2ExtLocMSB1        dword       0
  SDF2DataLenLSB1       dword       iso116Len-iso1F2
  SDF2DataLenMSB1       dword       0
  SDF2Date1             byte        93,8,8,0,0,0,0
  SDF2Flags1            byte        _CD_NORMAL
  SDF2FileUnitSize1     byte        0
  SDF2InterLeave1       byte        0
  SDF2VolSeqNoLSB1      word        1
  SDF2VolSeqNoMSB1      word        0
  SDF2FIDLen1           byte        9
  SDF2FileID1           byte        "HELLO.ASM"
  SDF12End     label    byte
SDF13          label    byte
  SDF3RecLen1           byte        SDF13End-SDF13
  SDF3EAttribLen1       byte        0
  SDF3ExtLocLSB1        dword       17h
  SDF3ExtLocMSB1        dword       0
  SDF3DataLenLSB1       dword       iso117Len-iso1F3
  SDF3DataLenMSB1       dword       0
  SDF3Date1             byte        93,8,8,0,0,0,0
  SDF3Flags1            byte        _CD_NORMAL
  SDF3FileUnitSize1     byte        0
  SDF3InterLeave1       byte        0
  SDF3VolSeqNoLSB1      word        1
  SDF3VolSeqNoMSB1      word        0
  SDF3FIDLen1           byte        9
  SDF3FileID1           byte        "HELLO.EXE"
  SDF13End     label    byte
                        byte        0
iso115Len          word     $-iso1SubDir1

iso1F2            label    byte                                ; lbn 16h

 byte ";",cr,lf
 byte ";        hello.asm",cr,lf
 byte ";",cr,lf
 byte cr,lf
 byte ".model small, os_dos",cr,lf
 byte cr,lf
 byte "option nokeyword:<length name>",cr,lf
 byte "option expr16",cr,lf
 byte cr,lf
 byte "cr                      equ     13",cr,lf
 byte "lf                      equ     10",cr,lf
 byte cr,lf
 byte "Display   MACRO  msg",cr,lf
 byte "         mov     dx,offset msg",cr,lf
 byte "         mov     ah,9",cr,lf
 byte "         int     21h",cr,lf
 byte "ENDM",cr,lf
 byte cr,lf
 byte ".data",cr,lf
 byte cr,lf
 byte "HelloMsg byte cr,lf",cr,lf
 byte 'byte "Greetings from Sam Houston State University in '
 byte 'Huntsville, TX.",cr,lf,lf',cr,lf
 byte 'byte "Sam Houston State has an enrollment of some 12,000'
 byte ' students.  It",cr,lf',cr,lf
 byte 'byte "is located 68 miles North of Houston in the scenic pine'
 byte ' forests of",cr,lf',cr,lf
 byte 'byte "East Texas.",cr, lf,lf,''$''',cr,lf
 byte cr,lf
 byte ".code",cr,lf
 byte cr,lf
 byte "   .startup",cr,lf
 byte "   Display HelloMsg",cr,lf
 byte "   .exit",cr,lf
 byte "END",cr,lf
iso116Len          word     $-iso1F2
; ------------------------------------------------------------------------
;  Warning!!! Don't mess with the following code.
;  It is the machine code for HELLO.EXE.
; ------------------------------------------------------------------------
iso1F3            label    byte                                ; lbn 17h
 byte "MZú       ÿÿ    D                          "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                                                 "
 byte "                      º ŽÚŒÓ+ÚÑãÑãÑãÑãúŽÒãûº ´"
 byte ht,"Í!´LÍ!",cr,lf
 byte "Greetings from Sam Houston State University in Huntsville, TX.",cr,lf,lf
 byte "Sam Houston State has an enrollment of some 12,000 students.  It",cr,lf
 byte "is located 68 miles North of Houston in the scenic pine forests of",cr,lf
 byte "East Texas.",cr,lf,lf,'$'
iso117Len          word     $-iso1F3


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
      les      bx, rhAddr                ; make sure we have rh addr
      mov      al, es:[bx].rh.SubUnit
      .if   al >= SubUnits
            mov   ax,(DeviceDone OR DeviceError OR DE_UnknownUnit)
            jmp    ExitWithStatus
      .endif
      mov      DeviceUnit, al
      mov      al, es:[bx].rh.command
      xor      ah, ah
      .if      (al ==128)
          jmp  ReadLong
      .elseif  (al == 3)
          jmp IoctlInput
      .elseif  (al == 12)
          jmp IoctlOutput
      .elseif  (al == 13)
          ;Open  Just drop on through.
      .elseif  (al == 14 )
          ;Close Just drop on through.
      .elseif  (al ==131 )
          ;seek  Just drop on through
      .else
          jmp   UnknownCommand
      .endif

       mov   ax, (DeviceDone)
       jmp   ExitWithStatus


IoctlInput:
      les      bx, es:[bx].rhIOCTL.CBPtr
      mov      al, byte ptr es:[bx]           ; 1st byte of dta is subcommand
      .if      al == IOCtl_RDHACmd
         lea   ax, DevHeader
         mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress, ax
         mov   ax, ds
         mov   word ptr es:[bx].IoCB_RDHA.DeviceHeaderAddress+2, ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif   al == IOCtl_ReadDriveBytes
         mov   ax, 0
         mov   word ptr es:[bx+1], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif   al == IOCtl_DevStatCmd
         mov   al, DeviceUnit
         .if   al == 1
            mov    al, 4
         .endif
         cbw
         mov   si, ax
         mov   ax, word ptr [si+DriveStatus]
         mov   word ptr es:[bx+1], ax
         mov   ax, 0
         mov   word ptr es:[bx+3], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif   al == IOCtl_ReturnSectorSize
         mov   ax, 0
         mov   word ptr es:[bx+1], ax
         mov   ax, 2048
         mov   word ptr es:[bx+2], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif   al == IOCtl_ReturnVolumeSize
         .if  DeviceUnit == 0
               mov   ax, word ptr VolSizeLSB
               mov   word ptr es:[bx+1], ax
               mov   ax, word ptr VolSizeLSB+2
               mov   word ptr es:[bx+3], ax
         .else
               mov   ax, word ptr VolSizeLSB1
               mov   word ptr es:[bx+1], ax
               mov   ax, word ptr VolSizeLSB1+2
               mov   word ptr es:[bx+3], ax
         .endif
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif   al == IOCtl_MediaChangedCmd
         mov   al, DeviceUnit
         .if   al == 1
            mov    al, 4
         .endif
         cbw
         mov   si, ax
         mov   ax, word ptr [si+DriveStatus]
         and   ax, DrvDoorUnlocked
         .if    ax == 0                         ; door is locked
            mov   byte ptr es:[bx+1], 01h       ; report media hasn't changed
         .else
            mov   ax, word ptr [si+DriveStatus]
            and   ax, DrvDoorOpen
            .if   ax == 0
               mov   byte ptr es:[bx+1], 0h    ; closed, report don't know
            .else
               mov   byte ptr es:[bx+1], 0ffh    ; open, report media change
            .endif
         .endif
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .endif
      jmp      UnknownCommand

IoctlOutput:
      les      bx, es:[bx].rhIOCTL.CBPtr
      mov      al, byte ptr es:[bx]             ; 1st byte of dta is subcommand
      .if      al == 0                          ; eject disk
         mov   al, DeviceUnit
         .if   al == 1
            mov  al, 4
         .endif
         cbw
         mov   si, ax
         mov   ax, DrvDoorUnlocked OR DrvDoorOpen
         mov   word ptr [si+DriveStatus], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif  al == 1              ; lock/unlock door
         mov   al, DeviceUnit
         .if   al == 1
            mov   al, 4
         .endif
         cbw
         mov   si, ax
         cmp   byte ptr es:[bx+1], 0
         jne    @F
               mov   ax, word ptr [si+DriveStatus]
               or    ax, DrvDoorUnlocked        ; unlock door
               mov   word ptr [si+DriveStatus], ax
               mov   ax, DeviceDone
               jmp   ExitRestoreRH
     @@:
         mov   word ptr [si+DriveStatus], 0     ; closed and locked
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif al == 2              ; reset drive
         mov   al, DeviceUnit
         .if   al == 1
            mov    al, 4
         .endif
         cbw
         mov   si, ax
         xor    ax, ax
         mov   word ptr [si+DriveStatus], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .elseif al == 5              ; close door
         mov   al, DeviceUnit
         .if   al == 1
            mov    al, 4
         .endif
         cbw
         mov   si, ax
         mov   ax, word ptr [si+DriveStatus]
         or    ax, DrvDoorUnlocked
         and   ax, NOT(DrvDoorOpen)
         mov   word ptr [si+DriveStatus], ax
         mov   ax, DeviceDone
         jmp   ExitRestoreRH
      .endif
      jmp      UnknownCommand


ReadLong:

    mov   al, DeviceUnit
    .if   al == 1
       mov al, 4
    .endif
    cbw
    mov   si, ax
    mov   ax, word ptr [si+DriveStatus]
    and    ax, DrvDoorOpen

    .if  ax != 0
       mov   ax,(DeviceDone OR DeviceError OR DE_DeviceNotReady)
       jmp   ExitWithStatus
    .endif

    .if word ptr es:[bx+2].rhTransfer.StartSector != 0  ;hi order word
       mov   ax,(DeviceDone OR DeviceError OR DE_SectorNoFound)
       jmp   ExitWithStatus
    .endif

    mov    ax, word ptr es:[bx].rhTransfer.DtaPtr
    mov    dta_off, ax
    mov    ax, word ptr es:[bx+2].rhTransfer.DtaPtr
    mov    dta_seg, ax

    mov   ax, word ptr es:[bx].rhTransfer.StartSector

    .if es:[bx].rh.SubUnit == 0
      .if (ax > word ptr VolSizeLSB)
        mov   ax,(DeviceDone OR DeviceError OR DE_SectorNoFound)
        jmp   ExitWithStatus
      .endif
      mov   ah, byte ptr VolSizeLSB
      sub   ah, al
      .if  ah > byte ptr es:[bx].rhTransfer.SectorCount
          mov  ah, byte ptr es:[bx].rhTransfer.SectorCount
      .endif
      .if al == 0h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 1h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 2h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 3h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 4h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 5h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 6h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 7h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 8h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 9h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Ah
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Bh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Ch
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Dh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Eh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Fh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 010h         ; PVD
        mov   si, offset iso0PVD
        mov   cx, iso010Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 011h     ; VDSTerm
        mov   si, offset iso0VDSTerm
        mov   cx, iso011Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 012h     ; RootDir
        mov   si, offset iso0RootDir
        mov   cx, iso012Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 013h     ; Path
        mov   si, offset iso0PathTable
        mov   cx, iso013Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 014h     ; F1
        mov   si, offset iso0F1
        mov   cx, iso014Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
       mov   ax, (DeviceDone)
       jmp   ExitRestoreRH
   .elseif es:[bx].rh.SubUnit == 1
      .if (ax >= word ptr VolSizeLSB1)
        mov   ax,(DeviceDone OR DeviceError OR DE_SectorNoFound)
        jmp   ExitWithStatus
      .endif
      mov   ah, byte ptr VolSizeLSB1
      sub   ah, al
      .if  ah > byte ptr es:[bx].rhTransfer.SectorCount
          mov  ah, byte ptr es:[bx].rhTransfer.SectorCount
      .endif
      .if al == 0h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 1h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 2h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 3h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 4h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 5h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 6h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 7h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 8h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 9h
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Ah
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Bh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Ch
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Dh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Eh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 0Fh
        mov   si, offset Sector0
        mov   cx, 2048
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 010h         ; PVD
        mov   si, offset iso1PVD
        mov   cx, iso110Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 011h     ; VDSTerm
        mov   si, offset iso1VDSTerm
        mov   cx, iso111Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 012h     ; RootDir
        mov   si, offset iso1RootDir
        mov   cx, iso112Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 013h     ; Path
        mov   si, offset iso1PathTable
        mov   cx, iso113Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 014h     ; F1
        mov   si, offset iso1F1
        mov   cx, iso114Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 015h     ; SubDir1
        mov   si, offset iso1SubDir1
        mov   cx, iso115Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 016h     ; F2
        mov   si, offset iso1F2
        mov   cx, iso116Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
      .if al == 017h     ; F3
        mov   si, offset iso1F3
        mov   cx, iso117Len
        les   di, dta
        cld
        rep   movsb
        .if  ah > 1
           dec  ah
           inc  al
           add  dta_off, 2048
        .endif
      .endif
       mov   ax, (DeviceDone)
       jmp   ExitRestoreRH
   .endif
   mov   ax,(DeviceDone OR DeviceError OR DE_UnknownUnit)
   jmp    ExitWithStatus

UnknownCommand:

      mov      ax, (DeviceDone OR DeviceError OR DE_UnknownCommand)


ExitRestoreRH:

      les      bx, rhAddr                ; restore rh ptr

ExitWithStatus:

      mov   es:[bx].rh.Status, ax

Exit0:
      ret

Interrupt   endp

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


     byte "End of DRVX"

LastByte               label byte
;============================================================================
;  everything below this line is discarded after installing the driver

DriverParm              equ     'D'     ; command line parms /D:, etc
ArgumentNotFound        EQU     2       ; Unrecognized argument
NoArgumentsFound        EQU     1       ; No argument in command line
ArgumentFound           EQU     0       ; Ok argument in command line

UnInstall               equ     2
DontInstall             equ     1
Install                 equ     0

InstallFlag             byte    0

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
          .if InstallFlag == Install
              call Link
              .if (InstallFlag == Install)
                  Display  InstallMsg
                  Display  DriverMsg
                  Display  DriverName
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
      cld
      mov      _bx, bx                 ; save current header addr
      mov      _es, es
      les      bx, es:[bx]             ; load next header addr into es:bx
      mov      ax, es
      cmp      bx, 0FFFFh              ; end of drivers?
      je       DriverNotInstalled
      mov      cx, 8
      lea      di, DeviceName[bx]      ; es:di is chained device name
      mov      si, offset DeviceName   ; ds:si is our device name
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
      pop      ds                      ; cs=ds=@code -- need this for net
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

      display DriverMsg
      display DriverName                ; tell the world we did it
      display UnInstallMsg
      ret

  DriverNotInstalled:
      Display  CouldNotRemoveMsg
      display DriverMsg
      display  DriverName
      ret

UnInstallDriver endp

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
      mov      al, 'U'            ; /U unInstall driver
      call     GetParm
      .if ax == ArgumentFound
          mov      InstallFlag, UnInstall
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

UsageMsg    db cr, lf,"Usage:  SHSUDRVX"
            db " [/?] [/D:DriverName] [/U]",cr,lf,"$"

InstallMsg  db cr,lf,"SHSUDRVX V1.1 CD Driver Installed.",cr,lf
            db "Copyright 1993, 1995, 1996 John H. McCoy.",cr, lf
            db "Sam Houston State University","$"
DriverMsg   db cr,lf,"Driver Name:  ","$"
UnInstallMsg       db " Uninstalled and memory freed",cr,lf,"$"
CouldNotRemoveMsg  db "Could not remove SHSUDRVX with ","$"

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

.stack
            end  Init

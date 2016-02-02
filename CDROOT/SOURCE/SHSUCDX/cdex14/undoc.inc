;begin undoc.inc
;
; revised March 1, 1994

;
;   various useful DOS structures

DOS_LOL        struct
               fptr  ?
     CurSFT    fptr  ?
               fptr  ?
               fptr  ?
               word  ?
               fptr  ?
        CDS    fptr  ?
               fptr  ?
               word  ?
               byte  ?
   LastDrive   byte  ?
DOS_LOL        ends

CDS            struct
   CurrPath    byte  67 dup (?)
   Flags       word  ?
               fptr  ?
   Redir       fptr  ?
               word  ?
   RootOff     word  ?
CDS            ends

SFT         struct
   RefCnt      word   ?           ; Reference count
   Mode        word   ?           ; Open Mode
   DirAttrib   byte   ?
   Flags       word   ?
   DCB         dword  ?           ; Device control block
   Cluster     word   ?           ; Initial cluster
   HHMMSS      word   ?           ; Hour, Min, Sec/2
   YYMMDD      word   ?           ; Year, Month, Day
   FilSiz      dword  ?           ; file size/EOF location
   FilPos      dword  ?           ; Current file position
   RelClstr    word   ?           ; clusters from beginning
   CurClstr    word   ?           ; current cluster
   LBN         word   ?           ; block number
   DirIndex    byte   ?           ; directory index
   Name        byte 11 dup (?)    ; file name
   Unknown     byte  4 dup (?)
   OwnerMach   word   ?           ; machine number of owner
   OwnerPSP    word   ?           ; psp of owner task
   Status      word   ?
SFT         ends

;; These are really documented.  They are here for convenience.

frame       struc
   fr_OldBP        nptr   ?
   fr_RetAddr      fptr   ?
   fr_Flags        word   ?
   fr_Parm1        word   ?
frame       ends

DirEnt      struct
   FName           byte    11 DUP (?)
   Fattr           byte      ?
   FTime           word      ?
   FDate           word      ?
   BlkNo           dword     ?
   FSize           dword     ?
   ParentBlk       dword     ?
   Forw            nptr      ?
   Back            nptr      ?
DirEnt      ends

DrvEnt      struct
   No             byte     ?
   Letter         byte     ?
   Unit           byte     ?
   FlagsOff       byte     ?
   Type           word     ?
   LastAccess     word     ?
   DevHdrp        fptr     ?
   DriverName     byte     8 dup (?)
   Strategyp      fptr     ?
   Interruptp     fptr     ?
   VolSize        dword    ?
   BlkSize        word     ?
   RootEnt        DirEnt      <>
   BufBlkNo       dword    ?
   Bufp           nptr     ?
   VLabel         byte    12 dup (?)
DrvEnt      ends

rh    struc
   Length            byte  ?              ; header size in bytes
   SubUnit           byte  ?              ; MSCDEX fills in cd drive unit
   Command           byte  ?              ; device command code
   Status            word  ?              ; device command status
   Reserved          byte  8 dup(?)
rh    ends

rhIOCTL   struc
                     byte size rh dup(?) ;rh common
   MediaDesc         byte  ?
   CBPtr             fptr  ?
   BytesToTransfer   word  ?              ; Control Block length
   StartSector       word  ?
   VolIdPtr          fptr  ?
rhIOCTL   ends

rhReadLong   struc
                     byte size rh dup(?) ;rh common
   AddrMode          byte  ?
   Bufp              fptr  ?
   Count             word  ?
   StartBlk          dword ?
   ReadMode          byte  ?
   ISize             byte  ?
   ISkip             byte  ?
rhReadLong   ends

rhcmdIOCTL_In      equ 3
rhcmdIOCTL_Out     equ 12
rhcmdReadLong      equ 128

; end undoc.inc
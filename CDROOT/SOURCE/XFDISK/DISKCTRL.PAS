{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    DISKCTRL.PAS
    Low Level Fixed Disk Access

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to
  Free Software Foundation, Inc.
        59 Temple Place - Suite 330
        Boston, MA  02111-1307, USA
    or visit the GNU Homepage at http://www.gnu.org/.
}

unit DiskCtrl;

interface

const
  TimeoutInactivate = 0;
  TimeoutReset      = 1;
  TimeoutIgnore     = 2;

  ErrorMemoryLow          = $e0;
  ErrorDriveFormatFailed  = $f0;
  ErrorDriveFormatUnknown = $f1;
  ErrorDriveBadLBA        = $fe; {added, UM}
  ErrorDriveDeInit        = $ff;

type
  {Initialisation}
  TInit = (INITNONE, INITREAD, INITWRITE, INITREADWRITE);
  {Positions Record fÅr BIOS-Funktionen}
  TPos = record
    Head     :byte;                    {Schreib-/Lesekopf}
    SecCyl   :word;                    {Sektor/Cylinder im BIOS-Format}
  end;
  {Laufwerksgeometrie, added by UM}
  PDriveData = ^TDriveData;
  TDriveData = record
    Drive        :integer; { BIOS number}
    Int13X       :boolean; { BIOS supports extended Functions for this drive }
    Cylinders    :longint; { max. cylinder number of old BIOS function }
    Heads        :longint;
    Sectors      :longint;
    TotalSectors :longint;
    Next         :PDriveData;
  end;
  {Bios Parameter Block}
  PBPB = ^TBPB;
  TBPB = record
    SectSize :word;                    {Sektorgrî·e in Byte}
    ClusSize :byte;                    {Clustergrî·e in Sektoren}
    ResvSect :word;                    {Reservierte Sektoren f. BootStrap}
    FATCnt   :byte;                    {Anzahl der FATs}
    RootEntr :word;                    {Grî·e des Wurzelverzeichnisses}
    VoluSize :word;                    {Volumegrî·e in Sektoren}
    Media    :byte;                    {MediaDescriptor}
    FATSize  :word;                    {Grî·e der FAT}
  end;
  {Eintrag in die Partitionstabelle}
  PPartEntr = ^TPartEntr;
  TPartEntr = record
    PartStat :byte;                    {Status: $80 Aktiv, $00 inaktiv}
    StartPos :TPos;                    {Startposition der Partition}
    PartType :byte;                    {Partitionstyp ($05=Erweitert)}
    EndPos   :TPos;                    {Endposition}
    Distance :longint;                 {Entfernung zur Partitionstabelle}
    PartSize :longint;                 {Grî·e der Partition in Sektoren}
  end;
  {Eintrag in das BootManager Menu}
  PInfoEntr = ^TInfoEntr;
  TInfoEntr = record
    BootDriv :byte;                    {Festplatte}
    BootPos  :TPos;                    {Position des Bootsektors}
    PartLDrv :byte;{LW-Buchstabe;primÑr:0}     {Logisches Laufwerk}
    PartType :byte;                    {Partitionstyp}
    PartEntr :byte;{log:0,pri:Nr1-4}   {Eintag in der Partitionstabelle}
    PartSize :word;                    {Grî·e der Partition in MB}
    Distance :longint;                 {Distanz der Partition}
    PartName :array[0..16] of char;    {Name der Partition (16 Chars + #0)}
    EntrMark :word;                    {Markierung: $0/$AA55 un-/benutzt}
  end;
  {Eintrag in die Passwortliste}
  PPWDEntry = ^TPWDEntry;
  TPWDEntry = record
    Protection  :byte;
    Password    :array[0..16] of char;
  end;
  {Puffer fÅr den Partitionssektor}
  PPartSect =^TPartSect;
  TPartSect = record
    PartCode :array[0..445] of byte;   {Partitions-Code}
    PartEntr :array[0..3] of TPartEntr;{EintrÑge in die Partitionstabelle}
    PartMark :word;                    {Markierung: $AA55 Partitionssektor}
  end;
  {Puffer fÅr den Bootsektor}
  PBootSect = ^TBootSect;
  TBootSect = record
    BootJump :array[0..2] of byte;     {Jump-Befehl in die BootStrap}
    PartName :array[0..7] of byte;     {Partitionsname}
    PartBlck :TBPB;                    {BIOS Parameter Block}
    CylnSize :word;                    {Cylindergrî·e in Sektoren}
    Heads    :word;                    {Anzahl der Schreib-/Lesekîpfe}
    Distance :longint;                 {Entfernung vom Partitionssektor}
    Unused   :array[0..10] of byte;
    PartLabl :array[0..10] of byte;    {Label}
    PartSyst :array[0..7] of byte;     {Systemkennung}
    BootCode :array[0..447] of byte;   {BootStrap}
    PartMark :word;
  end;
  {Puffer fÅr den BootManager Menusektor}
  PInfoSect = ^TInfoSect;
  TInfoSect = record
    InfoEntr    :array[0..7] of TInfoEntr;        {EintrÑge in die Menutabelle}
    MasterPWD   :TPWDEntry;              {Master Pa·wort}
    FloppyPWD   :TPWDEntry;              {Floppy Pa·wort}
    PWDEntr     :array[0..7] of TPWDEntry;        {Partitionspa·wîrter}
    Unused      :array[0..51] of byte;
    FloppyBoot  :byte;                   {Bootmanager: immer von Floppy booten}
    AutoHide    :byte;                   {verstecken von prim. Partitionen}
    SimpleMenus :byte;                   {Einfache Menus}
    BootLast    :byte;                   {Zuletzt gewÑhlten Eintrag booten}
    LastEntry   :byte;                   {Zuletzt gewÑhlter Eintrag}
    SuppBlind   :byte;                   {Support for blind people}
    TimeHndl    :byte;                   {Timeout Handling
                                          0: inaktivieren,
                                          1: zurÅcksetzen,
                                          2: ignorieren}
    ClearScrn   :byte;                   {Bildschirm lîschen}
    VerMark     :word;                   {Versionsnummer Markierung}
    VerInfo     :word;                   {Version}
    MENRndIdx   :longint;                {Random Seed}
    MENChkSum   :longint;                {Checksumme}
    WaitTime    :word;                   {Timeout Zeit}
    InfoMark    :word;                   {Markierung}
  end;
  {Sektorpuffer}
  PBuffer = ^TBuffer;
  TBuffer = array[0..511] of byte;
  {Speicherzugriff; added UM}
  PLBuffer = ^TLBuffer;
  TLBuffer = array[0..65527] of byte;
  {Puffer fÅr den Partitionszylinder (BootManager)}
  PPartCyln = ^TPartCyln;
  TPartCyln = record
    PartSect :TPartSect;               {Partitionssektor}
    CodeSect :array[0..13] of TBuffer; {14 Codesektoren}
    InfoSect :TInfoSect;               {Menusektor}
    OldMBR   :TPartSect;               {Kopie des Originalen MBR}
  end;

procedure CHS2HeadSecZyl (Head, Cylinder, Sector: integer; var VPos: TPos);
procedure HeadSecZyl2CHS (var Head, Cylinder, Sector: integer; VPos: TPos);
procedure CHS2LBA (Drive: integer; Head, Cylinder, Sector: longint; var LBA: longint);
procedure LBA2CHS (Drive: integer; var Head, Cylinder, Sector: longint; LBA: longint);
procedure LBA2HeadSecZyl (Drive: integer; var Pos: TPos; LBA: longint; PQMagicComp: boolean);
procedure HeadSecZyl2LBA (Drive: integer; Pos: TPos; var LBA: longint);
{all New, UM}

function GetNumberOfDrives :integer;
function GetDriveStatus (Drive: integer) :integer;
function InitReadWriteSectors (Drive: integer; RWInit: TInit) :integer;
function DeInitReadWriteSectors (Drive: integer) :integer;

function ReadSectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;
function WriteSectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;
function VerifySectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;

function GetDriveFormat (Drive: integer; var DG: PDriveData) :boolean;
function getfirstdrivesectors: byte;

implementation

uses
  DOS, Misc;

type
  TEmul = (EMULNONE, EMULWIN32, EMULOS2, EMULLINUX, EMULMAC);

const
  MaxErrors = 3;

var
  Init :TInit;
  Emul :TEmul;

const
  DriveFormat : PDriveData = nil;

{ Umrechung CHS <-> BIOS HeadSecZyl }
procedure CHS2HeadSecZyl (Head, Cylinder, Sector: integer; var VPos: TPos);
begin
  VPos.Head := Head;
  VPos.SecCyl := (lo (Cylinder) shl 8) + ((hi (Cylinder) and 3) shl 6) + (lo (Sector) and 63)
end;

procedure HeadSecZyl2CHS (var Head, Cylinder, Sector: integer; VPos: TPos);
begin
  Head := VPos.Head;
  Sector := lo (VPos.SecCyl) and 63;
  Cylinder := (hi (VPos.SecCyl)) + ((lo (VPos.SecCyl) and 192) shl 2)
end;

{ Umrechnung CHS <-> LBA }
procedure CHS2LBA (Drive: integer; Head, Cylinder, Sector: longint; var LBA: longint);
var
  DriveData : PDriveData;
begin
  if not GetDriveFormat (Drive, DriveData) then Halt (1);
  LBA := ( Cylinder * (DriveData^.Heads + 1) + Head ) * DriveData^.Sectors
    + Sector - 1;
end;

procedure LBA2CHS (Drive: integer; var Head, Cylinder, Sector: longint; LBA: longint);
var
  DriveData : PDriveData;
begin
  if not GetDriveFormat (Drive, DriveData) then Halt (1);
  Sector := LBA mod DriveData^.sectors + 1;
  LBA := LBA div DriveData^.sectors;
  Head := LBA mod (DriveData^.heads + 1);
  Cylinder := LBA div (DriveData^.heads + 1);
end;

{ Umrechung LBA <-> BIOS HeadSecZyl }
procedure LBA2HeadSecZyl (Drive: integer; var Pos: TPos; LBA: longint; PQMagicComp: boolean);
var
  H,C,S     : longint;
  DriveData : PDriveData;
begin
  LBA2CHS (Drive, H, C, S, LBA);
  GetDriveFormat (Drive, DriveData);
  
  if (C > DriveData^.Cylinders) or (H > DriveData^.Heads)
      or (S > DriveData^.Sectors) then begin 
    { overflow -> int13x long value placeholder }
    
    if PQMagicComp then begin 
      if C > 1023 then C := 1023;
      if H > DriveData^.Heads then H := DriveData^.Heads; { kann doch gar nicht sein? }
      if S > DriveData^.Sectors then S := DriveData^.Sectors; { kann doch gar nicht sein? }
    end else begin 
      C := DriveData^.Cylinders;
      H := DriveData^.Heads;
      S := DriveData^.Sectors;
    end;
  end;
  
  CHS2HeadSecZyl (H, C, S, Pos);
end;

procedure HeadSecZyl2LBA (Drive: integer; Pos: TPos; var LBA: longint);
var
  Head, Cylinder, Sector :integer;
begin
  HeadSecZyl2CHS (Head, Cylinder, Sector, Pos);
  CHS2LBA (Drive, Head, Cylinder, Sector, LBA);
end;

function GetNumberOfDrives :integer; Assembler;
{replaced, stolen by Florian's diskca.asm :-) }
const
  FIRST_FIXED_DISK_ID = $80;
asm
  { Scan for Fixed Disks }
  mov dl, FIRST_FIXED_DISK_ID

@__IDNextDisk:
  push dx
  { INT 13, AH 15: Get Disk Type (AH) }
  mov ah, 15h
  int 13h
  pop dx
  jc @__IDLastDrive
  test ah, ah
  jz @__IDLastDrive

  { Next Fixed Disk }
  inc dl
  jnz @__IDNextDisk

@__IDLastDrive:
  { return Number of Fixed Disks }
  sub dl, FIRST_FIXED_DISK_ID
  xor ah, ah
  mov al, dl
end;


function ResetDrive (Drive: integer) :integer;
var
  regs: registers;
begin
  if Drive >= $80 then
    Regs.AH := $0D
  else
    Regs.AH := $00;
  Regs.DL := Drive;
  Intr ($13, Regs);
  if (Regs.Flags and 1) = 1 then
    ResetDrive := Regs.AH
  else
    ResetDrive := 0;
end;

function GetDriveStatus (Drive: integer) :integer;
var
  regs: registers;
begin
  Regs.AH := $01;
  Regs.DL := Drive;
  Intr ($13, Regs);
  if (Regs.Flags and 1) = 1 then
    GetDriveStatus := Regs.AH
  else
    GetDriveStatus := 0;
end;

function InitReadWriteSectors (Drive: integer; RWInit: TInit) :integer;
var
  regs: registers;
begin
  Init := RWInit;
  if (Emul = EMULWIN32) and
      ((RWInit = INITWRITE) or (RWInit = INITREADWRITE)) then begin 
    Regs.AX := $440D;
    Regs.BH := 1;
    Regs.BL := Drive;
    Regs.CX := $084B;
    Regs.DX := 1;
    Intr ($21, Regs);
    if (Regs.flags and fCarry) <> 0 then begin 
      InitReadWriteSectors := Regs.AX;
      Init := INITNONE;
    end
    else
      InitReadWriteSectors := 0;
  end
  else
    InitReadWriteSectors := 0;
end;

function DeInitReadWriteSectors (Drive: integer) :integer;
var
  regs: registers;
 begin
   if (Emul = EMULWIN32) and
       ((Init = INITWRITE) or (Init = INITREADWRITE)) then begin 
     Regs.AX := $440D;
     Regs.BL := Drive;
     Regs.CX := $086B;
     Intr ($21, Regs);
     if (Regs.flags and fCarry) <> 0 then
       DeInitReadWriteSectors := Regs.AX
     else
       DeInitReadWriteSectors := 0;
   end
   else
     DeInitReadWriteSectors := 0;
   
   Init := INITNONE;
 end;

{*************************************************************************}

type Int13XBlock = record
       Size      : Byte;    {00h    BYTE    10h (size of packet)}
       Res       : Byte;    {01h    BYTE    reserved (0)}
       Count     : Word;    {02h    WORD    number of blocks to transfer (max 007Fh for Phoenix EDD)}
       Addr      : Pointer; {04h    DWORD   -> transfer buffer}
    LBAlo     : longint; {08h    QWORD   starting absolute block number}
    LBAhi     : longint;
     end;

function ReadSectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;
var
  ErrCnt    :integer;
  Result    :integer;
  VPos      :TPos;
  Buf       :Int13XBlock;
  Regs      :Registers;
  DriveData :PDriveData;
begin
  if not GetDriveFormat (Drive, DriveData) then begin 
    ReadSectors := ErrorMemoryLow;
    Exit;
  end;
  
  if LBA + Count > DriveData^.TotalSectors then begin 
    ReadSectors := ErrorDriveBadLBA;
    Exit;
  end;
  
  if (Init = INITREAD) or (Init = INITREADWRITE) then begin 
    ErrCnt := MaxErrors;
    
    if DriveData^.Int13X then begin 
      Fillchar (Buf, SizeOf (Buf), 0);
      Buf.Size := SizeOf (Buf);
      Buf.Count := Count;
      Buf.Addr := Buffer;
      Buf.LBAlo := LBA;
      
      repeat
        Regs.AH := $42;                {INT 13 Extensions - EXTENDED READ}
        Regs.DL := Drive;
        Regs.DS := Seg (Buf);
        Regs.SI := Ofs (Buf);
        Intr ($13, Regs);
        if (Regs.Flags and FCarry) <> 0 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end
    else begin 
      LBA2HeadSecZyl (Drive, VPos, LBA, False);
      repeat
        Regs.AH := $02;
        Regs.DL := Drive;
        Regs.DH := VPos.Head;
        Regs.CX := VPos.SecCyl;
        Regs.AL := Count;
        Regs.ES := seg (Buffer^);
        Regs.BX := ofs (Buffer^);
        Intr ($13, Regs);
        if (Regs.Flags and FCarry) <> 0 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end;
    ReadSectors := Result;
  end
  else
    ReadSectors := ErrorDriveDeInit;
end;

{******************************}

function WriteSectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;
var
  ErrCnt :integer;
  Result :integer;
  VPos   :TPos;
  Buf    :Int13XBlock;
  Regs   :Registers;
  DriveData : PDriveData;
begin
  if not GetDriveFormat (Drive, DriveData) then begin 
    WriteSectors := ErrorMemoryLow;
    Exit;
  end;
  
  if LBA + Count > DriveData^.TotalSectors then begin 
    WriteSectors := ErrorDriveBadLBA;
    Exit;
  end;

  if (Init = INITWRITE) or (Init = INITREADWRITE) then begin 
    ErrCnt := MaxErrors;
    
    {for no-risk tests:}
    {writesectors := 0;
    Exit;}
    
    if DriveData^.Int13X then begin 
      Fillchar (Buf, SizeOf (Buf), 0);
      Buf.Size := SizeOf (Buf);
      Buf.Count := Count;
      Buf.Addr := Buffer;
      Buf.LBAlo := LBA;
      
      repeat
        Regs.AH := $43;                {INT 13 Extensions - EXTENDED WRITE}
        Regs.AL := 0;
        Regs.DL := Drive;
        Regs.DS := Seg (Buf);
        Regs.SI := Ofs (Buf);
        Intr ($13, Regs);
        if (Regs.Flags and FCarry) <> 0 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end
    else begin 
      LBA2HeadSecZyl (Drive, VPos, LBA, False);
      repeat
        Regs.AH := $03;
        Regs.DL := Drive;
        Regs.DH := VPos.Head;
        Regs.CX := VPos.SecCyl;
        Regs.AL := Count;
        Regs.ES := seg (Buffer^);
        Regs.BX := ofs (Buffer^);
        Intr ($13, Regs);
        if (Regs.Flags and 1) = 1 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end;
    WriteSectors := Result;
  end
  else
    WriteSectors := ErrorDriveDeInit;
end;

{******************************}

function VerifySectors (Drive: integer; LBA: longint; Count: integer; Buffer: pointer): integer;
var
  ErrCnt :integer;
  Result :integer;
  VPos   :TPos;
  Buf    :Int13XBlock;
  Regs   :Registers;
  DriveData : PDriveData;
begin
  if not GetDriveFormat (Drive, DriveData) then begin 
    VerifySectors := ErrorMemoryLow;
    Exit;
  end;
  
  if LBA + Count > DriveData^.TotalSectors then begin 
    VerifySectors := ErrorDriveBadLBA;
    Exit;
  end;
  
  if (Init = INITREAD) or (Init = INITREADWRITE) then begin 
    ErrCnt := MaxErrors;
    
    if DriveData^.Int13X then begin 
      Fillchar (Buf, SizeOf (Buf), 0);
      Buf.Size := SizeOf (Buf);
      Buf.Count := Count;
      Buf.Addr := Buffer;
      Buf.LBAlo := LBA;
      
      repeat
        Regs.AH := $44;                {INT 13 Extensions - EXTENDED VERIFY}
        Regs.DL := Drive;
        Regs.DS := Seg (Buf);
        Regs.SI := Ofs (Buf);
        Intr ($13, Regs);
        if (Regs.Flags and FCarry) <> 0 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end
    else begin 
      LBA2HeadSecZyl (Drive, VPos, LBA, False);
      repeat
        Regs.AH := $04;
        Regs.DL := Drive;
        Regs.DH := VPos.Head;
        Regs.CX := VPos.SecCyl;
        Regs.AL := Count;
        Regs.ES := seg (Buffer^);
        Regs.BX := ofs (Buffer^);
        Intr ($13, Regs);
        if (Regs.Flags and 1) = 1 then begin 
          Result := Regs.AH;
          ResetDrive (Drive);
        end
        else
          Result := 0;
        dec (ErrCnt);
      until (ErrCnt = 0) or (Result = 0) ;
    end;
    VerifySectors := Result;
  end
  else
    VerifySectors := ErrorDriveDeInit;
end;

{******************************}

function GetDriveFormat (Drive: integer; var DG: PDriveData) :boolean;
{added int13x support, modified parameters, UM}
type
  tcallbuffer = record
    size        : word;
    flag        : word;
    cylinders,
    heads,
    sectors     : longint;
    sizelo,
    sizehi      : longint;
    bytespersec : word;
    eddconf     : longint;
  end;

var
  regs      :registers;
  vpos      :tpos;
  Head,
  Cylinder,
  Sector    :integer;
  buf       :tcallbuffer;
begin
  GetDriveFormat := FALSE;

  if DriveFormat = nil then begin
    if MaxAvail < sizeof (TDriveData) then Exit;
    New (DriveFormat);

    {set "harmless" initialisation values}
    DriveFormat^.Drive := 0;
    DriveFormat^.Int13X := False;
    DriveFormat^.Cylinders := 0;
    DriveFormat^.Heads := 0;
    DriveFormat^.Sectors := 1;
    DriveFormat^.TotalSectors := 0;
    DriveFormat^.Next := nil;
  end;

  DG := DriveFormat;
  while (DG^.Drive <> Drive) and (DG^.Next <> nil) do
    DG := DG^.Next;

  if DG^.Drive <> Drive then begin
    if MaxAvail < sizeof (TDriveData) then Exit;
    New (DG^.Next);

    DG := DG^.Next;
    DG^.Drive := Drive;
    DG^.Cylinders := 0;
    DG^.Heads := 0;
    DG^.Sectors := 1;
    DG^.TotalSectors := 0;
    DG^.Next := nil;
    DG^.Int13x := false;

    Regs.AH := $08;
    Regs.DL := Drive;
    Intr ($13, Regs);
    if (Regs.Flags and FCarry) = 0 then begin
      {kein fehler}
      VPos.Head := Regs.DH;
      VPos.SecCyl := Regs.CX;
      { convert to CHS }
      HeadSecZyl2CHS (Head, Cylinder, Sector, VPos);

      DG^.Cylinders := Cylinder;
      DG^.Heads := Head;
      DG^.Sectors := Sector;
      DG^.TotalSectors := (dg^.Heads + 1) * (longint (Cylinder) + 1) * dg^.Sectors;
    end;
    
    Regs.AH := $41;
    Regs.BX := $55AA;
    Regs.DL := Drive;
    Intr ($13, Regs);
    if ((Regs.Flags and FCarry) = 0)   {Carry-Flag}
        and (Regs.BX = $AA55)          {installed flag}
        and (Regs.CX and 1 = 1)
        {bit 0 set: read/write support} then begin
      {BIOS supports Int13x}
      Regs.AH := $48;
      Regs.DL := Drive;
      Regs.DS := Seg (Buf);
      Regs.SI := Ofs (Buf);
      Buf.Size := sizeof (Buf);
      Intr ($13, Regs);
      if ((Regs.Flags and fCarry) = 0)
          and (Regs.AH = 0)
          and (Buf.bytespersec = 512)  {support only 512 byte sectors}
          and (Buf.sizehi = 0)         {<2 Terabyte} 
          and (buf.SizeLo > DG^.TotalSectors + DG^.Heads * DG^.Sectors)
            {more then one additional cylinder gain}
      then begin 
        DG^.int13x := true;
        DG^.TotalSectors := buf.SizeLo;
      end;
    end;
  end;

  {write('disk ', DG^.Drive, ': ',
    DG^.Cylinders, ' cyl, ',
    DG^.Heads, ' heads, ',
    DG^.Sectors, ' secs, ',
    DG^.TotalSectors, ' secs total, ');

  if DG^.Int13x then
    writeln('int13x support')
  else
    writeln('no int13x support');}

  GetDriveFormat := TRUE;
end;

function getfirstdrivesectors: byte;
var DG: PDriveData;
begin
  if GetDriveFormat($80, DG) then
    getfirstdrivesectors := dg^.sectors
  else
    getfirstdrivesectors := 0;
end;

{*************************************************************************}

var
  DosVer, WinVer :word;

begin
  Init := INITNONE;

  DosVer := GetDosVer;
  WinVer := GetWinVer;
  if WinVer = 0 then
    if hi (DosVer) <> 20 then
      Emul := EMULNONE
    else
      Emul := EMULOS2
  else if hi (WinVer) < 4 then
    Emul := EMULNONE
  else
    Emul := EMULWIN32;
end.


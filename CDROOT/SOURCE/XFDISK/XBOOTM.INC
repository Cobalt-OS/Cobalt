{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XBOOTM.INC
    Bootmanager Configuration and Installation Functions and Procedures

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a Copy of the GNU General Public License
    along with this program; if not, Write to
        Free Software Foundation, Inc.
        59 Temple Place - Suite 330
        Boston, MA  02111-1307, USA
    or visit the GNU Homepage at http://www.gnu.org/.
}

{Checksumme ermitteln}
function CalcCheck (var Source; Cnt: integer) :longint;
var
  iCnt   :integer;
  iChk   :longint;
  Buffer :PLBuffer; {changed to large buffer type, UM}
begin
  Buffer := PLBuffer (@Source);
  iChk := 0;
  for iCnt := 1 to Cnt do begin 
    {UM: disable range check; here will be an overflow}
    {this has nothing to do with debug mode!}
    {$IFOPT R+}
    {$DEFINE RANGECHECK}
    {$R-}
    {$ENDIF}
    {$IFOPT Q+}
    {$DEFINE OVERFLOWCHECK}
    {$Q-}
    {$ENDIF}
    
    iChk := iChk + (longint (Buffer^[iCnt - 1]) * longint (iCnt));
    if Odd (iChk) then
      iChk := iChk xor BMCheckOddMask
    else
      iChk := iChk xor BMCheckEvenMask;
    
    {$IFDEF RANGECHECK}
    {$UNDEF RANGECHECK}
    {$R+}
    {$ENDIF}
    {$IFDEF OVERFLOWCHECK}
    {$UNDEF OVERFLOWCHECK}
    {$Q+}
    {$ENDIF}
  end;
  
  CalcCheck := iChk
end;

{BootManager crypten}
procedure ProtectBM (var Source; Size: integer; Seed: longint);
var
  Cnt    :integer;
  Buffer :PLBuffer; {changed to large buffer type, UM}
begin
  Buffer := PLBuffer (@Source);
  RandSeed := Seed;
  for Cnt := 0 to Size - 1 do begin 
    Buffer^[Cnt] := Buffer^[Cnt] xor Random (256);
  end;
end;

{Bootmanager EintrÑge erstellen}
procedure DoCreateBMEntries(PartCyln: PPartCyln);
var
  HDriv             :PDriveChain;    {DriveChain}
  HPart             :PPartChain;     {PartitionChain}
  Pos               :integer;        {InfoEntr.Pos (Fkt.glob.), PartSect.Pos}
begin
  HDriv := DriveChain;
  {von jedem Laufwerk...}
  repeat
    HPart := HDriv^.PartChain;
    {...jede Partition abarbeiten}
    while HPart <> nil do begin
      {BootmenÅ-Eintrag?}
      if (HPart^.PartEntr <> PartEntrNone) and
       (HPart^.PartStat in [PartStatPri, PartStatAct, PartStatLog]) then begin
        {1.) gemeinsame EintrÑge von pri/log}
        Pos := HPart^.PartEntr;
        {BootDriv}
        PartCyln^.InfoSect.InfoEntr[Pos].BootDriv := HDriv^.Drive;
        {PartType}
        {BUG: öberlauf bei PartSyst = PartSystNone}
        {FIX: PrÅfen ob PartSyst gÅltig ist}
        if HPart^.PartSyst <> PartSystNone then
          PartCyln^.InfoSect.InfoEntr[Pos].PartType := HPart^.PartSyst - 1
        else
          PartCyln^.InfoSect.InfoEntr[Pos].PartType := PartSystNone;
        {PartName}
        StrCopy (PartCyln^.InfoSect.InfoEntr[Pos].PartName, HPart^.PartName);
        FillChar (PartCyln^.InfoSect.PWDEntr[Pos], 18, 0);
        {Passwort}
        if HPart^.PartPWD[0] <> #0 then begin
          StrCopy (PartCyln^.InfoSect.PWDEntr[Pos].Password, HPart^.PartPWD);
          ProtectPWD (PartCyln^.InfoSect.PWDEntr[Pos].Password, StrLen (HPart^.PartPWD));
          PartCyln^.InfoSect.PWDEntr[Pos].Protection := StrLen (HPart^.PartPWD);
        end;
        PartCyln^.InfoSect.InfoEntr[Pos].EntrMark := Marker;

        {2.) Unterschiede zwischen pri/log}
        if HPart^.PartStat = PartStatLog then begin {log. Laufwerk}
          {BootPos}
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.InfoSect.InfoEntr[Pos].BootPos,
            HPart^.StartSec, PQMagicComp);
          {PartLDrv}
          {BUG: Runtime Error 201: Range Check Overflow
          ...PartDriv + Logstart... bei PartDriv = PartDrivNone}
          {FIX: PrÅfen, ob das logische Laufwerk gÅltig ist}
          if HPart^.PartDriv <> PartDrivNone then
{            PartCyln^.InfoSect.InfoEntr[Pos].PartLDrv := HPart^.PartDriv + LogStart + FirstDriveIndex}
            PartCyln^.InfoSect.InfoEntr[Pos].PartLDrv := HPart^.PartDriv + FirstDriveIndex
          else
            PartCyln^.InfoSect.InfoEntr[Pos].PartLDrv := 0;
          {PartEntr}
          PartCyln^.InfoSect.InfoEntr[Pos].PartEntr := 0;
          {PartSize}
          PartCyln^.InfoSect.InfoEntr[Pos].PartSize :=
            round ((HPart^.PartSize - (HPart^.StartSec - HPart^.Distance)) / 2048);
          {Distance}
          {Bootmanager greift direkt auf log. Laufwerk zu:}
          PartCyln^.InfoSect.InfoEntr[Pos].Distance := HPart^.StartSec;
        end else begin {prim. Partition}
          {BootPos (CHS)}
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.InfoSect.InfoEntr[Pos].BootPos,
            HPart^.Distance, PQMagicComp);
          {PartLDrv}
          PartCyln^.InfoSect.InfoEntr[Pos].PartLDrv := 0;
          {PartEntr}
          {wurde bei Version <=0.8.5 als Nummer im Partitionssektor bentutzt,
          jetzt nur noch zur Unterscheidung primar/logisch im BootmenÅ}
          PartCyln^.InfoSect.InfoEntr[Pos].PartEntr := 1;
          {PartSize}
          PartCyln^.InfoSect.InfoEntr[Pos].PartSize := round (HPart^.PartSize / 2048);
          {Distanze}
          PartCyln^.InfoSect.InfoEntr[Pos].Distance := HPart^.Distance;
        end;
      end;

      HPart := HPart^.Next;
    end;
    HDriv := HDriv^.Next;
  until HDriv = nil;

  {Reste lîschen!!!}
  {BUG: Runtime Error 201: Range Check Overflow
  ...InfoEntr[InfoEntr]... bei InfoEntr = MaxEntr}
  {FIX: Puffer nur lîschen, wenn nîtig, also InfoEntr < MaxEntr}
  if InfoEntr < MaxEntries then begin
    FillChar (PartCyln^.InfoSect.InfoEntr[InfoEntr], (MaxEntries - InfoEntr) * 32, 0);
    FillChar (PartCyln^.InfoSect.PWDEntr[InfoEntr], (MaxEntries - InfoEntr) * 18, 0);
  end;

  {Passwîrter}
  FillChar (PartCyln^.InfoSect.MasterPWD, 18, 0);
  if MasterPWD[0] <> #0 then begin
    StrCopy (PartCyln^.InfoSect.MasterPWD.Password, MasterPWD);
    ProtectPWD (PartCyln^.InfoSect.MasterPWD.Password, StrLen (MasterPWD));
    PartCyln^.InfoSect.MasterPWD.Protection := StrLen (MasterPWD);
  end;

  FillChar (PartCyln^.InfoSect.FloppyPWD, 18, 0);
  if FloppyPWD[0] <> #0 then begin
    StrCopy (PartCyln^.InfoSect.FloppyPWD.Password, FloppyPWD);
    ProtectPWD (PartCyln^.InfoSect.FloppyPWD.Password, StrLen (FloppyPWD));
    PartCyln^.InfoSect.FloppyPWD.Protection := StrLen (FloppyPWD);
  end;

  PartCyln^.InfoSect.WaitTime := Timeout;
  PartCyln^.InfoSect.TimeHndl := TimeHndl;

  PartCyln^.InfoSect.ClearScrn := byte (ClearScrn);
  PartCyln^.InfoSect.BootLast := byte (BootLast);
  PartCyln^.InfoSect.SimpleMenus := byte (SimpleMenus);
  PartCyln^.InfoSect.SuppBlind := byte (SuppBlind);
  PartCyln^.InfoSect.AutoHide := byte (AutoHide);
  PartCyln^.InfoSect.LastEntry := LastEntry;
  PartCyln^.InfoSect.FloppyBoot := 0;

  {PartCyln^.InfoSect.MENChkSum := CalcCheck (PartCyln^.CodeSect[0], CheckSize);}
  PartCyln^.InfoSect.MENChkSum := CalcCheck (PartCyln^.CodeSect[2], NewCHKSize);
end;

{BootManager Installation}
function DoInstallBM(isupdate: boolean) :boolean;
var
  PartCyln    :PPartCyln;
  Res, Res2   :integer;
  BinFile     :file;
  Buf         :PBuffer;
  n, i        :Word;
  sec         :byte;
const
  Reserved    :String[21] = '* reserved by XFDisk ';
begin
  DoInstallBM := FALSE;

  if MaxAvail < sizeof (TPartCyln) then begin
    MemoryError;
    Exit;
  end;
  New (PartCyln);
  sec := getfirstdrivesectors;

  if sec >= 15 then begin
   InitReadWriteSectors (FirstDrive, INITREAD);
   {Res := ReadSectors (FirstDrive, 0, PartCylnSectorCount, PartCyln);}
   {xfdisk now does not use codesect[0..1] any more and is stored at the
    end of the first head}
   Res := ReadSectors (FirstDrive, 0, 1, PartCyln);
   Res := ReadSectors (FirstDrive, sec - 14, 14, @(PartCyln^.CodeSect[2]));
   DeInitReadWriteSectors (FirstDrive);

   if Res = 0 then begin
    {Ist der originale MBR Åberhaupt gÅltig?}
    if PartCyln^.PartSect.PartMark = Marker then begin
      if not isupdate then
        Move (PartCyln^.PartSect, PartCyln^.OldMBR, 512);
      FillChar (PartCyln^.PartSect, 440, 0)
    end else
      FillChar (PartCyln^.PartSect, 512, 0);
    FillChar (PartCyln^.CodeSect[0], 8192, $F6);
    {UM: mit 0xF6 initialisieren}

    {UM: OldMBR must not be empty, otherwise PQMagic will store its
         settings here and lose it every time XFDisk is updated}
    if PartCyln^.OldMBR.PartMark <> Marker then begin
      Buf := @PartCyln^.OldMBR;
      n := 0;
      i := 1;
      while n < 512 do begin
        Buf^[n] := byte(Reserved[i]);
        n := n + 1;
        i := i + 1;
        if i > length(Reserved) then
          i := 1;
      end;
    end;

    Assign (BinFile, XFDISKPath);
    FileMode := 0;
    Reset (BinFile, 1);
    if IOResult = 0 then
      Seek (BinFile, BinarySize);
    if IOResult = 0 then begin
      BlockRead (BinFile, PartCyln^.PartSect, XMBRSize, Res);
      if Res = XMBRSize then begin
        BlockRead (BinFile, PartCyln^.CodeSect[2], XMENUSize, Res);
        {0.9.3: [0] -> [2]}

        BlockRead (BinFile, PartCyln^.InfoSect.MENRndIdx, 4, Res2);
        Close (BinFile);
        if (Res = XMENUSize) and (Res2 = 4) then begin
          PartCyln^.InfoSect.InfoMark := Marker;

          PartCyln^.PartSect.PartMark := Marker;

          PartCyln^.InfoSect.VerMark := Marker;
          PartCyln^.InfoSect.VerInfo := VerInfo.Prog * 10000 +
            VerInfo.Major * 100 + VerInfo.Minor * 10 + VerInfo.Patch;
          {0.9.3: Patchnummer hinzugefÅgt}

          DoCreateBMEntries(PartCyln);

          {Encrypten}
          {ProtectBM (PartCyln^.CodeSect[0], XCRYSize, PartCyln^.InfoSect.MENRndIdx);}
          ProtectBM (PartCyln^.CodeSect[2], NewCRYSize, PartCyln^.InfoSect.MENRndIdx);

          InitReadWriteSectors (FirstDrive, INITWRITE);

          {If BMCleanOldVersion Then Begin
            FillChar (PartCyln^.CodeSect[0], 1024, 0);
            Res := WriteSectors (FirstDrive, 0, PartCylnSectorCount, PartCyln)
          End Else Begin
            Res := WriteSectors (FirstDrive, 0, 1, PartCyln);
            Res := WriteSectors (FirstDrive, 3, PartCylnSectorCount-2, @(PartCyln^.CodeSect[2]));
          End;}

          Res := WriteSectors (FirstDrive, 0, 1, PartCyln);
          Res := WriteSectors (FirstDrive, sec - 14, 14, @(PartCyln^.CodeSect[2]));
          DeInitReadWriteSectors (FirstDrive);

          if Res = 0 then begin
            doInstallBM := TRUE

          end else
            MessageBox (BOX_WARN_errHDD_HDR,
              BOX_WARN + BOX_WARN_errHDD_wrtPCyl,
              ButtonOK or ButtonDefOK)

        end else
          MessageBox (BOX_WARN_errFile_HDR,
            BOX_WARN + BOX_WARN_errFile_rdBMgr,
            ButtonOK or ButtonDefOK)

      end
      else
        MessageBox (BOX_WARN_errFile_HDR,
          BOX_WARN + BOX_WARN_errFile_rdMBR,
          ButtonOK or ButtonDefOK)

    end
    else
      MessageBox (BOX_WARN_errFile_HDR,
        BOX_WARN + BOX_WARN_errFile_opnXFD,
        ButtonOK or ButtonDefOK)

  end
  else
    MessageBox (BOX_WARN_errHDD_HDR,
      BOX_WARN + BOX_WARN_errHDD_rdPCyl,
      ButtonOK or ButtonDefOK);

   dispose (PartCyln);
   doInstallBM := TRUE;
  end;
end;

function InstallBM: boolean;
begin
  InstallBM := doInstallBM(false);
end;

function UpdateBM: boolean;
begin
  UpdateBM := doInstallBM(true);
end;


{BootManager Deinstallation}
function UnInstallBM :boolean;
var
  PartCyln    :PPartCyln;
  Res         :integer;
  HDriv       :PDriveChain;
  HPart       :PPartChain;
  sec         :byte;
begin
  UnInstallBM := FALSE;

  if MaxAvail < sizeof (TPartCyln) then begin
    MemoryError;
    Exit;
  end;
  New (PartCyln);

  sec := getfirstdrivesectors;
  if(sec>=15) then begin
   InitReadWriteSectors (FirstDrive, INITREAD);
   {Res := ReadSectors (FirstDrive, 0, PartCylnSectorCount, PartCyln);}
   Res := ReadSectors (FirstDrive, 0, 1, PartCyln);
   Res := ReadSectors (FirstDrive, sec - 14, 14, @(PartCyln^.CodeSect[2]));
   DeInitReadWriteSectors (FirstDrive);
  end else res := 1;

  if Res = 0 then begin
    {Ist der alte MBR Åberhaupt gÅltig?}
    if PartCyln^.OldMBR.PartMark = Marker then
      Move (PartCyln^.OldMBR, PartCyln^.PartSect, 446);
    FillChar (PartCyln^.CodeSect[0], 8192, 0);

    InitReadWriteSectors (FirstDrive, INITWRITE);

    {If BMCleanOldVersion Then {0.9.3}
    {  Res := WriteSectors (FirstDrive, 0, PartCylnSectorCount, PartCyln)
    Else Begin
      Res := WriteSectors (FirstDrive, 0, 1, PartCyln);
      Res := WriteSectors (FirstDrive, 3, PartCylnSectorCount-2, @(PartCyln^.CodeSect[2]));
    End;}
    Res := WriteSectors (FirstDrive, 0, 1, PartCyln);
    Res := WriteSectors (FirstDrive, sec - 14, 14, @(PartCyln^.CodeSect[2]));
    DeInitReadWriteSectors (FirstDrive);

    if Res = 0 then begin
      UnInstallBM := TRUE;

    end
    else
      MessageBox (BOX_WARN_errHDD_HDR,
        BOX_WARN + BOX_WARN_errHDD_wrtPCyl,
        ButtonOK or ButtonDefOK)

  end
  else
    MessageBox (BOX_WARN_errHDD_HDR,
      BOX_WARN + BOX_WARN_errHDD_rdPCyl,
      ButtonOK or ButtonDefOK);

  dispose (PartCyln);
end;

procedure CheckMBR; {check if XFDisk's MBR is installed}
var
  MBRinstalled, MBRxfdisk : TBuffer;
  BinFile     :file;
  Res         :integer;
  equal       :boolean;
  n           :Word;
const
  Reserved    :String[21] = '* reserved by XFDisk ';
begin
  InitReadWriteSectors (FirstDrive, INITREAD);
  Res := ReadSectors (FirstDrive, 0, 1, @MBRinstalled);
  DeInitReadWriteSectors (FirstDrive);

  if Res = 0 then begin
    Assign (BinFile, XFDISKPath);
    FileMode := 0;
    Reset (BinFile, 1);
    if IOResult <> 0 then
      exit;
    Seek (BinFile, BinarySize);
    if IOResult <> 0 then
      exit;

    BlockRead (BinFile, MBRxfdisk, XMBRSize, Res);
    if Res <> XMBRSize then exit;
    Close(BinFile);

    equal := TRUE;
    for n :=0 to XMBRSize - 1 do {000924-UM: starts at 0}
      if MBRinstalled[n] <> MBRxfdisk[n] then
        equal := FALSE;

    if not equal then begin
      BMUpdate := true;

      Res := MessageBox (BOX_INFO_mbr1,
               BOX_INFO + BOX_INFO_mbr2,
               ButtonOK or ButtonDefOK);
    end;
  end;
end;

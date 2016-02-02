{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XPART.INC
    Partition Configuration Functions and Procedures

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

{MBR schreiben}
function WriteMBR (Drive: integer) :boolean;
var
  MBRSect     :PPartSect;
  ErrCnt, Res :integer;
  Cnt         :integer;
  BinFile     :file;
begin
  WriteMBR := FALSE;
  
  if MaxAvail < sizeof (TPartSect) then begin 
    MemoryError;
    Exit;
  end;
  New (MBRSect);

  InitReadWriteSectors (Drive, INITREAD);
  Res := ReadSectors (Drive, 0, 1, MBRSect);
  DeInitReadWriteSectors (Drive);

  if Res = 0 then begin 
    FillChar (MBRSect^, 446, 0);
    
    Assign (BinFile, XFDISKPath);
    FileMode := 0;
    Reset (BinFile, 1);
    if IOResult = 0 then
      Seek (BinFile, BinarySize);
    if IOResult = 0 then begin 
      BlockRead (BinFile, MBRSect^, XMBRSize, Res);
      Close (BinFile);
      if Res = XMBRSize then begin 
        MBRSect^.PartMark := Marker;

        InitReadWriteSectors (Drive, INITWRITE);
        Res := WriteSectors (Drive, 0, 1, MBRSect);
        DeInitReadWriteSectors (Drive);
        
        if Res = 0 then
          WriteMBR := TRUE
        else
          MessageBox (BOX_WARN_errHDD_HDR, 
            BOX_WARN + BOX_WARN_errHDD_wrtMBR, 
            ButtonOK or ButtonDefOK);
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
      BOX_WARN + BOX_WARN_errHDD_rdMBR, 
      ButtonOK or ButtonDefOK);
  
  dispose (MBRSect)
end;

{Freiraum feststellen und DriveChain korrigieren}
function CleanUpChain (DriveChain: PDriveChain) :boolean;

  {kann hier log. Laufwerk angelegt werden?}
  function validLogArea(HDriv: PDriveChain; HPart: PPartChain): boolean;
  var
    WPart  :PPartChain;
    l1, l2 :longint;
    result :boolean;
  begin
    result := true;

    if HDriv^.ExtPart then begin
      {ermittle Start und Ende der log. Laufwerke}
      l1:=0;
      l2:=0;
      WPart := HDriv^.PartChain;
      while WPart <> nil do begin
        if WPart^.PartStat=PartStatLog then begin
          if (l1 = 0) or (WPart^.Distance < l1) then
            l1 := WPart^.Distance;
          if (l2 = 0) or (WPart^.Distance > l2) then
            l2 := WPart^.Distance;
        end;
        WPart := WPart^.next;
      end;

      {liegt prim. Partition zwischen HPart und log. Laufwerken?}
      WPart := HDriv^.PartChain;
      while (result) and (WPart <> nil) do begin
        if WPart^.PartStat in [PartStatPri, PartStatAct] then begin
          if (l2 < WPart^.Distance) and (WPart^.Distance < HPart^.Distance)
           or (HPart^.Distance < WPart^.Distance) and (WPart^.Distance < l1)
            then result := false;
        end;
        WPart := WPart^.next;
      end;
    end;
    validLogArea := result;
  end;

  {liegt Bereich zwischen zwei logischen Laufwerken?}
  function betweenLogDrives(HDriv: PDriveChain; HPart: PPartChain): boolean;
  var
    WPart       :PPartChain;
    left, right :boolean;
  begin
    left := false;
    right := false;

    WPart := HDriv^.PartChain;
    while WPart <> nil do begin
      if WPart^.PartStat=PartStatLog then begin
        if WPart^.Distance < HPart^.Distance then
          left := true;
        if WPart^.Distance > HPart^.Distance then
          right := true;
      end;
      WPart := WPart^.next;
    end;

    betweenLogDrives := (left and right);
  end;

var
  HDriv        :PDriveChain;
  HPart, WPart :PPartChain;
  LogEntr      :integer;
  lba          :longint;
  lwb          :byte;
  PartPosUsed  :Array [0..MaxPartitions-1] of Boolean;
  PartPosExtended,
  i            :byte;
begin
  CleanUpChain := FALSE;
  HDriv := DriveChain;
  repeat
    HPart := HDriv^.PartChain;
    {Checken, ob die Partitiostabelle belegt ist...}
    if HPart <> nil then begin
      {Check wegen erweitereter Partition am Anfang}
      {UM: wird nicht mehr benoetigt; Kontrolle geschieht jetzt beim
      Erstellen neuer Partitionen}

      {Checken, ob am Anfang noch was frei ist...}
      CHS2LBA (HDriv^.Drive, 1, 0, 1, lba);
      {Head, Zyl, Sek...}
      if HPart^.Distance > lba then begin
        if MaxAvail < sizeof (TPartChain) then begin
          MemoryError;
          Exit;
        end;
        New (WPart);

        WPart^.IsNew := IsNew_No;
        WPart^.PartDriv := PartDrivNone;
        WPart^.PartStat := PartStatFree;
        WPart^.PartType := PartTypeNone;
        
        WPart^.Distance := lba;
        WPart^.PartSize := HPart^.Distance - WPart^.Distance;

        WPart^.PartSyst := PartSystNone;
        WPart^.PartEntr := PartEntrNone;
        WPart^.PartPos := PartPosNone;
        
        WPart^.PartLabl[0] := #0;
        WPart^.PartName[0] := #0;
        WPart^.PartPWD[0] := #0;
        WPart^.KnowsPass := FALSE;
        
        WPart^.Next := HPart;
        HDriv^.PartChain := WPart
      end;

      {Checken, ob dazwischen noch was frei ist...}
      HPart := HDriv^.PartChain;
      while HPart^.Next <> nil do begin
        if HPart^.Distance + HPart^.PartSize < HPart^.Next^.Distance then begin 
          if MaxAvail < sizeof (TPartChain) then begin 
            MemoryError;
            Exit;
          end;
          New (WPart);
          
          WPart^.IsNew := IsNew_No;
          WPart^.PartDriv := PartDrivNone;
          WPart^.PartStat := PartStatFree;
          WPart^.PartType := PartTypeNone;

          WPart^.Distance := HPart^.Distance + HPart^.PartSize;
          WPart^.PartSize := HPart^.Next^.Distance - WPart^.Distance;
          
          WPart^.PartSyst := PartSystNone;
          WPart^.PartEntr := PartEntrNone;
          WPart^.PartPos := PartPosNone;
          
          WPart^.PartLabl[0] := #0;
          WPart^.PartName[0] := #0;
          WPart^.PartPWD[0] := #0;
          WPart^.KnowsPass := FALSE;
          
          WPart^.Next := HPart^.Next;
          HPart^.Next := WPart
        end;
        
        HPart := HPart^.Next
      end;

      {Checken, ob am Ende noch was frei ist...}
      if HPart^.Distance + HPart^.PartSize < HDriv^.Size then begin 
        if MaxAvail < sizeof (TPartChain) then begin 
          MemoryError;
          Exit;
        end;
        New (WPart);

        WPart^.IsNew := IsNew_No;
        WPart^.PartDriv := PartDrivNone;
        WPart^.PartStat := PartStatFree;
        WPart^.PartType := PartTypeNone;
        
        WPart^.Distance := HPart^.Distance + HPart^.PartSize;
        WPart^.PartSize := HDriv^.Size - WPart^.Distance;
        
        WPart^.PartSyst := PartSystNone;
        WPart^.PartEntr := PartEntrNone;
        WPart^.PartPos := PartPosNone;
        
        WPart^.PartLabl[0] := #0;
        WPart^.PartName[0] := #0;
        WPart^.PartPWD[0] := #0;
        WPart^.KnowsPass := FALSE;

        WPart^.Next := nil;
        HPart^.Next := WPart
      end;
      
      {ZurÅcksetzen}
      HPart := HDriv^.PartChain;
      while HPart <> nil do begin 
        if not (HPart^.PartStat in [PartStatPri, PartStatAct, PartStatLog]) then
          HPart^.PartStat := PartStatFree;
        HPart := HPart^.Next
      end;

      {Soo, und jetzt zusammenfassen...}
      HPart := HDriv^.PartChain;
      while HPart <> nil do begin 
        if HPart^.PartStat = PartStatFree then begin 
          WPart := HPart^.Next;
          while (WPart <> nil) and (WPart^.PartStat = PartStatFree) do begin
            HPart^.PartSize := HPart^.PartSize + WPart^.PartSize;
            HPart^.Next := WPart^.Next; dispose (WPart);
            WPart := HPart^.Next
          end
        end;
        
        HPart := HPart^.Next
      end;
      
      {Wenn die Partitionstabelle leer ist, dann eine neue anlegen}
    end
    else begin 
      if MaxAvail < sizeof (TPartChain) then begin 
        MemoryError;
        Exit;
      end;
      New (HPart);
      
      HPart^.IsNew := IsNew_No;
      HPart^.PartDriv := PartDrivNone;
      HPart^.PartStat := PartStatFree;
      HPart^.PartType := PartTypeNone;

      CHS2LBA (HDriv^.Drive, 1, 0, 1, HPart^.Distance);
      {H,Z,S!}
      HPart^.PartSize := HDriv^.Size - HPart^.Distance;
      
      HPart^.PartSyst := PartSystNone;
      HPart^.PartEntr := PartEntrNone;
      HPart^.PartPos := PartPosNone;
      
      HPart^.PartLabl[0] := #0;
      HPart^.PartName[0] := #0;
      HPart^.PartPWD[0] := #0;
      WPart^.KnowsPass := FALSE;
      
      HPart^.Next := nil;
      HDriv^.PartChain := HPart
    end;
    
    HDriv := HDriv^.Next;
  until HDriv = nil ;
  
  {Jetzt die Chain korrigieren...}
  {1. PriPart/ExtPart ermitteln}
  HDriv := DriveChain;
  repeat
    HPart := HDriv^.PartChain;

    HDriv^.PriPart := 0; {Anzahl prim. Partitionen}
    HDriv^.ExtPart := FALSE;
    while HPart <> nil do begin
      if HPart^.PartStat = PartStatLog then {Logisches Laufwerk?}
        HDriv^.ExtPart := True
      else if HPart^.PartStat in [PartStatPri, PartStatAct] then
        Inc (HDriv^.PriPart);

      HPart^.PartDriv := 0; {erst mal kein LWbuchst}
      HPart := HPart^.next;
    end;
    HDriv := HDriv^.Next;
  until HDriv = nil;

  LWB := 1; { zu vergebender Laufwerksbuchstabe }
  {1.) "erste" primÑre Partitionen zuweisen}
  HDriv := DriveChain;
  while (HDriv <> nil) and (LWB < MaxLWB) do begin
    {erste aktive Partition suchen}
    HPart := HDriv^.PartChain;
    while (HPart <> nil) and not
     ((HPart^.PartStat = PartStatAct) and
      (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E])) do
      HPart := HPart^.next;

    if HPart <> nil then begin {aktive Partition gefunden}
      HPart^.PartDriv := LWB;
      LWB := LWB + 1;
    end else begin {also erste primÑre nicht-aktive suchen}
      HPart := HDriv^.PartChain;
      while (HPart <> nil) and not
       ((HPart^.PartStat = PartStatPri) and
        (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E])) do
        HPart := HPart^.next;

      if HPart <> nil then begin
        HPart^.PartDriv := LWB;
        LWB := LWB + 1;
      end;
    end;

    HDriv := HDriv^.Next;
  end;

  {2.) logische Laufwerke zuweisen}
  HDriv := DriveChain;
  while (HDriv <> nil) and (LWB < MaxLWB) do begin
    HPart := HDriv^.PartChain;
    while (HPart <> nil) and (LWB < MaxLWB) do begin
      while (HPart <> nil) and not
       ((HPart^.PartStat = PartStatLog) and
        (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E])) do
        HPart := HPart^.next;

      if HPart <> nil then begin
        HPart^.PartDriv := LWB;
        LWB := LWB + 1;
        HPart := HPart^.next;
      end;
    end;
    HDriv := HDriv^.Next;
  end;

  {3.) restliche prim. Partitionen zuweisen}
  HDriv := DriveChain;
  while (HDriv <> nil) and (LWB < MaxLWB) do begin
    HPart := HDriv^.PartChain;
    while (HPart <> nil) do begin
      while (HPart <> nil) and not
       ((HPart^.PartStat in [PartStatPri, PartStatAct]) and
        (HPart^.PartDriv = 0) and
        (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E])) do
        HPart := HPart^.next;

      if HPart <> nil then begin
        if autohide then begin
          HPart^.PartType := HPart^.PartType + PartMaskHide;
          HDriv^.HasChanged := true;
        end else begin
          HPart^.PartDriv := LWB;
          LWB := LWB + 1;
        end;
        HPart := HPart^.next;
      end;
    end;

    HDriv := HDriv^.Next;
  end;

  {...und die freien Bereiche definieren}
  HDriv := DriveChain;
  repeat
    HPart := HDriv^.PartChain;
    while HPart <> nil do begin
      {Freier Bereich?}
      if HPart^.PartStat = PartStatFree then
        {Maximal zulÑssige primÑre Partitionen?}
        if HDriv^.PriPart >= MaxPartitions then
          HPart^.PartStat := PartStatUnusable
          {Eine weniger als maximal zulÑssig?}
        else if HDriv^.PriPart = (MaxPartitions - 1) then
          {Keine logischen Laufwerke?}
          if not HDriv^.ExtPart then
            HPart^.PartStat := PartStatFreePriLog
            {Liegt der freie Bereich zulÑssig?}
          else if validLogArea(HDriv, HPart) then
            HPart^.PartStat := PartStatFreeLog
          else HPart^.PartStat := PartStatUnusable
        {Keine logischen Laufwerke?}
        else if not HDriv^.ExtPart then
          HPart^.PartStat := PartStatFreePriLog
        {Liegt der freie Bereich zwischen zwei logischen Laufwerken?}
        else if betweenLogDrives(HDriv, HPart) then
          HPart^.PartStat := PartStatFreeLog
        else if validLogArea(HDriv, HPart) then
          HPart^.PartStat := PartStatFreePriLog
        else HPart^.PartStat := PartStatFreePri;

      {Ist es eine "Mini"-Partition?}
      if HPart^.PartSize < HDriv^.MinSize then begin
        {Ist der freie Bereich auch fÅr Partitionen vorgesehen?}
        if HPart^.PartStat = PartStatFreePriLog then
          HPart^.PartStat := PartStatFreePri;
        {Ist der freie Bereich nur fÅr logische Laufwerke vorgesehen?}
        if HPart^.PartStat = PartStatFreeLog then
          HPart^.PartStat := PartStatUnusable;
        {Liegt der Bereich am Ende der Platte und ist kleiner als ein Zylinder?}
        if HPart^.Distance + HDriv^.MinSize > HDriv^.Size then
          HPart^.PartStat := PartStatUnusable;

      end;

      HPart := HPart^.Next
    end;

    HDriv := HDriv^.Next;
  until HDriv = nil ;

  {PartPos anpassen}
  HDriv := DriveChain;
  repeat
    for i := 0 to MaxPartitions - 1 do
      PartPosUsed[i] := false;

    {PartPosUsed initialisieren}
    PartPosExtended := PartPosNone;
    HPart := HDriv^.PartChain;
    while HPart <> nil do begin
      if not SortPartTable then begin
        if HPart^.PartPos <> PartPosNone then begin
          PartPosUsed[HPart^.PartPos] := true;
          if HPart^.PartStat = PartStatLog then
            PartPosExtended := HPart^.PartPos;
        end;
      end else begin
        HPart^.PartPos := PartPosNone;
      end;

      HPart := HPart^.next;
    end;

    {PartPos definieren}
    HPart := HDriv^.PartChain;
    while HPart <> nil do begin
      if HPart^.PartPos = PartPosNone then begin
        if (HPart^.PartStat = PartStatLog)
         and (PartPosExtended <> PartPosNone) then begin
          HPart^.PartPos := PartPosExtended
        end else if HPart^.PartStat in [PartStatPri, PartStatAct, PartStatLog]
         then begin
          i := 0;
          while (i < MaxPartitions) and (PartPosUsed[i]) do
            inc(i);

          if i = MaxPartitions then begin
            MemoryError; {trifft es nicht ganz, aber besser als gar nix}
            Exit;
          end;
          PartPosUsed[i] := True;
          if HPart^.PartStat = PartStatLog then
            PartPosExtended := i;

          HPart^.PartPos := i;
        end;
      end;

      HPart := HPart^.next;
    end;

    HDriv := HDriv^.Next;
  until HDriv = nil ;

  CleanUpChain := TRUE;
end;

{Partitionstabelle aus dem Speicher lîschen}
procedure UnloadTable (DriveChain: PDriveChain);
var
  HDriv, WDriv :PDriveChain;
  HPart, WPart :PPartChain;
begin
  HDriv := DriveChain;
  while HDriv <> nil do begin 
    HPart := HDriv^.PartChain;
    while HPart <> nil do begin
      WPart := HPart^.Next;
      dispose (HPart);
      HPart := WPart
    end;

    WDriv := HDriv^.Next;
    dispose (HDriv);
    HDriv := WDriv
  end
end;

{Partitionstabelle laden}
function LoadTable :boolean;

  function CheckBootSect (BootSect: PBootSect; CStr: string) :boolean;
  var
    Cnt :integer;
  begin
    CheckBootSect := TRUE;
    for Cnt := 1 to length (CStr) do
      if BootSect^.PartSyst[Cnt - 1] <> Ord (CStr[Cnt]) then
        CheckBootSect := FALSE
  end;

var
  Cnt, CI                   :integer;
  Res, PCnt, ECnt           :integer;
  VPos                      :longint;
  PartSect, HPSect, HPSect2 :PPartSect;
  BootSect                  :PBootSect;
  PartCyln                  :PPartCyln;
  HDriv                     :PDriveChain;
  HPart, WPart, DPE         :PPartChain;
  RealEntr                  :integer;
  InfoBox                   :pointer;
  MoreVolumes               :Boolean;
  Distance                  :longint;
  H, S, C                   :longint; {changed, UM}
  DriveData                 :PDriveData; {added, UM}
  SPos                      :longint; {added, UM}
  HPos                      :TPos;
  StartSect                 :byte;
  sec                       :byte;
begin
  LoadTable := FALSE;

  InfoBox := CreateInfoBox (BOX_INFO_rdSyst_HDR, BOX_INFO_rdSyst);
  if InfoBox = nil then begin
    MemoryError;
    Exit;
  end;

  if MaxAvail < (sizeof (TPartSect) + sizeof (TPartCyln)) then begin
    DestroyInfoBox (InfoBox);
    MemoryError;
    Exit;
  end;
  New (PartSect);
  New (PartCyln);

  sec := getfirstdrivesectors;
  if(sec>=15) then begin
   InitReadWriteSectors (FirstDrive, INITREAD);
   {Res := ReadSectors (FirstDrive, BMSectorStart, BMSectorCount, @PartCyln^.CodeSect[0]);}
   Res := ReadSectors (FirstDrive, sec - 14, 14, @(PartCyln^.CodeSect[2]));
   DeInitReadWriteSectors (FirstDrive);
  end else res := 1;

  if Res <> 0 then begin
    DestroyInfoBox (InfoBox);
    MessageBox (BOX_WARN_errHDD_HDR, 
      BOX_WARN + BOX_WARN_errHDD_rdBMgr, 
      ButtonOK or ButtonDefOK);
    
    dispose (PartSect);
    dispose (PartCyln);
    Exit;
  end;
  
  {HasChanged := FALSE;}
  WriteBM := FALSE;
  
  BMIsInst := FALSE;
  BMHasPWDs := FALSE;
  
  BMUpdate := FALSE;
  
  BMNewInst := FALSE;
  BMPrevInst := FALSE;
  BMPrevVer := 0;

  InfoEntr := 0;
  RealEntr := 0;
  
  {PrÅfen, ob der BootManager installiert ist...}
  if PartCyln^.InfoSect.InfoMark = Marker then begin
    StartSect := 0;
    {Unterschiedliche CryptSize ab Version 1.0.9.3}
    if (PartCyln^.InfoSect.VerMark = Marker) and
        (PartCyln^.InfoSect.VerInfo >= BMVersionExtension4) then begin
      CryptSize := NewCRYSize;
      CheckSize := NewCHKSize;
      StartSect := 2;
    end;
    {Unterschiedliche CryptSize ab Version 1.07}
    if (PartCyln^.InfoSect.VerMark = Marker) and
        (PartCyln^.InfoSect.VerInfo < BMVersionExtension2) then begin
      CryptSize := OldCRYSize;
      CheckSize := OldCHKSize;
    end;
    {Unterschiedliche CryptSize ab Version 1.05}
    if (PartCyln^.InfoSect.VerMark <> Marker) or
        (PartCyln^.InfoSect.VerInfo < BMVersionExtension1) then begin
      CryptSize := OldCHKSize;
      CheckSize := OldCHKSize;
    end;
    {Bootmanager Decrypten}
    if (PartCyln^.InfoSect.VerMark <> Marker) or
        (PartCyln^.InfoSect.VerInfo <> BMVersionExtension3) then
      ProtectBM (PartCyln^.CodeSect[StartSect], CryptSize, PartCyln^.InfoSect.MENRndIdx);

    {Checksumme prÅfen!}
    {BUG: Checksummenfehler bei Floppy- oder MasterPa·wort wegen Protect}
    {FIX: Reihenfolge Checksumme/Pa·wîrter geÑndert}
    if PartCyln^.InfoSect.MENChkSum <> CalcCheck (PartCyln^.CodeSect[StartSect], CheckSize) then begin
      MessageBox (BOX_WARN_errBMgr_HDR,
        BOX_WARN + BOX_WARN_errBMgr_CHKS,
        ButtonOK or ButtonDefOK);
    end;

    begin {just to avoid manuel reordering...}
      {AbzÑhlen!}
      {BUG: TatsÑchliche Anzahl von Entries kann abweichen!}
      {FIX: öberprÅfung der Anzahl beim laden der Partitionstabelle
      InfoEntr = RealEntr}
      {BUG: Runtime Error 201: Range Check Overflow
      ...InfoEntr[InfoEntr]... bei InfoEntr = MaxEntr}
      {FIX: Umstellung der Bedingung um Kurzschlu· zu bezwecken}
      while (InfoEntr < MaxEntries) and
          (PartCyln^.InfoSect.InfoEntr[InfoEntr].EntrMark = Marker) do
        Inc (InfoEntr);

      {Anzahl der Entries gÅltig?}
      {BUG: Runtime Error 201 wenn fehlerhafter BootManager (InfoEntr - 1)}
      {FIX: Falls InfoEntr = 0 BootManager deinstallieren}
      if InfoEntr > 0 then begin
        BMIsInst := TRUE;

        BMPrevInst := TRUE;

        CheckMBR; {added 000816-UM}

        TimeOut := PartCyln^.InfoSect.WaitTime;
        if Timeout > TimeoutMax then
          Timeout := TimeoutMax;
        if Timeout <> 0 then
          if SuppBlind then
            if Timeout < TimeoutMinBlind then
              Timeout := TimeoutMinBlind
            else                       {damn pascal syntax!}
          else
            if Timeout < TimeoutMinNoBlind then
              Timeout := TimeoutMinNoBlind;

        TimeHndl := PartCyln^.InfoSect.TimeHndl;
        if not (TimeHndl in [TimeoutInactivate, TimeoutReset, TimeoutIgnore]) then
          TimeHndl := TimeoutInactivate;
        
        ClearScrn := (PartCyln^.InfoSect.ClearScrn = 1);
        BootLast := (PartCyln^.InfoSect.BootLast = 1);
        if BootLast then
          LastEntry := PartCyln^.InfoSect.LastEntry
        else
          LastEntry := 0;
        SimpleMenus := (PartCyln^.InfoSect.SimpleMenus = 1);
        
        {Passwîrter ab Version 1.05}
        if (PartCyln^.InfoSect.VerMark = Marker) and
            (PartCyln^.InfoSect.VerInfo >= BMVersionExtension1) then begin 
          if PartCyln^.InfoSect.MasterPWD.Protection <> 0 then begin 
            ProtectPWD (PartCyln^.InfoSect.MasterPWD.Password, PartCyln^.InfoSect.MasterPWD.Protection);
            StrCopy (MasterPWD, PartCyln^.InfoSect.MasterPWD.Password);
          end;
          if PartCyln^.InfoSect.FloppyPWD.Protection <> 0 then begin 
            ProtectPWD (PartCyln^.InfoSect.FloppyPWD.Password, PartCyln^.InfoSect.FloppyPWD.Protection);
            StrCopy (FloppyPWD, PartCyln^.InfoSect.FloppyPWD.Password);
          end;
          
          BMHasPWDs := TRUE;
          BMPrevVer := PartCyln^.InfoSect.VerInfo;
        end
        else
          BMPrevVer := BMVersionExtension1 - 1;

        If BMPrevVer < BMVersionExtension4 Then BMCleanOldVersion := true;

        {Anzahl der Entries ungÅltig}
      end
      else begin
        MessageBox (BOX_WARN_errBMgr_HDR,
          BOX_WARN + BOX_WARN_errBMgr_INST,
          ButtonOK or ButtonDefOK);
        UnInstallBM;
      end;

      {Checksumme ungÅltig}
    end;
  end;

  HDriv := DriveChain;
  for Cnt := 1 to PhyDrives do begin
    DestroyInfoBox (InfoBox);
    InfoBox := CreateInfoBox (BOX_INFO_rdSyst_HDR,
      BOX_INFO_rdSyst_PAR1 + Int2Str (Cnt, 1) + '.');
    if InfoBox = nil then begin
      MemoryError;
      Exit;
    end;

    {Partitionssektor laden}
    InitReadWriteSectors (FirstDriveIndex + Cnt, INITREAD);
    Res := ReadSectors (FirstDriveIndex + Cnt, 0, 1, PartSect);
    DeInitReadWriteSectors (FirstDriveIndex + Cnt);

    if Res <> 0 then begin
      DestroyInfoBox (InfoBox);
      MessageBox (BOX_WARN_errHDD_HDR,
        BOX_WARN + BOX_WARN_errHDD_rdPTbl,
        ButtonOK or ButtonDefOK);

      dispose (PartSect);
      dispose (PartCyln);
      Exit;
    end;

    {und die DriveChain initialisieren}
    if HDriv = nil then begin
      if MaxAvail < sizeof (TDriveChain) then begin
        DestroyInfoBox (InfoBox);
        MemoryError;
        Exit;
      end;
      New (HDriv);

      DriveChain := HDriv
    end
    else begin
      if MaxAvail < sizeof (TDriveChain) then begin
        DestroyInfoBox (InfoBox);
        MemoryError;
        Exit;
      end;
      New (HDriv^.Next);

      HDriv := HDriv^.Next
    end;

    {BUG: Runtime Error 204 wenn Partitionstabelle ungÅltig}
    {FIX: Initialisierung des Next Pointers}
    {PartChain und Next Pointer initialisieren}
    HDriv^.PartChain := nil;
    HDriv^.Next := nil;
    HDriv^.HasChanged := false;

    {DriveFormat eintragen}
    if not GetDriveFormat (FirstDriveIndex + Cnt, DriveData) then begin
      DestroyInfoBox (InfoBox);
      MemoryError;
      Exit;
    end;
    HDriv^.Drive := FirstDriveIndex + Cnt;
    HDriv^.Head := DriveData^.Heads;
    HDriv^.Sector := DriveData^.Sectors;

    {UM: und hier ein kleiner Trick: TotalSectors kann bei den Int13X-
    Funktionen ein Wert sein, der an einer ungeraden CHS-Adresse endet.
    Beim Anlegen einer neuen Partition endet diese aber immer an einer
    geraden Adresse. Es bleibt also am Ende ein kleiner Bereich unbenutzt.
    Um die Benuter nicht zu verwirren, soll dieser nicht angezeigt werden.
    Wenn man ihn aber kÅnstlich verstecken wÅrde, dann wÑre eine Partition
    evtl. merklich kleiner als der zuvor freie Bereich. Um diese Schwankunen
    zu Ñndern, wird hier einfach Totalsectors abgerundet:}
    if ((DriveData^.Heads + 1) * DriveData^.Sectors) = 0
        {?} then
      HDriv^.Size := DriveData^.TotalSectors
    else
      HDriv^.Size := ( DriveData^.TotalSectors
        div ( (DriveData^.Heads + 1) * DriveData^.Sectors ) )
        * ( (DriveData^.Heads + 1) * DriveData^.Sectors );

    HDriv^.MaxOldBIOSSec := ( (DriveData^.Cylinders + 1)
      * (DriveData^.Heads + 1) * DriveData^.Sectors) - 1;
    HDriv^.MinSize := DriveData^.Sectors * (DriveData^.Heads + 1);

    {Ist die Partitionstabelle gÅltig?}
    if PartSect^.PartMark = Marker then begin
      if MaxAvail < sizeof (TBootSect) then begin
        DestroyInfoBox (InfoBox);
        MemoryError;
        Exit;
      end;
      New (BootSect);

      {Alle EintrÑge durchgehen...}
      for PCnt := 0 to (MaxPartitions - 1) do begin
        {Handelt es sich um eine erweiterte Partition?}
        if (PartSect^.PartEntr[PCnt].PartType = PartTypeExtN) or
            (PartSect^.PartEntr[PCnt].PartType = PartTypeExtX) then begin
          if MaxAvail < 2 * sizeof (TPartSect) then begin
            DestroyInfoBox (InfoBox);
            MemoryError;
            Exit;
          end;
          New (HPSect);
          New (HPSect2);

          Distance := PartSect^.PartEntr[PCnt].Distance;
          VPos := Distance;
          {UM: entspricht PartSect^.PartEntr[PCnt].StartPos}
          
          {Alle Logischen Laufwerke einlesen}
          repeat
            MoreVolumes := FALSE;
            
            {Logischer Partitionssektor}
            InitReadWriteSectors (FirstDriveIndex + Cnt, INITREAD);
            Res := ReadSectors (FirstDriveIndex + Cnt, VPos, 1, HPSect);
            DeInitReadWriteSectors (FirstDriveIndex + Cnt);
            
            if Res <> 0 then begin 
              DestroyInfoBox (InfoBox);
              MessageBox (BOX_WARN_errHDD_HDR, 
                BOX_WARN + BOX_WARN_errHDD_rdExtP,
                ButtonOK or ButtonDefOK);
              
              dispose (HPSect);
              dispose (BootSect);
              dispose (PartSect);
              dispose (PartCyln);
              Exit;
            end;
            
            {Ist der Partitionssektor gÅltig?}
            if HPSect^.PartMark = Marker then begin 
              {Ist der Erste Eintrag der Link oder das logische Laufwerk?}
              if (HPSect^.PartEntr[0].PartType = PartTypeExtN) or
                  (HPSect^.PartEntr[0].PartType = PartTypeExtX) then
                ECnt := 1
              else
                ECnt := 0;
              
              {Logisches Laufwerk}
              if HPSect^.PartEntr[ECnt].PartType <> 0 then begin 
                {Wenn der erste Eintrag, dann PartChain anlegen}
                if HDriv^.PartChain = nil then begin 
                  if MaxAvail < sizeof (TPartChain) then begin 
                    DestroyInfoBox (InfoBox);
                    MemoryError;
                    Exit;
                  end;
                  New (WPart);

                  HDriv^.PartChain := WPart;
                  WPart^.Next := nil
                end
                else begin
                  WPart := HDriv^.PartChain;
                  {Gehîrt der Eintrag vor den ersten?}
                  if Distance < WPart^.Distance then begin 
                    HPart := WPart;
                    if MaxAvail < sizeof (TPartChain) then begin 
                      DestroyInfoBox (InfoBox);
                      MemoryError;
                      Exit;
                    end;
                    New (WPart);
                    
                    WPart^.Next := HPart;
                    HDriv^.PartChain := WPart;
                  end
                  else begin 
                    repeat
                      HPart := WPart;
                      WPart := WPart^.Next;
                      DPE := WPart;
                    until (Distance < WPart^.Distance) or (WPart = nil) ;
                    
                    if MaxAvail < sizeof (TPartChain) then begin 
                      DestroyInfoBox (InfoBox);
                      MemoryError;
                      Exit;
                    end;
                    New (WPart);

                    WPart^.Next := DPE;
                    HPart^.Next := WPart;
                  end;
                end;

                WPart^.PartName[0] := #0;
                WPart^.PartLabl[0] := #0;
                WPart^.PartPWD[0] := #0;
                WPart^.KnowsPass := FALSE;

                WPart^.PartSyst := PartSystNone;
                WPart^.PartEntr := PartEntrNone;

                WPart^.PartPos := PCnt;
                
                WPart^.IsNew := IsNew_No;
                WPart^.PartStat := PartStatLog;
                WPart^.PartType := HPSect^.PartEntr[ECnt].PartType;
                WPart^.PartDriv := PartDrivNone;
                for CI := 1 to MaxSystems do
                  if HPSect^.PartEntr[Ecnt].PartType = IPartType[CI] then
                    WPart^.PartSyst := CI;
                
                WPart^.Distance := Distance;
                WPart^.StartSec := Distance + HPSect^.PartEntr[ECnt].Distance;
                
                {Auf Distance Fehler prÅfen:
                Distace-Wert sollte relativ zum Partitionssektor der erw.
                Partition sein, ist aber manchmal fÑlschlich Absolutadresse.
                Falls CHS-Adressen gueltig, ist das einfach: }

                LBA2CHS(HDriv^.Drive, C, H, S, WPart^.Distance);

                if WPart^.Distance < HDriv^.MaxOldBIOSSec then begin
                  HeadSecZyl2LBA (HDriv^.Drive, HPSect^.PartEntr[ECnt].StartPos, SPos);

                  if WPart^.StartSec <> SPos then begin
                    WPart^.PartSize := WPart^.PartSize + (WPart^.StartSec - SPos);
                    WPart^.StartSec := SPos;
                    HDriv^.HasChanged := TRUE;
writeln('corrected absolute adressing in extended partition (CHS)');
{Write (STR_PROG_WAITKEY);}
{ReadKey;}
                  end;
                end
                else begin
                  {CHS-Adressen sind Platzhalter }
                  { wenn .Distance Absolutadresse ist, dann ist
                  PartEntr.Distance > StartPos (Variable: Distance, urks)}
                  if (HPSect^.PartEntr[ECnt].Distance > Distance)
                      { angenommen, es ist Absolutadresse, dann berechnen
                      wir mal die Grî·e der erw. Partition bis einschl. diesem
                      Laufwerk und vergl. mit Angabe im Partitionssektor }
                      and (HPSect^.PartEntr[ECnt].Distance
                      + HPSect^.PartEntr[ECnt].PartSize
                      - PartSect^.PartEntr[PCnt].Distance + 1
                      <= PartSect^.PartEntr[PCnt].PartSize) then begin
                    { hier wahrscheinlich Absolutdistanz }
                    { und letzte HÅrde: }
                    WPart^.StartSec := HPSect^.PartEntr[ECnt].Distance;
                    HDriv^.HasChanged := TRUE;
writeln('corrected absolute adressing in extended partition (LBA)');
{Write (STR_PROG_WAITKEY);
ReadKey;}
                  end;
                end;

                {Gesamtgrî·e des log. Laufwerks + Partitionsteil}
                WPart^.PartSize := HPSect^.PartEntr[ECnt].PartSize +
                  (WPart^.StartSec - WPart^.Distance);

                {Im MenÅsystem suchen...}
                if BMIsInst then
                  for CI := 0 to InfoEntr - 1 do begin
                    {Stimmen alle Angaben Åberein?}
                    if (PartCyln^.InfoSect.InfoEntr[CI].BootDriv = HDriv^.Drive) and
                        {UM: bei LBA nur noch .Distance prÅfen, log LW: InfoEntr.Distance = PartEntr.StartSec}
                        (PartCyln^.InfoSect.InfoEntr[CI].Distance = WPart^.StartSec) then begin
                      strcopy (WPart^.PartName, PartCyln^.InfoSect.InfoEntr[CI].PartName);
                      
                      {Anzahl der Entries ÅberprÅfen}
                      Inc (RealEntr);
                      
                      {Passwîrter an Version 1.05}
                      if (PartCyln^.InfoSect.VerMark = Marker) and
                          (PartCyln^.InfoSect.VerInfo >= BMVersionExtension1) then begin 
                        if PartCyln^.InfoSect.PWDEntr[CI].Protection <> 0 then begin
                          ProtectPWD (PartCyln^.InfoSect.PWDEntr[CI].Password, PartCyln^.InfoSect.PWDEntr[CI].Protection);
                          StrCopy (WPart^.PartPWD, PartCyln^.InfoSect.PWDEntr[CI].Password);
                          WPart^.KnowsPass := FALSE;
                        end;
                      end;
                      
                      WPart^.PartEntr := CI;
                    end;
                  end;
                
                
                {Label im Bootsektor lesen}
                VPos := WPart^.StartSec;

                InitReadWriteSectors (FirstDriveIndex + Cnt, INITREAD);
                Res := ReadSectors (FirstDriveIndex + Cnt, VPos, 1, BootSect);
                DeInitReadWriteSectors (FirstDriveIndex + Cnt);
                
                {auskommentiert: Build 324; jetzt kein Programmabbruch mehr!
                if Res <> 0 then begin
                  DestroyInfoBox (InfoBox);
                  MessageBox (BOX_WARN_errHDD_HDR,
                    BOX_WARN + BOX_WARN_errHDD_rdBSec,
                    ButtonOK or ButtonDefOK);

                  dispose (HPSect2);
                  dispose (HPSect);
                  dispose (BootSect);
                  dispose (PartSect);
                  dispose (PartCyln);
                  Exit;
                end;}

                {Label aus dem Bootsektor kopieren}
                if (res=0) and (BootSect^.PartMark = Marker)
                   and (BootSect^.PartLabl[CI] > 33)
                   and (BootSect^.PartLabl[CI] < 128)
                then begin
                  for CI := 0 to 10 do
                    WPart^.PartLabl[CI] := Chr (BootSect^.PartLabl[CI]);

                  WPart^.PartLabl[11] := #0;
                end;
              end;
              
              {Noch ein Eintrag vorhanden? Keine Backlinks erlauben!}
              if (HPSect^.PartEntr[1 - ECnt].PartType = PartTypeExtN) or
                  (HPSect^.PartEntr[1 - ECnt].PartType = PartTypeExtX) then begin 
                if HPSect^.PartEntr[1 - ECnt].Distance + PartSect^.PartEntr[PCnt].Distance > Distance then begin
                  MoreVolumes := TRUE;
                  Distance := PartSect^.PartEntr[PCnt].Distance + HPSect^.PartEntr[1 - ECnt].Distance;
                  VPos := Distance;
                  {UM: = HPSect^.PartEntr[1 - ECnt].StartPos}
                end;
              end;
            end;
          until (Res <> 0) or not MoreVolumes ;
          dispose (HPSect2);
          dispose (HPSect);

          {Es ist eine primÑre Partition}
        end
        else if PartSect^.PartEntr[PCnt].PartType <> 0 then begin
          {Wenn der erste Eintrag, dann PartChain anlegen}
          if HDriv^.PartChain = nil then begin
            if MaxAvail < sizeof (TPartChain) then begin
              DestroyInfoBox (InfoBox);
              MemoryError;
              Exit;
            end;
            New (WPart);

            HDriv^.PartChain := WPart;
            WPart^.Next := nil
          end
          else begin
            WPart := HDriv^.PartChain;
            HPart := WPart;

            {Gehîrt der Eintrag vor den ersten?}
            if PartSect^.PartEntr[PCnt].Distance < WPart^.Distance then begin
              if MaxAvail < sizeof (TPartChain) then begin
                DestroyInfoBox (InfoBox);
                MemoryError;
                Exit;
              end;
              New (WPart);

              WPart^.Next := HPart;
              HDriv^.PartChain := WPart;
            end
            else begin
              repeat
                HPart := WPart;
                WPart := WPart^.Next;
                DPE := WPart;
              until (PartSect^.PartEntr[PCnt].Distance < WPart^.Distance) or (WPart = nil) ;

              if MaxAvail < sizeof (TPartChain) then begin
                DestroyInfoBox (InfoBox);
                MemoryError;
                Exit;
              end;
              New (WPart);

              WPart^.Next := DPE;
              HPart^.Next := WPart;
            end;
          end;

          WPart^.PartName[0] := #0;
          WPart^.PartLabl[0] := #0;
          WPart^.PartPWD[0] := #0;
          WPart^.KnowsPass := FALSE;

          WPart^.PartSyst := PartSystNone;
          WPart^.PartEntr := PartEntrNone;

          WPart^.PartPos := PCnt;

          WPart^.IsNew := IsNew_No;
          WPart^.PartStat := PartSect^.PartEntr[PCnt].PartStat;
          WPart^.PartType := PartSect^.PartEntr[PCnt].PartType;
          WPart^.PartDriv := PartDrivNone;
          for CI := 1 to MaxSystems do
            if PartSect^.PartEntr[PCnt].PartType = IPartType[CI] then
              WPart^.PartSyst := CI;

          WPart^.PartSize := PartSect^.PartEntr[PCnt].PartSize;
          WPart^.Distance := PartSect^.PartEntr[PCnt].Distance;
          WPart^.StartSec := PartSect^.PartEntr[PCnt].Distance;
          
          {Im MenÅsystem nachsehen...}
          if BMIsInst then
            for CI := 0 to InfoEntr - 1 do begin 
              {Stimmen alle Angaben Åberein?}
              if (PartCyln^.InfoSect.InfoEntr[CI].BootDriv = HDriv^.Drive) and
                  {UM: bei LBA nur noch .Distance prÅfen, log LW: InfoEntr.Distance = PartEntr.StartSec}
                  (PartCyln^.InfoSect.InfoEntr[CI].Distance = WPart^.Distance) then begin
                strcopy (WPart^.PartName, PartCyln^.InfoSect.InfoEntr[CI].PartName);

                {Anzahl der Entries ÅberprÅfen}
                Inc (RealEntr);
                
                {Passwîrter an Version 1.05}
                if (PartCyln^.InfoSect.VerMark = Marker) and
                    (PartCyln^.InfoSect.VerInfo >= BMVersionExtension1) then begin 
                  if PartCyln^.InfoSect.PWDEntr[CI].Protection <> 0 then begin
                    ProtectPWD (PartCyln^.InfoSect.PWDEntr[CI].Password, PartCyln^.InfoSect.PWDEntr[CI].Protection);
                    StrCopy (WPart^.PartPWD, PartCyln^.InfoSect.PWDEntr[CI].Password);
                    WPart^.KnowsPass := FALSE;
                  end;
                end;
                
                WPart^.PartEntr := CI;
              end;
            end;
          
          VPos := WPart^.StartSec;
          
          {System im Bootsektor nachsehen}
          InitReadWriteSectors (FirstDriveIndex + Cnt, INITREAD);
          Res := ReadSectors (FirstDriveIndex + Cnt, VPos, 1, BootSect);
          DeInitReadWriteSectors (FirstDriveIndex + Cnt);
          
          {auskommentiert: Build 324; jetzt kein Programmabbruch mehr!
          if Res <> 0 then begin
            DestroyInfoBox (InfoBox);
            MessageBox (BOX_WARN_errHDD_HDR,
              BOX_WARN + BOX_WARN_errHDD_rdBSec,
              ButtonOK or ButtonDefOK);

            dispose (BootSect);
            dispose (PartSect);
            dispose (PartCyln);
            Exit;
          end;}

          {Label aus dem Bootsektor kopieren}
          if (Res = 0) and (BootSect^.PartMark = Marker)
             and (BootSect^.PartLabl[CI] > 33)
             and (BootSect^.PartLabl[CI] < 128)
          then begin
            for CI := 0 to 10 do
              WPart^.PartLabl[CI] := Chr (BootSect^.PartLabl[CI]);

            WPart^.PartLabl[11] := #0;
          end;
        end;
      end;

      dispose (BootSect);
      
      {Partitionstabelle ist nicht gÅltig}
    end
    else if not WriteMBR (HDriv^.Drive) then
      MessageBox (BOX_WARN_errHDD_HDR,
        BOX_WARN + BOX_WARN_errHDD_wrtMBR, 
        ButtonOK or ButtonDefOK);
  end;

  {Anzahl der Entries ÅberprÅfen}
  if InfoEntr <> RealEntr then
    MessageBox (BOX_WARN_errBMgr_HDR, 
      BOX_WARN + BOX_WARN_errBMgr_ENTR, 
      ButtonOK or ButtonDefOK);
  
  if not CleanUpChain (DriveChain) then ExitProgram (FAILURE);
  
  DestroyInfoBox (InfoBox);
  
  dispose (PartSect);
  dispose (PartCyln);

  LoadTable := TRUE;
end;

{Bootmanager installieren/aktualisieren/deinstallieren}
function MenuWriteBM :boolean;
var
  InfoBox           :pointer;
  result            :boolean;
begin
  MenuWriteBM := FALSE;

  InfoBox := CreateInfoBox (BOX_INFO_wrtChng_HDR, BOX_INFO_wrtChng);
  if InfoBox = nil then begin
    MemoryError;
    Exit;
  end;

  if BMPrevInst then begin {ist schon installiert}
    if BMIsInst then {...und soll installiert bleiben}
      if WriteBM or BMUpdate then {...und mu· aktualisiert werden}
        result := UpdateBM
      else
    else {und soll nicht wieder installiert werden}
      result := UninstallBM;
  end else if BMIsInst then
    result := InstallBM; {also Erstinstallaion}

  DestroyInfoBox (InfoBox);
  MenuWriteBM := TRUE;
end;

{Partitionstabelle Schreiben}
function WriteTable(HDriv :PDriveChain):boolean;
var
  PartCyln          :PPartCyln;      {Temp. Partitionszylidnerstruktur}
  PartSect          :PPartSect;      {Temp. Partitionssektor (log. LW)}
  BootSect          :PBootSect;      {Leerer Bootsektor}
  VPos, IPos, SPos  :Longint;{UM: LBA} {Temp. Positionsstruktur, Erweiterte Part.}
  HPart, TPart      :PPartChain;     {PartitionChain}
  Res               :integer;        {FehlerzÑhler und Result}
  Pos, Cnt          :integer;        {InfoEntr.Pos (Fkt.glob.), PartSect.Pos}
  LastLogEnd, ISize :longint;        {Temp. Grî·e der erw. Partition}
  CH, CC, CS        :longint;{changed, UM}
  DoExit            :boolean;
  IsNew             :byte;
  InfoBox           :pointer;

  procedure rawformat(driv: PDriveChain; vpos, size: longint; full: boolean);
  {Bootsektor schreiben, Partition "formatieren"
   Ergebnis in "Res"
   990411-UM in Prozedor ausgelagert, da an zwei Stellen benutzt
             Schreiben von "FAT-Sector" hinzugefuegt
   000813-UM Mode "full" initialisiert gesamte Partition
  }
  var
    CH, CC, CS :longint;
    HCnt,start :longint;
    towrite    :word;
    buf        :pointer;
    p          :byte;
    todo       :longint;
    oldx, oldy :byte;

    procedure update(todo, done: longint);
    var t, d :longint;
        pnew :byte;
    begin
      t := todo div 100;
      d := done div 100;

      if t = 0 then
        exit
      else begin
        pnew := ((t - d) * 100) div t;

        if pnew <> p then begin
          p := pnew;
          gotoxy(25, oldy + 1);
          write(p:3, '%');
        end;
      end;
    end;

  begin
    LBA2CHS (Driv^.Drive, CH, CC, CS, VPos);

    InitReadWriteSectors (Driv^.Drive, INITWRITE);

    p:=101;
    oldx := wherex;
    oldy := wherey; {dieses MenÅsystem ist grausam}
    if full then begin
      getmem(buf, 32768);
      fillchar(buf^, 32768, EmptyBootSect);

      start := vpos;
      todo := size;
      while size > 0 do begin
        update(todo, size);

        if size > 64 then towrite := 64
        else towrite := size;
        size := size - towrite;

        Res := WriteSectors (Driv^.Drive, start, towrite, buf);

        start := start + towrite;
      end;

      freemem(buf, 32768);
    end else begin
      HCnt := Size div Driv^.Sector;
      HCnt := HCnt div 256 + 1;

      todo := HCnt;
      repeat
        update(ToDo, HCnt);

        CHS2LBA (Driv^.Drive, CH, CC, 1, VPos);
        Res := WriteSectors (Driv^.Drive, VPos, 1, BootSect);
        CHS2LBA (Driv^.Drive, CH, CC, 7, VPos);
        if Res = 0 then
          Res := WriteSectors (Driv^.Drive, VPos, 1, BootSect);

        Inc (CH);
        if CH > Driv^.Head then begin
          Inc (CC);
          CH := 0;
        end;

        Dec (HCnt);
      until (Res <> 0) or (HCnt = 0) ;
    end;

    DeInitReadWriteSectors (Driv^.Drive);
    gotoxy(oldx, oldy);
  end;

begin
  WriteTable := FALSE;

  InfoBox := CreateInfoBox (BOX_INFO_wrtChng_HDR, BOX_INFO_wrtChng);
  if InfoBox = nil then begin
    MemoryError;
    Exit;
  end;

  if MaxAvail < (sizeof (TPartCyln) + sizeof (TPartSect) + sizeof (TBootSect)) then begin
    DestroyInfoBox (InfoBox);
    MemoryError;
    Exit;
  end;
  New (PartCyln);
  New (PartSect);
  New (BootSect);

  FillChar (BootSect^, 512, EmptyBootSect);

  {former: repeat}
    VPos := 0;

    DestroyInfoBox (InfoBox);
    InfoBox := CreateInfoBox (BOX_INFO_wrtChng_HDR,
      BOX_INFO_wrtChng_PAR1 + Int2Str (HDriv^.Drive - FirstDriveIndex, 1) + '.');
    if InfoBox = nil then begin
      MemoryError;
      Exit;
    end;

    InitReadWriteSectors (HDriv^.Drive, INITREAD);
    Res := ReadSectors (HDriv^.Drive, VPos, 1, PartCyln);
    DeInitReadWriteSectors (HDriv^.Drive);

    if Res = 0 then begin
      {Puffer aufrÑumen...}
      FillChar (PartCyln^.PartSect.PartEntr[0], 64, 0);

      {...und die Variablen initialisieren}
      ISize := 0;

      HPart := HDriv^.PartChain;
      {Und jetzt die Partitionstabelle neu schreiben...}
      while HPart <> nil do begin
        Cnt := HPart^.PartPos;
        {Ist es ein logisches Laufwerk?}
        if HPart^.PartStat = PartStatLog then begin
          {Dann erstmal die Position in der Partitionstabelle festhalten}
          IPos := HPart^.Distance;     {UM: =HPart^.StartPos}
          {IDist removed, das ist ja sonst total verwirrent :-)}
          
          DoExit := FALSE;

          repeat
            LastLogEnd := HPart^.Distance + HPart^.PartSize - 1;
            {hier endet das letze belegte(!) logische Laufwerk; 990508-UM}

            ISize := ISize + HPart^.PartSize;
            VPos := HPart^.Distance;   {UM: erw. Part, =HPart^.StartPos}
            IsNew := HPart^.IsNew;
            
            {Erweiterte Tabellen laden und ausfÅllen}
            InitReadWriteSectors (HDriv^.Drive, INITREAD);
            Res := ReadSectors (HDriv^.Drive, VPos, 1, PartSect);
            DeInitReadWriteSectors (HDriv^.Drive);

            if Res <> 0 then begin 
              DestroyInfoBox (InfoBox);
              MessageBox (BOX_WARN_errHDD_HDR,
                BOX_WARN + BOX_WARN_errHDD_rdExtP,
                ButtonOK or ButtonDefOK);

              dispose (BootSect);
              dispose (PartSect);
              dispose (PartCyln);
              Exit;
            end;

            {Partitionstabelle lîschen}
            if (IsNew <> IsNew_No) or (PartSect^.PartMark <> Marker) then
              FillChar (PartSect^, 512, 0)
            else
              FillChar (PartSect^.PartEntr[0], 64, 0);

            PartSect^.PartEntr[0].PartStat := PartStatAct;

            {Abstand ExtPart -> logLW ermitteln}
            LBA2HeadSecZyl (HDriv^.Drive, PartSect^.PartEntr[0].StartPos,
              HPart^.StartSec, PQMagicComp);
            LBA2HeadSecZyl (HDriv^.Drive, PartSect^.PartEntr[0].EndPos,
              HPart^.Distance + HPart^.PartSize - 1, PQMagicComp);
            {Partsize wird mit Distance berechnet, nicht mit StartSec}
            PartSect^.PartEntr[0].PartType := HPart^.PartType;
            PartSect^.PartEntr[0].Distance := HPart^.StartSec - HPart^.Distance;
            PartSect^.PartEntr[0].PartSize := HPart^.PartSize - PartSect^.PartEntr[0].Distance;

            SPos := HPart^.StartSec;
            {Startposition fuers Formatieren}

            {UM: removed handling of BootManager MenÅ}

            {Freie Bereiche Åberspringen, falls sie Status 64 haben (nur log. Laufwerk)}
            while (HPart^.Next <> nil) and
                (HPart^.Next^.PartStat = PartStatFreeLog) do begin 
              ISize := ISize + HPart^.PartSize;
              {added, 990503-UM}
              HPart := HPart^.Next;
            end;

            if (HPart^.Next <> nil) and (HPart^.Next^.PartStat = PartStatLog) then begin 
              HPart := HPart^.Next;

              PartSect^.PartEntr[1].PartStat := PartStatPri;
              PartSect^.PartEntr[1].PartType := PartTypeExtN;
              {990411-UM: ist wohl doch fix}
              
              {orig.:
              PartSect^.PartEntr[1].StartPos := HPart^.StartPos;
              PartSect^.PartEntr[1].EndPos := HPart^.EndPos;}
              LBA2HeadSecZyl (HDriv^.Drive, PartSect^.PartEntr[1].StartPos,
                HPart^.Distance, PQMagicComp);
              LBA2HeadSecZyl (HDriv^.Drive, PartSect^.PartEntr[1].EndPos,
                HPart^.Distance + HPart^.PartSize - 1, PQMagicComp);
              
              PartSect^.PartEntr[1].Distance := HPart^.Distance - IPos;
              PartSect^.PartEntr[1].PartSize := HPart^.PartSize;
            end
            else
              DoExit := TRUE;
            
            PartSect^.PartMark := Marker;

            if HDriv^.HasChanged then begin
              {BUG: erst Bootsektor ab VPos, dann PartSec auf VPos schreiben}
              {Bootsektor schreiben}
              {BUG: Partitionen wurden nicht richtig formatiert}
              {FIX: n div 256 + 1 Tracks lîschen (n = Grî·e der Partition)}

              if (IsNew = IsNew_Init) or (IsNew = IsNew_Full) then begin
                rawformat (HDriv, VPos, PartSect^.PartEntr[0].PartSize,
                  IsNew = IsNew_Full);

                if Res <> 0 then begin 
                  DestroyInfoBox (InfoBox);
                  MessageBox (BOX_WARN_errHDD_HDR,
                    BOX_WARN + BOX_WARN_errHDD_wrtBSec, 
                    ButtonOK or ButtonDefOK);
                  
                  dispose (BootSect);
                  dispose (PartSect);
                  dispose (PartCyln);
                  Exit;
                end;
              end;

              {erweiterte Partitionstabelle schreiben}
              InitReadWriteSectors (HDriv^.Drive, INITWRITE);
              Res := WriteSectors (HDriv^.Drive, VPos, 1, PartSect);
              DeInitReadWriteSectors (HDriv^.Drive);

              if Res <> 0 then begin
                DestroyInfoBox (InfoBox);
                MessageBox (BOX_WARN_errHDD_HDR, 
                  BOX_WARN + BOX_WARN_errHDD_wrtExtP, 
                  ButtonOK or ButtonDefOK);

                dispose (BootSect);
                dispose (PartSect);
                dispose (PartCyln);
                Exit;
              end;
            end;
          until DoExit ;

          {Eintrag in die Haupttabelle}
          PartCyln^.PartSect.PartEntr[Cnt].PartStat := PartStatPri;
          if (LastLogEnd <= HDriv^.MaxOldBIOSSec) or NoWin9xPTyp then
            {990411-UM}
            PartCyln^.PartSect.PartEntr[Cnt].PartType := PartTypeExtN
          else
            PartCyln^.PartSect.PartEntr[Cnt].PartType := PartTypeExtX;

          {UM: orig.:
          PartCyln^.PartSect.PartEntr[Cnt].StartPos := IPos;
          PartCyln^.PartSect.PartEntr[Cnt].EndPos := HPart^.EndPos;
          }
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.PartSect.PartEntr[Cnt].StartPos,
            IPos, PQMagicComp);
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.PartSect.PartEntr[Cnt].EndPos,
            LastLogEnd, PQMagicComp);

          PartCyln^.PartSect.PartEntr[Cnt].Distance := IPos;
          PartCyln^.PartSect.PartEntr[Cnt].PartSize := LastLogEnd - IPos + 1;

          Inc (cnt);

          {Ist es eine primÑre Partition?}
        end
        else if HPart^.PartStat in [PartStatPri, PartStatAct] then begin
          IsNew := HPart^.IsNew;

          {Eintrag in die Haupttabelle PrimÑre Partition}
          PartCyln^.PartSect.PartEntr[Cnt].PartStat := HPart^.PartStat;
          PartCyln^.PartSect.PartEntr[Cnt].PartType := HPart^.PartType;

          {UM: orig.:
          PartCyln^.PartSect.PartEntr[Cnt].StartPos := HPart^.StartPos;
          PartCyln^.PartSect.PartEntr[Cnt].EndPos := HPart^.EndPos;
          }
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.PartSect.PartEntr[Cnt].StartPos,
            HPart^.Distance, PQMagicComp);
          LBA2HeadSecZyl (HDriv^.Drive, PartCyln^.PartSect.PartEntr[Cnt].EndPos,
            HPart^.Distance + HPart^.PartSize - 1, PQMagicComp);

          PartCyln^.PartSect.PartEntr[Cnt].Distance := HPart^.Distance;
          PartCyln^.PartSect.PartEntr[Cnt].PartSize := HPart^.PartSize;
          
          {UM: removed handling of BootManager MenÅ}

          {Bootsektor schreiben}
          {BUG: Partitionen wurden nicht richtig formatiert}
          {FIX: n div 256 + 1 Tracks lîschen (n = Grî·e der Partition)}
          if (IsNew = IsNew_Init) or (IsNew = IsNew_Full) then begin
            rawformat (HDriv, PartCyln^.PartSect.PartEntr[Cnt].Distance,
              PartCyln^.PartSect.PartEntr[Cnt].PartSize, IsNew = IsNew_Full);
            
            if Res <> 0 then begin
              DestroyInfoBox (InfoBox);
              MessageBox (BOX_WARN_errHDD_HDR,
                BOX_WARN + BOX_WARN_errHDD_wrtBSec, 
                ButtonOK or ButtonDefOK);
              
              dispose (BootSect);
              dispose (PartSect);
              dispose (PartCyln);
              Exit;
            end;
          end;

        end;

        HPart := HPart^.Next
      end

    end
    else begin
      DestroyInfoBox (InfoBox);
      MessageBox (BOX_WARN_errHDD_HDR, 
        BOX_WARN + BOX_WARN_errHDD_rdPTbl, 
        ButtonOK or ButtonDefOK);

      dispose (BootSect);
      dispose (PartSect);
      dispose (PartCyln);
      Exit;
    end;

    {BUG: falsche Partitionstabellensortierung konnte BM durcheinanderbringen}
    {FIX: Partitionstabelle auch schreiben, wenn nur der BootManager
    installiert/upgedated wird}
    if (HDriv^.HasChanged) or ((BMIsInst) and (WriteBM)) then begin
      {Partitionstabelle schreiben...}
      InitReadWriteSectors (HDriv^.Drive, INITWRITE);
      Res := WriteSectors (HDriv^.Drive, 0, 1, PartCyln);
      DeInitReadWriteSectors (HDriv^.Drive);
      
      if Res <> 0 then begin 
        DestroyInfoBox (InfoBox);
        MessageBox (BOX_WARN_errHDD_HDR, 
          BOX_WARN + BOX_WARN_errHDD_wrtPTbl, 
          ButtonOK or ButtonDefOK);

        dispose (BootSect);
        dispose (PartSect);
        dispose (PartCyln);
        Exit;
      end;
    end;

  {former: until...}

  DestroyInfoBox (InfoBox);

  dispose (BootSect);
  dispose (PartSect);
  dispose (PartCyln);
  WriteTable := TRUE;
end;


{Eintrag anzeigen}
procedure DisplayEntry (Entry: PPartChain; Pos: integer; HI: boolean);
begin
  if HI then
    if CRTIsMono then
      if Entry^.IsNew <> IsNew_No then
        SetColor (MonSelNewVG, MonSelBG)
      else if BootLast and (Entry^.PartEntr = LastEntry) then
        SetColor (MonSelThisVG, MonSelBG)
      else
        SetColor (MonSelItemVG, MonSelBG)
    else
      if Entry^.IsNew <> IsNew_No then
        SetColor (ColSelNewVG, ColSelBG)
    else if BootLast and (Entry^.PartEntr = LastEntry) then
      SetColor (ColSelThisVG, ColSelBG)
    else
      SetColor (ColSelItemVG, ColSelBG)
  else if CRTIsMono then
    if Entry^.IsNew <> IsNew_No then
      SetColor (MonWinNewVG, MonWinBG)
    else if BootLast and (Entry^.PartEntr = LastEntry) then
      SetColor (MonWinThisVG, MonWinBG)
    else
      SetColor (MonWinItemVG, MonWinBG)
  else
    if Entry^.IsNew <> IsNew_No then
      SetColor (ColWinNewVG, ColWinBG)
  else if BootLast and (Entry^.PartEntr = LastEntry) then
    SetColor (ColWinThisVG, ColWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG);

  GotoXY (1, Pos + 1);
  ClrEOL;
  if Entry^.PartEntr <> PartEntrNone then
    Write (Entry^.PartEntr: 2, ' ', Entry^.PartName);

  GotoXY (21, Pos + 1);

  if Entry^.PartDriv > 0 then
    Write (Chr (66 + Entry^.PartDriv), ':');

  GotoXY (25, Pos + 1);

  if Entry^.PartStat = PartStatLog then
    Write (WIN_PART_NA)
  else if (Entry^.PartStat = PartStatPri) or (Entry^.PartStat = PartStatAct) then
    if Entry^.PartType in [$11, $14, $16, $17, $1B, $1C, $1E] then
      Write (WIN_PART_HID)
    else if Entry^.PartStat = PartStatAct then
      Write (WIN_PART_ACT)
    else
      Write (WIN_PART_DACT)
  else
    Write (WIN_PART_FREE);

  GotoXY (36, Pos + 1);
  case Entry^.PartStat of
    PartStatLog: begin
      Write (WIN_PART_LOG);
    end;

    PartStatUnusable: begin
      Write (WIN_PART_NA);
    end;
    
    PartStatFreePri: begin 
      Write (WIN_PART_PRI);
    end;

    PartStatFreePriLog: begin 
      Write (WIN_PART_ANY);
    end;
    
    PartStatFreeLog: begin
      Write (WIN_PART_LOG);
    end;
    else
      Write (WIN_PART_PRI);
  end;
  
  GotoXY (45, Pos + 1);
  Write (round (Entry^.PartSize / 2048): 5, 'MB');

{write(Entry^.PartDriv + FirstDriveIndex:5, '  ');
alt:HPart^.PartDriv + LogStart + FirstDriveIndex
neu:HPart^.PartDriv + FirstDriveIndex}

  GotoXY (54, Pos + 1);
  if Entry^.PartSyst <> PartSystNone then
    Write (SPartType[Entry^.PartSyst])
  else if Entry^.PartStat in [PartStatPri, PartStatAct, PartStatLog] then
    Write (WIN_PART_NA, '(', WIN_PART_TYPE, ' ', Byte2Hex (Entry^.PartType), ')')
  else
    Write (WIN_PART_NA);
  
  GotoXY (68, Pos + 1);
  if Entry^.PartType in [$01, $04, $06, $07, $0E, $11, $14, $16, $17, $1E] then
    Write (Entry^.PartLabl)
  else
    Write (WIN_PART_NA);
  
  GotoXY (1, Pos + 1);
end;

{Partitionstabelle anzeigen}
procedure DisplayTable (Drive, Offset: integer);
var
  HDriv :PDriveChain;
  WPart :PPartChain;
  PCnt  :integer;
begin
  Window (1, 2, 80, 24);
  if CRTIsMono then
    SetColor (MonWinItemVG, MonWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG);
  
  GotoXY (3, 1);
  Write (' ', WIN_DRIVE_HDR, ' ', Drive - FirstDriveIndex, ' ');

  Window (2, 5, 79, 23); ClrScr;
  HDriv := DriveChain;
  while (HDriv^.Drive <> Drive) and (HDriv <> nil) do
    HDriv := HDriv^.Next;
  
  WPart := HDriv^.PartChain; PCnt := 0;
  while (PCnt < Offset) and (WPart <> nil) do begin 
    WPart := WPart^.Next;
    Inc (PCnt)
  end;
  
  PCnt := 0;
  while (PCnt < 19) and (WPart <> nil) do begin 
    DisplayEntry (WPart, PCnt, FALSE);
    WPart := WPart^.Next;
    Inc (PCnt)
  end
end;

{EintrÑge in eine Tabelle schreiben und Anzahl zurÅck}
function GetEntries (Drive: integer; var PTable: TPTable) :integer;
var
  Cnt   :integer;
  HDriv :PDriveChain;
  HPart :PPartChain;
begin
  Cnt := 0;
  HDriv := DriveChain;
  while HDriv^.Drive <> Drive do
    HDriv := HDriv^.Next;
  
  HPart := HDriv^.PartChain;
  while HPart <> nil do begin 
    PTable[Cnt] := HPart;
    HPart := HPart^.Next;
    Inc (Cnt);
  end;
  
  GetEntries := Cnt
end;

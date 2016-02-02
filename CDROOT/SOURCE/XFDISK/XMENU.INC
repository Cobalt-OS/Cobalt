{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XMENU.INC
    Main Menu

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

function GetDriveFromNumber (Drive: integer) :PDriveChain;
var
  HDriv :PDriveChain;
begin
  HDriv := DriveChain;
  while (HDriv^.Drive <> Drive) and (HDriv <> nil) do
    HDriv := HDriv^.Next;
  GetDriveFromNumber := HDriv;
end;

function GetPartFromPos (Drive, Pos: integer) :PPartChain;
var
  HDriv :PDriveChain;
  HPart :PPartChain;
  Cnt   :integer;
begin
  HDriv := GetDriveFromNumber (Drive);
  
  if HDriv <> nil then begin 
    HPart := HDriv^.PartChain;
    Cnt := 0;
    while (Cnt < Pos) and (HPart <> nil) do begin 
      HPart := HPart^.Next;
      Inc (Cnt)
    end;
  end
  else
    HPart := nil;
  GetPartFromPos := HPart;
end;

{$IFOPT F-}
{$DEFINE NOFARCALLS}
{$F+}
{$ENDIF}

function get_isnew(var IsNew: Byte): Boolean;
var res : byte;
    ret : boolean;
begin
  ret := false;

  Res := MessageBox (BOX_QUERY_format_HDR, BOX_QUERY + BOX_QUERY_format,
    ButtonYesNoCancel or ButtonDefYes);

  If Res = ButtonResNo Then Begin
    IsNew := IsNew_Yes;
    ret := True;
  End Else If Res = ButtonResYes then begin
    Res := MessageBox (BOX_QUERY_format2_HDR, BOX_QUERY + BOX_QUERY_format2,
      ButtonYesNoCancel or ButtonDefNo);

    If Res = ButtonResNo Then Begin
      IsNew := IsNew_Init;
      ret := True;
    End Else If Res = ButtonResYes Then Begin
      IsNew := IsNew_Full;
      ret := True;
    End;
  End;
  get_isnew := ret;
end;

{Parttion anlegen}
function MenCreatePartExec (Drive, Pos, Sel: integer; PrevMenu: boolean) :boolean;
var
  HDriv             :PDriveChain;
  HPart, WPart      :PPartChain;
  MEnt, MHlp        :array[0..1] of TMenuStr;
  Cnt, Res, Max     :longint; {UM}
  PartSize          :longint; {UM}
  CH, CC, CS        :longint; {UM}
  ValStr            :string;
  ValRes            :integer;
  Success           :boolean;
  MinStart, MaxEnd  :longint;
  CRTBuffer         :PCRTBuffer;
  IsNew             :Byte;

  procedure CalcRange; {UM: added}
  {berechnet MinStart/MaxEnd einer Partition in Abh. vom Typ}
  begin
    {Partition darf entweder an x/0/1 oder 0/1/1 beginnen, sofern primaer}
    if Sel = PartStatPri then          {prim. Part}
      CHS2LBA (HDriv^.Drive, 1, 0, 1, MinStart)
      {Parameterreichenfolge!}
    else
      CHS2LBA (HDriv^.Drive, 0, 1, 1, MinStart);


    if MinStart < HPart^.Distance then begin
      LBA2CHS (HDriv^.Drive, CH, CC, CS, HPart^.Distance);
      
      if CS <> 1 then begin 
        CS := 1;
        Inc (CH);
        {may get >HDriv^.Head, aber macht nichts}
      end;
      
      if CH <> 0 then begin 
        CH := 0;
        Inc (CC);
      end;
      
      CHS2LBA (HDriv^.Drive, CH, CC, CS, MinStart);
    end;
    {MinStart done}

    MaxEnd := HPart^.Distance + HPart^.PartSize - 1;
    LBA2CHS (HDriv^.Drive, CH, CC, CS, MaxEnd);
    if (CS <> HDriv^.Sector) or (CH <> HDriv^.Head) then begin 
      CS := HDriv^.Sector;
      CH := HDriv^.Head;
      
      if CC > 0 then
        Dec (CC)
      else begin
        CH := 0;
        CS := 1;
        {C/H/S = 0/0/1 -> no partition will be created}
      end;
      
      CHS2LBA (HDriv^.Drive, CH, CC, CS, MaxEnd);
    end;
    {MaxEnd done}
  end;

begin
  MenCreatePartExec := FALSE;
  
  HDriv := GetDriveFromNumber (Drive);
  HPart := GetPartFromPos (Drive, Pos);
  
  CalcRange;                           {added}
  if MaxEnd <= MinStart then Exit;     {sollte nie auftreten}
  
  Max := round ((MaxEnd - MinStart + 1)
    {HPart^.PartSize} / 2048);
  Success := FALSE;
  Cnt := MaxErrCount;
  repeat
    Str (Max, ValStr);
    Res := InputBox (BOX_INP_setPSize_HDR,
      BOX_INP_setPSize_PAR1 + ValStr + BOX_INP_setPSize_POST,
      ValStr, 5, RIGHT, FALSE);

    if Res = ButtonDefOK then begin
      Val (ValStr, PartSize, ValRes);
      if (ValRes = 0) and (PartSize >= 1) and (PartSize <= Max) then
        Success := TRUE
      else
        Beep (TRIPLE);
    end
    else
      MenCreatePartExec := FALSE;

    Dec (Cnt)
  until (Res = ButtonResCancel) or (Cnt = 0) or (Success) ;

  if Success then begin
    {Es soll nur ein Teilbereich angelegt werden? Dann Auswahl wohin damit}
    if PartSize < Max then begin
      MEnt[0] := MEN_setPPos_START;
      MHlp[0] := '[CREATEPOS]';

      MEnt[1] := MEN_setPPos_END;
      MHlp[1] := '[CREATEPOS]';

      CRTBuffer := nil;
      if PrevMenu then
        Res := DoMenu (7, 5, 2, 0, MEN_setPPos, MEnt, MHlp, CRTBuffer)
      else
        Res := DoMenu (5, 4, 2, 0, MEN_setPPos, MEnt, MHlp, CRTBuffer);
      RestoreWindow (CRTBuffer, FALSE);

      {Wenn keine Auswahl getÑtigt wurde, dann raus hier}
      if Res = 0 then begin
        MenCreatePartExec := FALSE;
        Exit;
      end;

      if not get_isnew(IsNew) then begin
        MenCreatePartExec := TRUE;
        Exit;
      end;

      {Ansonsten die Lagedaten berechnen}
      begin
        PartSize := PartSize * 2048;   {zurueck zu #Sektoren}
        if Res = 1 then begin
          {Am Anfang des freien Bereiches}
          max := MaxEnd;
          LBA2CHS (HDriv^.Drive, CH, CC, CS, MaxEnd);
          while (MaxEnd - MinStart + 1 > PartSize) and (CC > 0) do begin
            max := MaxEnd;
            Dec (CC);
            CHS2LBA (HDriv^.Drive, CH, CC, CS, MaxEnd);
          end;
          MaxEnd := max;

          {MaxEnd wird auf den Wert vor der letzten énderung zurÅckgesetzt,
          dadurch ist die erstellte Partition immer >= Partsize}
        end
        else begin
          max := MinStart;
          LBA2CHS (HDriv^.Drive, CH, CC, CS, MinStart);
          while MaxEnd - MinStart + 1 > PartSize do begin
            max := MinStart;
            Inc (CC);
            CH := 0;                   { falls bei 0/1/1 begonnen }
            CHS2LBA (HDriv^.Drive, CH, CC, CS, MinStart);
          end;
          MinStart := max;             {s.o.}
        end;
      end;
    end else
      if not get_isnew(IsNew) then begin
        MenCreatePartExec := TRUE;
        Exit;
      end;

    {Partitionsgrenzen jetzt anhand MinStart/MaxEnd festlegen}
    HPart^.Distance := MinStart;
    HPart^.PartSize := MaxEnd - MinStart + 1;

    {Partitions Status setzen}
    if Sel = PartStatPri then begin
      HPart^.PartStat := PartStatPri;
      HPart^.StartSec := HPart^.Distance;
    end
    else begin
      HPart^.PartStat := PartStatLog;
      {StartSec von logischen Laufwerken berechnen}
      LBA2CHS (HDriv^.Drive, CH, CC, CS, HPart^.Distance);
      if CH < HDriv^.Head then
        Inc (CH)
      else begin
        CH := 0;
        Inc (CC)
      end;
      CHS2LBA (HDriv^.Drive, CH, CC, CS, HPart^.StartSec);
    end;

    {Standardpartitionstyp in AbhÑngigkeit des Systems setzen}
    if UseOS2 or UseWinNT then
      HPart^.PartType := PartTypeHPFSNTFS
    else if UseLinux then
      HPart^.PartType := PartTypeLinux

      {Standardpartitionstyp in AbhÑngigkeit der Grî·e setzen}
    else if (HPart^.Distance + HPart^.PartSize - 1 <= HDriv^.MaxOldBIOSSec) or NoWin9xPTyp then begin
      if HPart^.PartSize <= MaxFAT12Size then
        HPart^.PartType := PartTypeFAT12
      else if HPart^.PartSize <= MaxFAT16SmlSize then
        HPart^.PartType := PartTypeFAT16Sml
      else if HPart^.PartSize <= MaxFAT16BigSize then
        HPart^.PartType := PartTypeFAT16Big
      else
        HPart^.PartType := PartTypeFAT32;
    end
    else begin
      if HPart^.PartSize <= FAT32LohntSichSize then
        HPart^.PartType := PartTypeFAT16X
      else
        HPart^.PartType := PartTypeFAT32X;
    end;

    {Auf evtl. vorhandene aktive primÑre Partitionen testen}
    {Mehrere sichtbare Partitionen werden von CleanUpChain behandelt!}
    if Sel = PartStatPri then
      if HDriv^.PriPart > 0 then begin
        Cnt := 0;
        WPart := HDriv^.PartChain;
        while WPart <> nil do begin
          if WPart^.PartStat = PartStatAct then
            Inc (Cnt);
          WPart := WPart^.Next;
        end;

        {noch keine aktiv}
        if Cnt = 0 then
          HPart^.PartStat := PartStatAct;
      end
      else
        HPart^.PartStat := PartStatAct;

    for Cnt := 1 to MaxSystems do
      if HPart^.PartType = IPartType[Cnt] then
        HPart^.PartSyst := Cnt;

    if not CleanUpChain (DriveChain) then ExitProgram (FAILURE);

    HPart^.IsNew := IsNew;
    HDriv^.HasChanged := TRUE;
    MenCreatePartExec := TRUE;
  end;
end;

function MenCreatePart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart      :PPartChain;
  MEnt, MHlp :array[0..1] of TMenuStr;
  Res        :integer;
  CRTBuffer  :PCRTBuffer;
  DoExit     :boolean;
begin
  HPart := GetPartFromPos (Drive, Pos);
  
  {Es kann sowohl eine Partition als auch ein log. Laufwerk angelegt werden}
  if HPart^.PartStat = PartStatFreePriLog then begin
    MEnt[0] := MEN_newPart_PRI;
    MHlp[0] := '[CREATEPART]';

    MEnt[1] := MEN_newPart_LOG;
    MHlp[1] := '[CREATEPART]';

    CRTBuffer := nil;
    DoExit := FALSE;
    Res := 1;
    repeat
      Res := DoMenu (5, 4, 2, Res - 1, MEN_newPart, MEnt, MHlp, CRTBuffer);
      if Res <> 0 then
        if Res = 1 then
          DoExit := MenCreatePartExec (Drive, Pos, PartStatPri, TRUE)
        else
          DoExit := MenCreatePartExec (Drive, Pos, PartStatLog, TRUE);
    until DoExit or (Res = 0) ;

    RestoreWindow (CRTBuffer, FALSE);
    MenCreatePart := DoExit;
  end

  {Es kann nur eine Partition angelegt werden}
  else if HPart^.PartStat = PartStatFreePri then
    MenCreatePart := MenCreatePartExec (Drive, Pos, PartStatPri, FALSE)

    {Es kann nur ein log. Laufwerk angelegt werden}
  else if HPart^.PartStat = PartStatFreeLog then
    MenCreatePart := MenCreatePartExec (Drive, Pos, PartStatLog, FALSE);
end;


function MenDeleteEntry (Drive, Pos: integer; Display: boolean) :boolean; forward;

{Partition lîschen}
function MenDeletePart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HDriv        :PDriveChain;
  HPart, WPart :PPartChain;
  Cnt, Res     :integer;
begin
  MenDeletePart := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  Res := MessageBox (BOX_QUERY_delPart_HDR, 
    BOX_QUERY + BOX_QUERY_delPart, 
    ButtonYesNo or ButtonDefNo);
  if Res = ButtonResYes then begin 
    HDriv := GetDriveFromNumber (Drive);
    HPart := HDriv^.PartChain;
    WPart := HPart;
    Cnt := 0;
    while (Cnt < Pos) and (HPart <> nil) do begin 
      WPart := HPart;
      HPart := HPart^.Next;
      Inc (Cnt)
    end;
    
    if (BMIsInst) and (HPart^.PartEntr <> PartEntrNone) then
      MenDeleteEntry (Drive, Pos, FALSE);
    
    if not BMIsInst or (HPart^.PartEntr = PartEntrNone) then begin 
      if WPart = HPart then begin 
        HDriv^.PartChain := HPart^.Next;
        dispose (HPart)
      end
      else begin 
        WPart^.Next := HPart^.Next;
        dispose (HPart)
      end;
      
      if not CleanUpChain (DriveChain) then ExitProgram (FAILURE);
      
      HDriv^.HasChanged := TRUE;
      MenDeletePart := TRUE;
    end;
  end;
end;

{Partition aktivieren}
function MenActivatePart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
  HDriv :PDriveChain;
  Cnt   :integer;
begin
  MenActivatePart := TRUE;

  HPart := GetPartFromPos (Drive, 0);
  Cnt := 0;
  while HPart <> nil do begin
    if Cnt = Pos then
      HPart^.PartStat := PartStatAct
    else if HPart^.PartStat = PartStatAct then
      HPart^.PartStat := PartStatPri;

    HPart := HPart^.Next;
    Inc (Cnt)
  end;

  if not CleanUpChain (Drivechain) then ExitProgram (FAILURE);

  HDriv := GetDriveFromNumber (Drive);
  If HDriv <> nil then
     HDriv^.HasChanged := TRUE;
end;

{Laufwerk zuordnen}
function MenSelectPart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
  HDriv :PDriveChain;
  Cnt   :integer;
  FoundActive :Boolean;
begin
  MenSelectPart := TRUE;

  HPart := GetPartFromPos (Drive, 0);
  Cnt := 0;
  FoundActive := FALSE;
  while HPart <> nil do begin
    if Cnt = Pos then
      HPart^.PartType := HPart^.PartType - PartMaskHide
    else if AutoHide and (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E]) then
      if HPart^.PartStat = PartStatPri then
        HPart^.PartType := HPart^.PartType + PartMaskHide
      else if HPart^.PartStat = PartStatAct then begin
        HPart^.PartType := HPart^.PartType + PartMaskHide;
        MenActivatePart (Drive, Pos, Display);
      end;

    {If HPart^.PartStat = PartStatAct then
      FoundActive := TRUE; lassen wir sein -> selektiert wird aktiv}

    HPart := HPart^.Next;
    Inc (Cnt)
  end;

  if not FoundActive then
    MenActivatePart (Drive, Pos, Display);

  if not CleanUpChain (Drivechain) then ExitProgram (FAILURE);

  HDriv := GetDriveFromNumber (Drive);
  If HDriv <> nil then
    HDriv^.HasChanged := TRUE;
end;

{Laufwerk verstecken}
function MenHidePart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
  HDriv :PDriveChain;
  Cnt   :integer;
begin
  MenHidePart := TRUE;

  HPart := GetPartFromPos (Drive, Pos);
  if (HPart <> nil) and (HPart^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E]) then begin
    HPart^.PartType := HPart^.PartType + PartMaskHide;
    if HPart^.PartStat = PartStatAct then
      HPart^.PartStat := PartStatPri;
    HDriv := GetDriveFromNumber (Drive);
    If HDriv <> nil then
      HDriv^.HasChanged := TRUE;
  end;

  if not CleanUpChain (Drivechain) then ExitProgram (FAILURE);
end;

{Partitionstyp Ñndern}
function MenChangePartTypeExec (Drive, Pos, Typ: integer) :boolean;
var
  HDriv      :PDriveChain;
  HPart      :PPartChain;
  Cnt, Res   :integer;
  ValStr     :string;
  Success    :boolean;
  IsNew      :byte;
begin
  MenChangePartTypeExec := FALSE;

  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;

  HPart := GetPartFromPos (Drive, Pos);

  if Typ = 0 then begin
    Success := FALSE;
    Cnt := MaxErrCount;
    repeat
      ValStr := Byte2Hex (HPart^.PartType);
      Res := InputBox (BOX_INP_setPTyp_HDR,
        BOX_INP_setPTyp, ValStr, 2, RIGHT, FALSE);

      if Res = ButtonResOK then begin
        ValStr := '$' + ValStr;
        Val (ValStr, Typ, Res);
        if (Res = 0) and (Typ >= 1) and (Typ <= 255) then
          Success := TRUE
        else
          Beep (TRIPLE);
      end;

      Dec (Cnt);
    until (Res = ButtonResCancel) or (Cnt = 0) or (Success) ;
  end
  else
    Success := TRUE;

  if Success and (Typ <> HPart^.PartType) then begin
    if get_isnew(IsNew) then begin
      if not HPart^.KnowsPass and (HPart^.PartPWD[0] <> #0) then
        if not VerifyPassword (MEN_PartPass, HPart^.PartPWD) then Exit
        else HPart^.KnowsPass := TRUE;

      HPart^.PartType := Typ;
      HPart^.PartSyst := PartSystNone;
      for Cnt := 1 to MaxSystems do
        if Typ = IPartType[Cnt] then
          HPart^.PartSyst := Cnt;

      if not CleanUpChain (DriveChain) then ExitProgram (FAILURE);

      HPart^.IsNew := IsNew;
      HDriv := GetDriveFromNumber (Drive);
      If HDriv <> nil then
         HDriv^.HasChanged := TRUE;

      if (BMIsInst) and (HPart^.PartEntr <> PartEntrNone) then
        WriteBM := TRUE;
      MenChangePartTypeExec := TRUE;
    end;
  end;
end;

{Partitionstyp Ñndern}
function MenChangePartType (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart      :PPartChain;
  HDriv      :PDriveChain;
  MEnt, MHlp :array[0..6] of TMenuStr;
  Typ        :array[0..6] of integer;
  Men, Res   :integer;
  CRTBuffer  :PCRTBuffer;
  DoExit     :boolean;
  Int13XFlag :boolean;
begin
  HPart := GetPartFromPos (Drive, Pos);
  HDriv := GetDriveFromNumber (Drive);
  
  {check need for Int13X-Flag}
  Int13XFlag := (HPart^.Distance + HPart^.PartSize - 1 > HDriv^.MaxOldBIOSSec)
    and not NoWin9xPTyp;
  
  Men := 0;
  
  {Other Partition Type}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  MEnt[Men] := MEN_otherPTyp;
  Typ[Men] := 0;
  Inc (Men);
  
  MEnt[Men] := '';
  Inc (Men);
  
  {FAT12/16}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  {FAT16 INT13X}
  if Int13XFlag then begin 
    if HPart^.PartSize <= MaxFAT16BigSize then begin 
      if (HPart^.PartType <> PartTypeFAT16X) and (HPart^.PartType <> (PartTypeFAT16X or PartMaskHide)) then begin 
        MEnt[Men] := 'FAT16 INT13X';
        Typ[Men] := PartTypeFAT16X;
        Inc (Men);
      end;
    end;
  end
  {FAT12 <16M}
  else if HPart^.PartSize <= MaxFAT12Size then begin 
    if (HPart^.PartType <> PartTypeFAT12) and (HPart^.PartType <> (PartTypeFAT12 or PartMaskHide)) then begin
      MEnt[Men] := 'FAT12 <16M';
      Typ[Men] := PartTypeFAT12;
      Inc (Men);
    end;
  end
  {FAT16 <32M}
  else if HPart^.PartSize <= MaxFAT16SmlSize then begin
    if (HPart^.PartType <> PartTypeFAT16Sml) and (HPart^.PartType <> (PartTypeFAT16Sml or PartMaskHide)) then begin 
      MEnt[Men] := 'FAT16 <32M';
      Typ[Men] := PartTypeFAT16Sml;
      Inc (Men);
    end;
  end
  {FAT16 >32M}
  else if HPart^.PartSize <= MaxFAT16BigSize then begin 
    if (HPart^.PartType <> PartTypeFAT16Big) and (HPart^.PartType <> (PartTypeFAT16Big or PartMaskHide)) then begin 
      MEnt[Men] := 'FAT16 >32M';
      Typ[Men] := PartTypeFAT16Big;
      Inc (Men);
    end;
  end;
  
  {FAT32 INT13X}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  if Int13XFlag then begin 
    if (HPart^.PartType <> PartTypeFAT32X) and (HPart^.PartType <> (PartTypeFAT32X or PartMaskHide)) then begin 
      MEnt[Men] := 'FAT32 INT13X';
      Typ[Men] := PartTypeFAT32X;
      Inc (Men);
    end;
  end
  {FAT32}
  else if (HPart^.PartType <> PartTypeFAT32) and (HPart^.PartType <> (PartTypeFAT32 or PartMaskHide)) then begin 
    MEnt[Men] := 'FAT32';
    Typ[Men] := PartTypeFAT32;
    Inc (Men);
  end;
  
  {HPFS/NTFS}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  if (HPart^.PartType <> PartTypeHPFSNTFS) and (HPart^.PartType <> (PartTypeHPFSNTFS or PartMaskHide)) then begin 
    MEnt[Men] := 'HPFS/NTFS';
    Typ[Men] := PartTypeHPFSNTFS;
    Inc (Men);
  end;
  
  {Linux Native}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  if HPart^.PartType <> PartTypeLinux then begin 
    MEnt[Men] := 'Linux native';
    Typ[Men] := PartTypeLinux;
    Inc (Men);
  end;
  
  {Linux Swap}
  MHlp[Men] := '[CHANGEPARTTYPE]';
  if HPart^.PartType <> PartTypeLnxSwap then begin 
    MEnt[Men] := 'Linux swap';
    Typ[Men] := PartTypeLnxSwap;
    Inc (Men);
  end;
  
  CRTBuffer := nil;
  DoExit := FALSE;
  repeat
    Res := DoMenu (5, 4, Men, 0, MEN_PartType, MEnt, MHlp, CRTBuffer);
    if Res <> 0 then
      DoExit := MenChangePartTypeExec (Drive, Pos, Typ[Res - 1]);
  until DoExit or (Res = 0) ;
  RestoreWindow (CRTBuffer, FALSE);
  
  MenChangePartType := DoExit;
end;

{in Menu aufnehmen}
function MenInsertEntry (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
  Res   :integer;
  Name  :string;
begin
  MenInsertEntry := FALSE;
  
  HPart := GetPartFromPos (Drive, Pos);
  
  Name := '';
  Res := InputBox (BOX_INP_newEntr_HDR, 
    BOX_INP_newEntr, Name, 16, LEFT, FALSE);
  
  if (Res = ButtonResOK) and (Name <> '') then begin 
    StrPCopy (HPart^.PartName, Name);
    HPart^.PartEntr := InfoEntr;
    Inc (InfoEntr);
    WriteBM := TRUE;
    MenInsertEntry := TRUE;
  end;
end;

{aus Menu lîschen}
function MenDeleteEntry (Drive, Pos: integer; Display: boolean) :boolean;
var
  HDriv :PDriveChain;
  HPart :PPartChain;
  Res   :integer;
begin
  MenDeleteEntry := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  HPart := GetPartFromPos (Drive, Pos);

  if Display then
    Res := MessageBox (BOX_QUERY_delEntr_HDR, 
      BOX_QUERY + BOX_QUERY_delEntr, 
      ButtonYesNo or ButtonDefNo)
  else
    Res := ButtonResYes;
  
  if Res = ButtonResYes then begin 
    if not HPart^.KnowsPass and (HPart^.PartPWD[0] <> #0) then
      if not VerifyPassword (MEN_PartPass, HPart^.PartPWD) then Exit;
    
    HPart^.PartName[0] := #0;
    HPart^.PartPWD[0] := #0;
    HPart^.KnowsPass := FALSE;
    Res := HPart^.PartEntr;
    HPart^.PartEntr := PartEntrNone;
    Dec (InfoEntr);
    
    {Und aufrÑumen...}
    HDriv := DriveChain;
    repeat
      HPart := HDriv^.PartChain;
      while HPart <> nil do begin 
        if (HPart^.PartEntr > Res) and (HPart^.PartEntr <> PartEntrNone) then
          Dec (HPart^.PartEntr);
        HPart := HPart^.Next
      end;
      HDriv := HDriv^.Next;
    until HDriv = nil ;
    
    WriteBM := TRUE;
    MenDeleteEntry := TRUE;
  end;
end;

{Partitionsname Ñndern}
function MenChangeEntry (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
  Res   :integer;
  Name  :string[16];
  Temp  :string[16];
begin
  MenChangeEntry := FALSE;

  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  HPart := GetPartFromPos (Drive, Pos);
  
  Name := StrPas (HPart^.PartName);
  Temp := Name;
  Res := InputBox (BOX_INP_chngEntr_HDR, 
    BOX_INP_chngEntr, Name, 16, LEFT, FALSE);
  
  if (Res = ButtonResOK) and (Name <> '') and (Name <> Temp) then begin 
    if not HPart^.KnowsPass and (HPart^.PartPWD[0] <> #0) then
      if not VerifyPassword (MEN_PartPass, HPart^.PartPWD) then Exit
      else HPart^.KnowsPass := TRUE;
    
    StrPCopy (HPart^.PartName, Name);
    WriteBM := TRUE;
    MenChangeEntry := TRUE;
  end;
end;

{Passowrt Ñndern}
function MenChangePassword (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
begin
  MenChangePassword := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  HPart := GetPartFromPos (Drive, Pos);
  
  if GetPassword (MEN_partPass, HPart^.PartPWD) then begin 
    WriteBM := TRUE;
    HPart^.KnowsPass := TRUE;
    MenChangePassword := TRUE;
  end;
end;

function MenInstallBM (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenInstallBM := TRUE;
  
  WriteBM := TRUE;
  
  BMIsInst := TRUE;
  BMHasPWDs := TRUE;
  
  BMUpdate := FALSE;
  
  BMNewInst := TRUE;
  
  CryptSize := XCRYSize;
  CheckSize := XCHKSize;
  
  Timeout := TimeoutDef;
  TimeHndl := TimeoutInactivate;
  ClearScrn := FALSE;
  BootLast := FALSE;
  SimpleMenus := FALSE;
end;

function MenUninstallBM (Drive, Pos: integer; Display: boolean) :boolean;
var
  Res   :integer;
  HDriv :PDriveChain;
  HPart :PPartChain;
begin
  MenUninstallBM := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  if Display then
    Res := MessageBox (BOX_QUERY_uninBMgr_HDR, 
      BOX_QUERY + BOX_QUERY_uninBMgr, 
      ButtonYesNo or ButtonDefNo)
  else
    Res := ButtonResYes;
  
  if Res = ButtonResYes then begin 
    if BMPrevInst then
      WriteBM := TRUE
    else
      WriteBM := FALSE;

    BMIsInst := FALSE;
    BMHasPWDs := FALSE;
    
    BMUpdate := FALSE;
    
    BMNewInst := FALSE;
    
    InfoEntr := 0;
    
    MasterPWD[0] := #0;
    FloppyPWD[0] := #0;
    
    HDriv := DriveChain;
    while HDriv <> nil do begin 
      HPart := HDriv^.PartChain;
      while HPart <> nil do begin
        HPart^.PartName[0] := #0;
        HPart^.PartPWD[0] := #0;
        HPart^.KnowsPass := FALSE;
        
        HPart^.PartEntr := PartEntrNone;
        
        HPart := HPart^.Next
      end;
      HDriv := HDriv^.Next;
    end;
    
    MenUninstallBM := TRUE;
  end;
end;

function MenUpdateBM (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenUpdateBM := FALSE;
  
  if BMUpdate then begin 
    if BMPrevVer < BMVersionExtension1 then begin 
      BMHasPWDs := FALSE;
      CryptSize := OldCHKSize;
      CheckSize := OldCHKSize;
    end;
    
    BMUpdate := FALSE;
    
  end
  else begin 
    BMHasPWDs := TRUE;
    CryptSize := XCRYSize;
    CheckSize := XCHKSize;
    
    BMUpdate := TRUE;
  end;
end;

{Set Default Partition}
function MenSetDefaultPart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HDriv        :PDriveChain;
  HPart, WPart :PPartChain;
  Cnt          :integer;
begin
  MenSetDefaultPart := TRUE;
  
  HDriv := GetDriveFromNumber (Drive);
  WPart := GetPartFromPos (Drive, Pos);
  
  Cnt := WPart^.PartEntr;
  WPart^.PartEntr := 0;

  HDriv := DriveChain;
  {AufrÑumen}
  repeat
    HPart := HDriv^.PartChain;
    while HPart <> nil do begin 
      if (HPart^.PartEntr < Cnt) and (HPart <> WPart) then
        Inc (HPart^.PartEntr);
      HPart := HPart^.Next
    end;
    HDriv := HDriv^.Next;
  until HDriv = nil ;
  WriteBM := TRUE;
end;

{Letzte Auswahl booten}
function MenSetBootLast (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenSetBootLast := FALSE;
  
  BootLast := not BootLast;
  WriteBM := TRUE;
end;

{Diese Partition auswaehlen}
function MenSetThisPart (Drive, Pos: integer; Display: boolean) :boolean;
var
  HPart :PPartChain;
begin
  MenSetThisPart := TRUE;
  
  HPart := GetPartFromPos (Drive, Pos);
  LastEntry := HPart^.PartEntr;
  WriteBM := TRUE;
end;

{Timeout Zeit festlegen}
function MenSetBootTimeout (Drive, Pos: integer; Display: boolean) :boolean;
var
  Cnt, Res         :integer;
  Success          :boolean;
  TimeStr          :string;
  TimeTemp, ValRes :integer;
  TimeoutMin       :integer;
begin
  MenSetBootTimeout := FALSE;
  if SuppBlind then
    TimeoutMin := TimeoutMinBlind
  else
    TimeoutMin := TimeoutMinNoBlind;
  
  Success := FALSE;
  Cnt := MaxErrCount;
  repeat
    Str (Timeout, TimeStr);
    Res := InputBox (BOX_INP_setTmOut_HDR, 
      BOX_INP_setTmOut_PAR1 + Int2Str (TimeoutMin, 1) +
      '..' + Int2Str (TimeoutMax, 1) + BOX_INP_setTmOut_POST, 
      TimeStr, 3, RIGHT, FALSE);
    
    if Res = ButtonResOK then begin 
      Val (TimeStr, TimeTemp, ValRes);
      if ValRes = 0 then
        Success := TRUE
      else
        Beep (TRIPLE);
    end
    else
      MenSetBootTimeout := FALSE;
    
    Dec (Cnt)
  until (Res = ButtonResCancel) or (Cnt = 0) or (Success) ;
  
  if Success then begin 
    if TimeTemp <> Timeout then begin 
      if TimeTemp > TimeoutMax then
        TimeTemp := TimeoutMax;
      
      if (TimeTemp < TimeoutMin) and (TimeTemp <> 0) then
        TimeTemp := TimeoutMin;
      
      Timeout := TimeTemp;
      WriteBM := TRUE;
    end;
    MenSetBootTimeout := TRUE;
  end;
end;

{Timeout Handling festlegen}
function MenSetTimeoutHandling (Drive, Pos: integer; Display: boolean) :boolean;
var
  MEnt, MHlp :array[0..2] of TMenuStr;
  Res, Sel   :integer;
  CRTBuffer  :PCRTBuffer;
begin
  MenSetTimeoutHandling := FALSE;
  
  MHlp[0] := '[STARTHANDLING]';
  MHlp[1] := '[STARTHANDLING]';
  MHlp[2] := '[STARTHANDLING]';
  
  Sel := TimeHndl;
  repeat
    MEnt[0] := MEN_setBVal_dactL;
    MEnt[1] := MEN_setBVal_rsetL;
    MEnt[2] := MEN_setBVal_ignrL;
    
    {HÑkchen setzen}
    MEnt[Sel] := ChrBullet + MEnt[Sel];
    
    CRTBuffer := nil;
    Res := DoMenu (7, 5, 3, Sel, MEN_setBVal_TmLim, MEnt, MHlp, CRTBuffer);
    if Res <> 0 then
      Sel := Res - 1;
    RestoreWindow (CRTBuffer, FALSE);
  until Res = 0 ;
  
  if Sel <> TimeHndl then begin 
    TimeHndl := Sel;
    WriteBM := TRUE;
  end;
end;

{Bildschirm lîschen}
function MenSetClearScreen (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenSetClearScreen := FALSE;
  
  ClearScrn := not ClearScrn;
  WriteBM := TRUE;
end;

{Einfache Menus}
function MenSetSimpleMenus (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenSetSimpleMenus := FALSE;
  
  SimpleMenus := not SimpleMenus;
  WriteBM := TRUE;
end;

{Masterpa·wort festlegen}
function MenSetBootPassword (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenSetBootPassword := FALSE;
  
  if GetPassword (MEN_mastPass, MasterPWD) then begin 
    WriteBM := TRUE;
    KnowsMaster := TRUE;
    MenSetBootPassword := TRUE;
  end;
end;

{Floppypa·wort festlegen}
function MenSetFloppyPassword (Drive, Pos: integer; Display: boolean) :boolean;
begin
  MenSetFloppyPassword := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  if GetPassword (MEN_flpyPass, FloppyPWD) then begin 
    WriteBM := TRUE;
    MenSetFloppyPassword := TRUE;
  end;
end;

{Startwerte festlegen.}
function MenSetStartVals (Drive , Pos: integer; Display: boolean) :boolean;
var
  HPart                 :PPartChain;
  Men, Res              :integer;
  BootL, ClrScr, SimplM :integer;
  MEnt, MHlp            :array[0..9] of TMenuStr;
  MAct                  :array[0..9] of TMenuFunc;
  DoExit                :boolean;
  CRTBuffer             :PCRTBuffer;
begin
  HPart := GetPartFromPos (Drive, Pos);
  
  Men := 0;
  
  if (HPart^.PartEntr <> PartEntrNone) and (HPart^.PartEntr > 0) then begin 
    MEnt[Men] := MEN_setBVal_defP;
    MHlp[Men] := '[STARTAKTPART]';
    MAct[Men] := MenSetDefaultPart;
    Inc (Men);
  end;
  
  BootL := Men;
  MHlp[Men] := '[STARTLASTPART]';
  MAct[Men] := MenSetBootLast;
  Inc (Men);
  
  if (HPart^.PartEntr <> PartEntrNone) and (HPart^.PartEntr <> LastEntry) then begin 
    MEnt[Men] := MEN_setBVal_ThisP;
    MHlp[Men] := '[STARTTHISPART]';
    MAct[Men] := MenSetThisPart;
    Inc (Men);
  end;

  MEnt[Men] := MEN_setBVal_TmOut;
  MHlp[Men] := '[STARTTIMEOUT]';
  MAct[Men] := MenSetBootTimeout;
  Inc (Men);
  
  MEnt[Men] := Men_setBVal_TmLim + ChrArrow;
  MHlp[Men] := '[STARTHANDLING]';
  MAct[Men] := MenSetTimeoutHandling;
  Inc (Men);

  ClrScr := Men;
  MHlp[Men] := '[STARTCLRSCR]';
  MAct[Men] := MenSetClearScreen;
  Inc (Men);
  
  SimplM := Men;
  MHlp[Men] := '[STARTSIMPLE]';
  MAct[Men] := MenSetSimpleMenus;
  Inc (Men);
  
  if BMHasPWDs then begin 
    {Trennzeile}
    MEnt[Men] := '';
    Inc (Men);
    
    {Masterpassword}
    if MasterPWD[0] <> #0 then
      MEnt[Men] := MEN_chngMastPass
    else
      MEnt[Men] := MEN_setMastPass;
    MHlp[Men] := '[STARTMASTER]';
    MAct[Men] := MenSetBootPassword;
    Inc (Men);
    
    {Floppypassword}
    if FloppyPWD[0] <> #0 then
      MEnt[Men] := MEN_chngFlpyPass
    else
      MEnt[Men] := MEN_setFlpyPass;
    MHlp[Men] := '[STARTFLOPPY]';
    MAct[Men] := MenSetFloppyPassword;
    Inc (Men);
  end;
  
  CRTBuffer := nil;
  DoExit := FALSE;
  Res := 1;
  repeat
    if BootLast then
      MEnt[BootL] := ChrCheck + MEN_setBVal_BootL
    else
      MEnt[BootL] := MEN_setBVal_BootL;
    
    if ClearScrn then
      MEnt[ClrScr] := ChrCheck + MEN_setBVal_ClrScr
    else
      MEnt[ClrScr] := MEN_setBVal_ClrScr;
    
    if SimpleMenus then
      MEnt[SimplM] := ChrCheck + MEN_setBVal_SimplM
    else
      MEnt[SimplM] := Men_setBVal_SimplM;

    Res := DoMenu (5, 4, Men, Res - 1, MEN_setBVal, MEnt, MHlp, CRTBuffer);
    if Res <> 0 then
      DoExit := MAct[Res - 1] (Drive, Pos, Display);
  until DoExit or (Res = 0) ;
  
  RestoreWindow (CRTBuffer, FALSE);
  MenSetStartVals := DoExit;
end;

function MenLoadConfig (Drive, Pos: integer; Display: boolean) :boolean;
var
  Res, NDr, NPt :integer;
  Cfg           :file;
  Header        :TCfgFileHeader;
  Error         :boolean;
  CfgErr        :boolean;
  BMWasInst     :boolean;
  BMInfo        :TCfgBMInfo;
  Protection    :integer;
  HDriv, WDriv  :PDriveChain;
  NewDriveChain :PDriveChain;
  HPart         :PPartChain;
begin
  MenLoadConfig := FALSE;

  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;

  Res := InputBox (BOX_INP_saveConf_HDR, BOX_INP_loadSaveConf,
    ConfigFile, PathMaxLen, LEFT, FALSE);
  if Res = ButtonResCancel then
    Exit;

  Assign (Cfg, ConfigFile);
  FileMode := 0;
  Reset (Cfg, 1);
  if IOResult <> 0 then begin
    Res := MessageBox (BOX_WARN_errFile_HDR,
      BOX_WARN + BOX_WARN_errFile_opnXCF,
      ButtonOK or ButtonDefOK);
    Exit;
  end;

  Error := FALSE;
  CfgErr := FALSE;

  {Read File Header}
  BlockRead (Cfg, Header, sizeof (Header), Res);
  if (Res <> sizeof (Header)) or
      (Header.Magic <> XFDCfgMagic) or (Header.Version <> XFDCfgVersion) then
    Error := TRUE

    {Read number of Drives}
  else begin
    BlockRead (Cfg, NDr, sizeof (NDr), Res);
    if Res <> sizeof (NDr) then
      Error := TRUE
    else if NDr <> PhyDrives then
      CfgErr := TRUE;
  end;

  {Read Bootmanager Configuration}
  if not (Error or CfgErr) then begin
    BlockRead (Cfg, BMWasInst, sizeof (BMWasInst), Res);
    if Res <> sizeof (BMWasInst) then
      Error := TRUE
  end;
  if BMWasInst and not (Error or CfgErr) then begin
    BlockRead (Cfg, BMInfo, sizeof (BMInfo), Res);
    if Res <> sizeof (BMInfo) then
      Error := TRUE;
  end;

  WDriv := DriveChain;
  NewDriveChain := nil;

  {Read Drive Information}
  while (NDr > 0) and not (Error or CfgErr) do begin
    if MaxAvail < sizeof (TDriveChain) then begin
      MemoryError;
      Exit;
    end;
    if NewDriveChain = nil then begin
      New (HDriv);
      NewDriveChain := HDriv;
    end
    else begin
      New (HDriv^.Next);
      HDriv := HDriv^.Next;
    end;
    BlockRead (Cfg, HDriv^, sizeof (HDriv^), Res);
    HDriv^.Next := nil;
    HDriv^.PartChain := nil;
    if Res <> sizeof (HDriv^) then
      Error := TRUE
    else if (HDriv^.Size <> WDriv^.Size) or
        (HDriv^.Head <> WDriv^.Head) or
        (HDriv^.Sector <> WDriv^.Sector) then
      CfgErr := TRUE

    else begin
      {Read number of Partitions}
      BlockRead (Cfg, NPt, sizeof (NPt), Res);
      if Res <> sizeof (NPt) then
        Error := TRUE;

      {Read Partition Information}
      while (NPt > 0) and not (Error or CfgErr) do begin
        if MaxAvail < sizeof (TPartChain) then begin
          MemoryError;
          Exit;
        end;
        if HDriv^.PartChain = nil then begin
          New (HPart);
          HDriv^.PartChain := HPart;
        end
        else begin
          New (HPart^.Next);
          HPart := HPart^.Next;
        end;
        BlockRead (Cfg, HPart^, sizeof (HPart^), Res);
        HPart^.Next := nil;
        if Res <> sizeof (HPart^) then
          Error := TRUE;

        if BMWasInst then begin
          if not (Error or CfgErr) then begin
            BlockRead (Cfg, Protection, sizeof (Protection), Res);
            if Res <> sizeof (Protection) then
              Error := TRUE
            else
              ProtectPWD (HPart^.PartPWD, Protection);
            HPart^.KnowsPass := FALSE;
          end;
        end;

        Dec (NPt);
      end;
    end;

    Dec (NDr);
    WDriv := WDriv^.Next;
  end;

  Close (Cfg);

  if not (Error or CfgErr) then
    if not CleanUpChain (NewDriveChain) then
      Error := TRUE;

  if Error or CfgErr then begin
    UnloadTable (NewDriveChain);
    if Error then
      Res := MessageBox (BOX_WARN_errFile_HDR,
        BOX_WARN + BOX_WARN_errFile_rdXCF,
        ButtonOK or ButtonDefOK)
    else
      Res := MessageBox (BOX_WARN_loadConf_HDR,
        BOX_WARN + BOX_WARN_loadConf,
        ButtonOK or ButtonDefOK);
  end
  else begin
    UnloadTable (DriveChain);
    DriveChain := NewDriveChain;

    if BMWasInst then begin
      if not BMIsInst then
        MenInstallBM (Drive, Pos, Display);
      InfoEntr := BMInfo.InfoEntr;
      Timeout := BMInfo.Timeout;
      TimeHndl := BMInfo.TimeHndl;
      ClearScrn := BMInfo.ClearScrn;
      BootLast := BMInfo.BootLast;
      LastEntry := BMInfo.LastEntry;
      SimpleMenus := BMInfo.SimpleMenus;
      ProtectPWD (BMInfo.MasterPWD, BMInfo.MasterProt);
      StrCopy (MasterPWD, BMInfo.MasterPWD);
      ProtectPWD (BMInfo.FloppyPWD, BMInfo.FloppyProt);
      StrCopy (FloppyPWD, BMInfo.FloppyPWD);

      WriteBM := TRUE;
    end
    else if BMIsInst then
      MenUninstallBM (Drive, Pos, Display);

    {HasChanged := TRUE;}
    HDriv := DriveChain;
    while HDriv <> nil do begin
      HDriv^.HasChanged := true;
      HDriv := HDriv^.Next;
    end;
    MenLoadConfig := TRUE;
  end;
end;

function MenSaveConfig (Drive, Pos: integer; Display: boolean) :boolean;
var
  Res, Cnt   :integer;
  Cfg        :file;
  Header     :TCfgFileHeader;
  Error      :boolean;
  BMInfo     :TCfgBMInfo;
  Protection :integer;
  HDriv      :PDriveChain;
  HPart      :PPartChain;
begin
  MenSaveConfig := FALSE;
  
  if not KnowsMaster and (MasterPWD[0] <> #0) then
    if not VerifyPassword (MEN_MastPass, MasterPWD) then Exit
    else KnowsMaster := TRUE;
  
  repeat
    Res := InputBox (BOX_INP_saveConf_HDR, BOX_INP_loadSaveConf, 
      ConfigFile, PathMaxLen, LEFT, FALSE);
    if Res = ButtonResOK then begin 
      Assign (Cfg, ConfigFile);
      Reset (Cfg);
      if IOResult = 0 then begin
        Close (Cfg);
        Res := MessageBox (BOX_QUERY_confExists_HDR, 
          BOX_QUERY + BOX_QUERY_confExists, 
          ButtonYesNoCancel or ButtonDefCancel);
      end;
    end;
  until (Res = ButtonResOK) or (Res = ButtonResCancel) ;
  
  if Res = ButtonResCancel then
    Exit;
  
  Rewrite (Cfg, 1);
  if IOResult <> 0 then begin 
    Res := MessageBox (BOX_WARN_errFile_HDR, 
      BOX_WARN + BOX_WARN_errFile_opnXCF, 
      ButtonOK or ButtonDefOK);
    Exit;
  end;
  
  Error := FALSE;
  
  {Write File Header}
  Header.Magic := XFDCfgMagic;
  Header.Version := XFDCfgVersion;
  BlockWrite (Cfg, Header, sizeof (Header), Res);
  if Res <> sizeof (Header) then
    Error := TRUE
    
    {Write number of Drives}
  else begin 
    BlockWrite (Cfg, PhyDrives, sizeof (PhyDrives), Res);
    if Res <> sizeof (PhyDrives) then
      Error := TRUE;
  end;

  {Write Bootmanager Configuration}
  if not Error then begin 
    BlockWrite (Cfg, BMIsInst, sizeof (BMIsInst), Res);
    if Res <> sizeof (BMIsInst) then
      Error := TRUE
  end;
  if BMIsInst and not Error then begin 
    BMInfo.InfoEntr := InfoEntr;
    BMInfo.Timeout := Timeout;
    BMInfo.TimeHndl := TimeHndl;
    BMInfo.ClearScrn := ClearScrn;
    BMInfo.BootLast := BootLast;
    BMInfo.LastEntry := LastEntry;
    BMInfo.SimpleMenus := SimpleMenus;
    StrCopy (BMInfo.MasterPWD, MasterPWD);
    BMInfo.MasterProt := StrLen (MasterPWD);
    ProtectPWD (BMInfo.MasterPWD, BMInfo.MasterProt);
    StrCopy (BMInfo.FloppyPWD, FloppyPWD);
    BMInfo.FloppyProt := StrLen (FloppyPWD);
    ProtectPWD (BMInfo.FloppyPWD, BMInfo.FloppyProt);
    BlockWrite (Cfg, BMInfo, sizeof (BMInfo), Res);
    if Res <> sizeof (BMInfo) then
      Error := TRUE;
  end;
  
  {Write Drive Information}
  HDriv := DriveChain;
  while (HDriv <> nil) and not Error do begin 
    BlockWrite (Cfg, HDriv^, sizeof (HDriv^), Res);
    if Res <> sizeof (HDriv^) then
      Error := TRUE
    
    else begin 
      {Write number of Partitions}
      Cnt := 0;
      HPart := HDriv^.PartChain;
      while HPart <> nil do begin 
        Inc (Cnt);
        HPart := HPart^.Next;
      end;
      BlockWrite (Cfg, Cnt, sizeof (Cnt), Res);
      if Res <> sizeof (Cnt) then
        Error := TRUE;

      {Write Partition Information}
      HPart := HDriv^.PartChain;
      while (HPart <> nil) and not Error do begin 
        if BMIsInst then begin 
          Protection := StrLen (HPart^.PartPWD);
          ProtectPWD (HPart^.PartPWD, Protection);
        end;
        BlockWrite (Cfg, HPart^, sizeof (HPart^), Res);
        if Res <> sizeof (HPart^) then
          Error := TRUE;
        
        if BMIsInst then begin 
          ProtectPWD (HPart^.PartPWD, Protection);
          if not Error then begin 
            BlockWrite (Cfg, Protection, sizeof (Protection), Res);
            if Res <> sizeof (Protection) then
              Error := TRUE;
          end;
        end;
        
        HPart := HPart^.Next;
      end;
    end;
    
    HDriv := HDriv^.Next;
  end;
  
  Close (Cfg);
  
  if Error then
    Res := MessageBox (BOX_WARN_errFile_HDR, 
      BOX_WARN + BOX_WARN_errFile_wrXCF, 
      ButtonOK or ButtonDefOK)
  else
    MenSaveConfig := TRUE;
end;

{$IFDEF NOFARCALLS}
{$UNDEF NOFARCALLS}
{$F-}
{$ENDIF}

{HauptmenÅ}
procedure MainMenu (Drive, Pos: integer; PTable: TPTable);
var
  Men, Res   :integer;
  MEnt, MHlp :array[0..19] of TMenuStr;
  MAct       :array[0..19] of TMenuFunc;
  DoExit     :boolean;
  UpdBM      :integer;
  CRTBuffer  :PCRTBuffer;
begin
  {Menuaufbau ermitteln}
  Men := 0;
  
  {Ist es eine Partition oder ein freier Bereich?}
  if PTable[Pos]^.PartStat in [PartStatPri, PartStatAct, PartStatLog] then begin 
    MEnt[Men] := MEN_delPart;
    MHlp[Men] := '[DELETEPART]';
    MAct[Men] := MenDeletePart;
    Inc (Men);
    
    {Ist die Partition inaktiv?}
    if (PTable[Pos]^.PartStat = PartStatPri) and (Drive = FirstDrive) then begin 
      MEnt[Men] := MEN_actPart;
      MHlp[Men] := '[ACTIVATEPART]';
      MAct[Men] := MenActivatePart;
      Inc (Men);
    end;
    
    {Ist die Partition verdeckt?}
    if (PTable[Pos]^.PartStat in [PartStatPri, PartStatAct]) and
        (PTable[Pos]^.PartType in [$11, $14, $16, $17, $1B, $1C, $1E]) then begin
      MEnt[Men] := MEN_showPart;
      MHlp[Men] := '[SELECTDRIVE]';
      MAct[Men] := MenSelectPart;
      Inc (Men);
    end;

    {Ist die Partition nicht verdeckt?}
    if (PTable[Pos]^.PartStat in [PartStatPri, PartStatAct]) and
        (PTable[Pos]^.PartType in [$01, $04, $06, $07, $0B, $0C, $0E]) then begin
      MEnt[Men] := MEN_hidePart;
      MHlp[Men] := '[SELECTDRIVE]';
      MAct[Men] := MenHidePart;
      Inc (Men);
    end;

    {Partitionstyp Ñndern}
    MEnt[Men] := MEN_chngPTyp + ChrArrow;
    MHlp[Men] := '[CHANGEPARTTYPE]';
    MAct[Men] := MenChangePartType;
    Inc (Men);
    
    {Trennzeile}
    MEnt[Men] := '';
    Inc (Men);
    
    {Freier Bereich}
  end
  else if PTable[Pos]^.PartStat <> PartStatUnusable then begin 
    if PTable[Pos]^.PartStat = PartStatFreePriLog then
      MEnt[Men] := MEN_newPart + ChrArrow
    else
      MEnt[Men] := MEN_newPart;
    MHlp[Men] := '[CREATEPART]';
    MAct[Men] := MenCreatePart;
    Inc (Men);
    
    {Trennzeile}
    MEnt[Men] := '';
    Inc (Men);
  end;
  
  {Ist der Bootmanager installiert?}
  if BMIsInst then begin 
    {Ist es auch kein freier Bereich?}
    if PTable[Pos]^.PartStat in [PartStatPri, PartStatAct, PartStatLog] then begin 
      {Ist die gewÑhlte Partition noch nicht im MenÅ?}
      if (PTable[Pos]^.PartEntr = PartEntrNone) and (InfoEntr < MaxEntries) then begin 
        MEnt[Men] := MEN_newEntr;
        MHlp[Men] := '[INSERTENTR]';
        MAct[Men] := MenInsertEntry;
        Inc (Men)
      end
      else begin 
        MEnt[Men] := MEN_delEntr;
        MHlp[Men] := '[DELETEENTR]';
        MAct[Men] := MenDeleteEntry;
        Inc (Men);
        
        MEnt[Men] := MEN_chngEntr;
        MHlp[Men] := '[CHANGENAME]';
        MAct[Men] := MenChangeEntry;
        Inc (Men);
        
        {Partitionspa·wort}
        if BMHasPWDs then begin 
          if PTable[Pos]^.PartPWD[0] <> #0 then
            MEnt[Men] := MEN_chngPartPass
          else
            MEnt[Men] := MEN_setPartPass;
          MHlp[Men] := '[CHANGEPASSWORD]';
          MAct[Men] := MenChangePassword;
          Inc (Men);
        end
      end;
      
      {Trennzeile}
      MEnt[Men] := '';
      Inc (Men);
    end;
    
    MEnt[Men] := MEN_uninBMgr;
    MHlp[Men] := '[UNINSTALL]';
    MAct[Men] := MenUninstallBM;
    Inc (Men);
    
    if not BMNewInst then begin 
      UpdBM := Men;
      MHlp[Men] := '[UPDATE]';
      MAct[Men] := MenUpdateBM;
      Inc (Men);
    end;
    
    MEnt[Men] := MEN_setBVal + ChrArrow;
    MHlp[Men] := '[BOOTMANAGER]';
    MAct[Men] := MenSetStartVals;
    Inc (Men);
    
  end
  else begin
    MEnt[Men] := MEN_instBMgr;
    MHlp[Men] := '[INSTALL]';
    MAct[Men] := MenInstallBM;
    Inc (Men)
  end;
  
  MEnt[Men] := '';
  Inc (Men);
  
  MEnt[Men] := MEN_loadConf;
  MHlp[Men] := '[LOADSAVE]';
  MAct[Men] := MenLoadConfig;
  Inc (Men);
  
  MEnt[Men] := MEN_saveConf;
  MHlp[Men] := '[LOADSAVE]';
  MAct[Men] := MenSaveConfig;
  Inc (Men);
  
  Window (1, 1, 80, 25);
  DisplayStatusBar (WIN_MENU_STAT);
  
  CRTBuffer := nil;
  DoExit := FALSE;
  Res := 1;
  repeat
    if BMIsInst and not BMNewInst then
      if BMUpdate then
        MEnt[UpdBM] := ChrCheck + MEN_updBMgr
      else
        MEnt[UpdBM] := MEN_updBMgr;

    Res := DoMenu (3, 3, Men, Res - 1, MEN_MAIN_HDR, MEnt, MHlp, CRTBuffer);
    if Res <> 0 then
      DoExit := MAct[Res - 1] (Drive, Pos, TRUE);
  until DoExit or (Res = 0) ;

  RestoreWindow (CRTBuffer, FALSE);
  Window (1, 1, 80, 25);
  DisplayStatusBar (WIN_MAIN_STAT);
  Window (2, 5, 79, 23);
end;


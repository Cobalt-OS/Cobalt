{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XFDISK.PAS
    Main Program

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

program XFDisk;

uses
  DOS, CRT, DiskCtrl, Strings, Misc;

{$I XTYPES.INC}                     {Typdefinitionen}
{$I LANGUAGE.INC}                   {Sprache}

const
  {Versionsinformationen}
  VerInfo   :TVerInfo = (Tag:   BETA;
    Prog:  1;
    Major: 0;
    Minor: 9;
    Patch: 3;
    Build: 325);

var
  {Globale Variablen}
  DriveChain  :PDriveChain;
  PhyDrives   :integer;
{  LogDrives   :integer;
  LogStart    :integer;}
  {HasChanged  :boolean; UM: aufgeteilt auf jedes Laufwerk einzeln}
  WriteBM     :boolean; {Einstellungen des BM wurden geÑndert}
  BMIsInst    :boolean;
  BMHasPWDs   :boolean;
  BMUpdate    :boolean;
  BMNewInst   :boolean;
  BMPrevInst  :boolean; {Bootmanager bereits installiert}
  BMPrevVer   :integer;
  Timeout     :integer;
  TimeHndl    :integer;
  ClearScrn   :boolean;
  BootLast    :boolean;
  LastEntry   :integer;
  SimpleMenus :boolean;
  MasterPWD   :array[0..16] of char;
  FloppyPWD   :array[0..16] of char;
  KnowsMaster :boolean;
  InfoEntr    :integer;
  MenEntr     :integer;
  SuppBlind   :boolean;                 {Cursor Anzeigen}
  NoWin9xPTyp :boolean;   {Don't use Win9x Part types}
  UseOS2,
  UseWinNT,
  UseLinux    :boolean;
  Autohide    :boolean; {hide multiple primary partitions automatically?}
  PQMagicComp :boolean;
  SortPartTable:boolean;
  CString     :string[74];
  Parameter   :string;
  XFDiskPath  :string;
  XHelpPath   :string;
  ConfigFile  :string;
  SafetyBuf   :pointer;

const BMCleanOldVersion :boolean = false;

procedure ExitProgram (Fail: TExit); forward;
procedure MemoryError; forward;
procedure UnloadTable (DriveChain: PDriveChain); forward;

{$I XFDISK.INC}     {Size of XFDISK.EXE _after_ compilation;-) }
{$I XIO.INC}        {Input/Output Routinen}
{$I XBOOTM.INC}     {BootManager}
{$I XPART.INC}      {Partitionsverwaltung}
{$I XMENU.INC}      {HauptmenÅ}

var
  {Lokale Variablen des Hauptprogrammes}
  CRTBuffer        :PCRTBuffer;
  Cnt, Key, CPos   :integer;
  CDrv, MDrv, ODrv :integer;
  CEnt, MEnt, OEnt :integer;
  CAct, MAct, OAct :integer;
  PTable           :TPTable;
  Res              :integer;
  WriteMBROnly,
  WaitForKey,
  ShowBuild        :boolean;
  IniFile          :text;
  HDriv            :PDriveChain;
  i                :integer;
  Options          :string;

{Mit Fehler beenden}
procedure ExitProgram (Fail: TExit);
begin
  RestoreWindow (CRTBuffer, TRUE);
  Halt (Ord (Fail));
end;

procedure PrepareMemoryError;
begin
  if MaxAvail < SafetyBufferSize then begin
    RestoreWindow (CRTBuffer, TRUE);
    WriteLn (STR_ERROR_MEMORY_LOW);
    Halt (1);
  end;
  GetMem (SafetyBuf, SafetyBufferSize);
end;

procedure MemoryError;
begin
  if SafetyBuf = nil then begin 
    RestoreWindow (CRTBuffer, TRUE);
    WriteLn (STR_ERROR_MEMORY_LOW);
    Halt (1);
  end;
  FreeMem (SafetyBuf, SafetyBufferSize);
  SafetyBuf := nil;

  MessageBox (BOX_WARN_errMEM_HDR,
    BOX_WARN + BOX_WARN_errMEM_LOW,
    ButtonOK or ButtonDefOK);
end;

begin
  CheckBreak := FALSE;
  CRTBuffer := nil;
  ConfigFile := 'A:\CONFIG.XCF';

  {Versionsanzeige}
  CString := STR_PROG_NAME + ' Version ' +
    Int2Str (VerInfo.Major, 1) + '.' +
    Int2Str (VerInfo.Minor, 1) + '.' +
    Int2Str (VerInfo.Patch, 1);

  case VerInfo.Tag of
    ALPHA:
      CString := CString + ' ' + STR_PROG_ALPHA
                 + ' (Build ' + Int2Str (VerInfo.Build, 3) + ')';

    BETA:
      CString := CString + ' ' + STR_PROG_BETA;

    GAMMA:
      CString := CString + ' ' + STR_PROG_GAMMA;

    RELEASE:
      CString := CString + ' ' + STR_PROG_REL;
  end;

  CString := CString + ' ' + STR_PROG_LANG;
  WriteLn (CString);
  WriteLn (STR_PROG_COPY);

  {VersionsprÅfung}
  {$IFNDEF DEBUG}
  Write (STR_PROG_CHKS);
  XFDISKPath := ParamStr (0);
  XHELPPath := Copy (XFDISKPath, 1, length (XFDISKPath) - 3) + 'XFH';
  if not GetCheck (XFDISKPath) then begin
    WriteLn;
    WriteLn (STR_ERROR_CHECKSUM);
    Halt (1)
  end;
  GotoXY (1, wherey); ClrEol;
  {$ENDIF}

  {Speicherfehler Meldung vorbereiten}
  PrepareMemoryError;

  {Parameter}
  WaitForKey := FALSE;
  ShowBuild := FALSE;
  WriteMBROnly := FALSE;
  NoWin9xPTyp := FALSE;
  SuppBlind := FALSE;
  UseOS2 := FALSE;
  UseWinNT := FALSE;
  UseLinux := FALSE;
  AutoHide := false;
  PQMagicComp := TRUE; {fix}
  SortPartTable := false;

  Assign (IniFile, Copy (XFDISKPath, 1, length (XFDISKPath) - 3) + 'INI');
  Reset (IniFile);
  if IOResult = 0 then
    Cnt := 0
  else
    Cnt := 1;

  Options := '';
  while Cnt <= ParamCount do begin
    if Cnt = 0 then begin
      Parameter := '';
      while (not EOF (IniFile)) and (Parameter = '') do begin
        ReadLn (IniFile, Parameter);
        if Copy (Parameter, 1, 1) = '#' then Parameter := '';
        while Copy (Parameter, Length (Parameter), 1) = ' ' do
          Dec (Parameter[0]);
      end;

      if EOF (IniFile) then begin
        Close (IniFile);
        Cnt := 1;

        if Parameter = '' then
          Continue;
      end;
    end
    else begin
      Parameter := UpStr (ParamStr (Cnt));
      Inc (Cnt);
    end;

    {HardCursor}
    if (Parameter = '/BLIND') and not SuppBlind then begin
      SuppBlind := TRUE;
      Options := Options + BOX_INFO_startup3 + STR_PROG_BLIND;
    end

    {No Color}
    else if (Parameter = '/NOCOLOR') or (Parameter = '/MONO') and not CRTIsMono then begin
      CRTIsMono := TRUE;
      CRTSegment := $B000;
      Options := Options + BOX_INFO_startup3 + STR_PROG_NOCOLOR;
    end

    {hide primary partitions of each other}
    else if (Parameter = '/AUTOHIDE') and not AutoHide then begin
      AutoHide := True;
      Options := Options + BOX_INFO_startup3 + STR_PROG_AUTOHIDE;
    end

    {sort entries in partition table}
    else if (Parameter = '/SORT') and not SortPartTable then begin
      SortPartTable := True;
      Options := Options + BOX_INFO_startup3 + STR_PROG_SORT;
    end

    {do not use Win9x Ext Part Type 0F}
    else if (Parameter = '/NOWIN9X') and not NoWin9xPTyp then begin
      NoWin9xPTyp := TRUE;
      Options := Options + BOX_INFO_startup3 + STR_PROG_NOWIN9XPTYP;
    end

    {System choice}
    else if (Parameter = '/OS2') and not UseOS2 then begin
      UseOS2 := TRUE; UseWinNT := FALSE; UseLinux := FALSE;
      NoWin9xPTyp := TRUE;
      Options := Options + BOX_INFO_startup3 + STR_PROG_USEOS2;
      Options := Options + BOX_INFO_startup3 + STR_PROG_NOWIN9XPTYP;
    end
    else if (Parameter = '/WINNT') and not UseWinNT then begin
      UseOS2 := FALSE; UseWinNT := TRUE; UseLinux := FALSE;
      Options := Options + BOX_INFO_startup3 + STR_PROG_USEWINNT;
    end
    else if (Parameter = '/LINUX') and not UseLinux then begin
      UseOS2 := FALSE; UseWinNT := FALSE; UseLinux := TRUE;
      SortPartTable := False;
      Options := Options + BOX_INFO_startup3 + STR_PROG_USELINUX;
    end

    {MBR erneuern}
    else if (Parameter = '/MBR') and not WriteMBROnly then begin
      WriteMBROnly := TRUE;
      WriteLn (STR_PROG_MBRONLY);
    end

    {Build Information anzeigen}
    else if (Parameter = '/BUILD') and not ShowBuild then begin
      ShowBuild := TRUE;
      WriteLn ('- Build ', VerInfo.Build);
    end

    {complain}
    else begin
      WriteLn (STR_ERROR_INVALID_PARAM, ' ', Parameter);
      Halt (1);
    end;
  end;

  if WaitForKey then begin
    Write (STR_PROG_WAITKEY);
    ReadKey;
    GotoXY (1, WhereY); ClrEOL;
  end;

  if not SaveWindow (CRTBuffer) then begin
    MemoryError;
    ExitProgram (FAILURE);
  end;

  if not SuppBlind then
    HideCursor;

  {Abfrage ob Festplatte vorhanden}
  PhyDrives := GetNumberOfDrives;
  if PhyDrives = 0 then begin
    MessageBox (BOX_WARN_noHDD_HDR,
      BOX_WARN + BOX_WARN_noHDD,
      ButtonOK or ButtonDefOK);

    ExitProgram (FAILURE);
  end;

  MasterPWD[0] := #0;
  FloppyPWD[0] := #0;

  DriveChain := nil;
  if not LoadTable then
    ExitProgram (FAILURE);

  {Soll nur der MBR geschrieben werden?}
  if WriteMBROnly then begin
    for Cnt := 1 to PhyDrives do
      if not WriteMBR (FirstDriveIndex + Cnt) then
        ExitProgram (FAILURE);

    ExitProgram (SUCCESS);
  end;

  {Bildschirmaufbau}
  SetupScreen;

  if not SuppBlind then
    HideCursor;

  If Options <> '' then
    Res := MessageBox (BOX_INFO_startup1,
      BOX_INFO + BOX_INFO_startup2 + Options,
    ButtonOK);

  DisplayTable (FirstDrive, 0);

  {Auswahl}
  MDrv := FirstDriveIndex + PhyDrives;
  CDrv := FirstDrive;

  CEnt := 0;
  CAct := 0;
  CPos := 1;

  MEnt := GetEntries (CDrv, PTable);
  MAct := MaxLines + 1;

  ODrv := CDrv;
  OEnt := CEnt;
  OAct := CAct;

  DisplayEntry (PTable[CEnt], CAct, TRUE);

  repeat
    Key := Ord (ReadKey);
    if Key = 0 then
      Key := Ord (ReadKey) + KeyExtended;

    case Key of
      {Pfeil nach oben}
      KeyArrowUp: begin
        if CEnt > 0 then
          Dec (CEnt);
      end;

      {Pfeil nach unten}
      KeyArrowDown: begin 
        if CEnt < MEnt - 1 then
          Inc (CEnt);
      end;
      
      {Enter - Auswahl}
      KeyEnter: begin 
        MainMenu (CDrv, CEnt, PTable);
        MEnt := GetEntries (CDrv, PTable);
        
        if CEnt > MEnt - 1 then begin 
          Dec (CEnt);
          Dec (CAct)
        end;

        DisplayTable (CDrv, CEnt - CAct);
        
        OEnt := CEnt;
        OAct := CAct;
        
        DisplayEntry (PTable[CEnt], CAct, TRUE)
      end;
      
      {F1 - Hilfe}
      KeyF1: begin 
        ExecHelp ('[ALLGEMEIN]');
        
        DisplayTable (CDrv, CEnt - CAct);
        DisplayEntry (PTable[CEnt], CAct, TRUE)
      end;
      
      {Tab - nÑchste Festplatte}
      KeyTab: begin 
        if CDrv = MDrv then
          CDrv := FirstDrive
        else
          Inc (CDrv);
        
        DisplayEntry (PTable[OEnt], OAct, FALSE);

        CEnt := 0;
        CAct := 0;
        CPos := 1;

        MEnt := GetEntries (CDrv, PTable);
        MAct := MaxLines + 1;

        ODrv := CDrv;
        OEnt := CEnt;
        OAct := CAct;

        DisplayTable (CDrv, 0);
        DisplayEntry (PTable[CEnt], CAct, TRUE);
      end;

      {F3 - Beenden}
      KeyF3: begin
        {BootManager schreiben...}
        if BMIsInst and (InfoEntr = 0) then
          {BootManager mu· mindestens einen Eintrag enthalten}
          MenUninstallBM (CDrv, CEnt, FALSE);

        if WriteBM or BMUpdate then begin
          Res := MessageBox (BOX_QUERY_wrtBMgr_HDR,
            BOX_QUERY + BOX_QUERY_wrtBMgr,
            ButtonYesNoCancel or ButtonDefYes);

          {BootManager schreiben}
          if Res = ButtonResYes then begin
            {Partitionstabelle schreiben}
            if not MenuWriteBM then
              ExitProgram (FAILURE);
          end;
        end else Res := ButtonResYes;

        {Partitionstabellen schreiben}
        if Res = ButtonResYes then begin
          HDriv := DriveChain;
          i := 0; {do not need to reboot}
          while (HDriv <> nil) and (Res <> ButtonResCancel) do begin
            {énderungen vorgenommen?}
            if HDriv^.HasChanged then begin
              Res := MessageBox (BOX_QUERY_wrtPTbl_HDR,
                BOX_QUERY + BOX_QUERY_wrtPTbl1
                + Int2Str(HDriv^.Drive - FirstDriveIndex, 1)
                + BOX_QUERY_wrtPTbl2,
                ButtonYesNoCancel or ButtonDefNo);

              {énderungen sichern}
              if Res = ButtonResYes then begin
                {Partitionstabelle schreiben}
                i := 1;
                if not WriteTable(HDriv) then
                  ExitProgram (FAILURE);
              end;
            end;
            HDriv := HDriv^.Next;
          end;

          if (Res <> ButtonResCancel) and (i = 1) then begin
            Res := MessageBox (BOX_QUERY_rsetPC_HDR,
              BOX_QUERY + BOX_QUERY_rsetPC,
              ButtonYesNo or ButtonDefYes);

            {Soll der Reboot durchgefÅhrt werden?}
            if Res = ButtonResYes then begin
              UnloadTable (DriveChain);
              ReBoot;
            end else
              ExitProgram (SUCCESS);
          end;
        end;

        if Res <> ButtonResCancel then
          ExitProgram (SUCCESS);

        DisplayTable (CDrv, CEnt - CAct);
        DisplayEntry (PTable[CEnt], CAct, TRUE);
      end;
    end;

    {Position hat sich geÑndert?}
    if CEnt <> OEnt then begin
      if CEnt <= MaxLines then
        CAct := CEnt;

      if (CEnt > MaxLines) or (OEnt > MaxLines) then begin
        CAct := MaxLines;
        DisplayTable (CDrv, CEnt - CAct)
      end;

      DisplayEntry (PTable[OEnt], OAct, FALSE);
      DisplayEntry (PTable[CEnt], CAct, TRUE);
      OAct := CAct; OEnt := CEnt
    end;
  until Key = 0 ;
end.

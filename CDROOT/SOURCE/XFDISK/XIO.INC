{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XIO.INC
    Input/Output Functions and Procedures

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

{Alarm}
procedure Beep (BeepType: TBeep);
var
  Count :integer;
begin
  case BeepType of
    SINGLE: begin 
      Sound (880);
      Wait (50);
      NoSound;
    end;
    
    TRIPLE: begin 
      Sound (440);
      Wait (500);
      NoSound;
    end;
    
    SOS: begin 
      Sound (440);
      Wait (500);
      Sound (220);
      Wait (500);
      NoSound;
    end;
  end;
  
  while KeyPressed do
    ReadKey;
end;

{InfoBox ausgeben}
function CreateInfoBox (Head, Message: string) :pointer;
var
  Len, Act, Old, Cnt :integer;
  XPs, YPs, Psn      :integer;
  Lines              :array[0..10] of string;
  CRTBuffer          :PCRTBuffer;
begin
  Len := Length (Head) + 2;
  Lines[0] := Message;
  Cnt := 0;
  repeat
    Psn := Pos ('|', Lines[Cnt]);
    Act := Length (Lines[Cnt]);
    if Psn <> 0 then begin 
      Lines[Cnt + 1] := Copy (Lines[Cnt], Psn + 1, Act - Psn + 1);
      Lines[Cnt] := Copy (Lines[Cnt], 1, Psn - 1);
      Inc (Psn);
    end
    else
      Lines[Cnt] := Copy (Lines[Cnt], 1, Act);
    
    Len := Max (Length (Lines[Cnt]) + 4, Len);
    Inc (Cnt);
  until (Psn = 0) or (Cnt = 10) ;
  
  XPs := round ((80 - Len) / 2);
  YPs := round ((24 - Cnt - 5) / 2);
  
  {Messagebox ”ffnen}
  Window (XPs , YPs, XPs + Len + 1, YPs + Cnt + 3);
  if not SaveWindow (CRTBuffer) then begin 
    CreateInfoBox := nil;
    Exit;
  end;
  
  if CRTIsMono then
    SetColor (MonStatItemVG, MonStatBG)
  else
    SetColor (ColStatItemVG, ColStatBG);
  ClrScr;
  
  
  GotoXY (1, 3 + Cnt);
  Write ('ศ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ผ');
  
  GotoXY (1, 1);
  Write ('ษ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ป');
  
  InsLine;
  
  for Psn := 2 to 3 + Cnt do begin 
    GotoXY (1, Psn);
    Write ('บ');
    GotoXY (Len + 2, Psn);
    Write ('บ');
  end;
  
  GotoXY (2 + round ((Len - Length (Head) - 2) / 2), 1);
  Write (' ', Head, ' ');
  
  if CRTIsMono then
    SetColor (MonWinItemVG, MonWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG);
  
  Window (XPs + 1, YPs + 1, XPs + Len, YPs + Cnt + 2);
  ClrScr;
  for Psn := 1 to Cnt do begin 
    GotoXY (3, Psn + 1);
    Write (Lines[Psn - 1]);
    GotoXY (3, Psn + 1);
  end;
  
  CreateInfoBox := CRTBuffer;
end;

{InfoBox entfernen}
procedure DestroyInfoBox (var CRTBuffer: pointer);
begin
  RestoreWindow (PCRTBuffer (CRTBuffer), FALSE)
end;


{MessageBox ausgeben}
function MessageBox (Head, Message: string; MBox: integer) :integer;
var
  Key, Btn, Def, Cnt :integer;
  Len, Act, Old      :integer;
  XPs, YPs, Psn      :integer;
  Lines              :array[0..10] of string;
  ButtonText         :array[0..2] of string[10];
  CRTBuffer          :PCRTBuffer;
begin
  while KeyPressed do
    ReadKey;
  
  {Lnge und Position der Box berechnen}
  case MBox and ButtonMask of
    {Nur OK-Button}
    ButtonOK: begin 
      ButtonText[0] := BTN_OK;
      Btn := 1
    end;
    
    {Ja, Nein Buttons}
    ButtonYesNo: begin 
      ButtonText[0] := BTN_YES;
      ButtonText[1] := BTN_NO;
      Btn := 2
    end;
    
    {Ja, Nein, Abbruch Buttons}
    ButtonYesNoCancel: begin 
      ButtonText[0] := BTN_YES;
      ButtonText[1] := BTN_NO;
      ButtonText[2] := BTN_CAN;
      Btn := 3
    end;
  end;
  
  {DefaultButton}
  Def := (MBox and ButtonDefMask) div ButtonDefDiv;
  if Def >= Btn then
    Def := 0;
  
  Len := Length (Head) + 2;
  Lines[0] := Message;
  Cnt := 0;
  repeat
    Psn := Pos ('|', Lines[Cnt]);
    Act := Length (Lines[Cnt]);
    if Psn <> 0 then begin 
      Lines[Cnt + 1] := Copy (Lines[Cnt], Psn + 1, Act - Psn + 1);
      Lines[Cnt] := Copy (Lines[Cnt], 1, Psn - 1);
      Inc (Psn);
    end
    else
      Lines[Cnt] := Copy (Lines[Cnt], 1, Act);
    
    Len := Max (Length (Lines[Cnt]) + 4, Len);
    Inc (Cnt);
  until (Psn = 0) or (Cnt = 10) ;
  
  Len := Max (Btn * 10, Len);
  XPs := round ((80 - Len) / 2);
  YPs := round ((24 - Cnt - 5) / 2);
  
  {Messagebox ”ffnen}
  Window (XPs, YPs, XPs + Len + 1, YPs + Cnt + 5);
  if not SaveWindow (CRTBuffer) then begin 
    MessageBox := ButtonResCancel;
    MemoryError;
    Exit;
  end;
  
  if CRTIsMono then
    SetColor (MonStatItemVG, MonStatBG)
  else
    SetColor (ColStatItemVG, ColStatBG);
  ClrScr;
  
  GotoXY (1, 5 + Cnt);
  Write ('ศ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ผ');
  
  GotoXY (1, 1);
  Write ('ษ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ป');
  
  InsLine;
  
  for Psn := 2 to 5 + Cnt do begin 
    GotoXY (1, Psn);
    Write ('บ');
    GotoXY (Len + 2, Psn);
    Write ('บ');
  end;
  
  GotoXY (2 + round ((Len - Length (Head) - 2) / 2), 1);
  Write (' ', Head, ' ');
  
  if CRTIsMono then
    SetColor (MonWinItemVG, MonWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG);
  
  Window (XPs + 1, YPs + 1, XPs + Len, YPs + Cnt + 4);
  ClrScr;
  for Psn := 1 to Cnt do begin 
    GotoXY (3, Psn + 1);
    Write (Lines[Psn - 1]);
  end;
  
  {Buttons}
  GotoXY ((Len div 2) - 5 * Btn, Cnt + 3);
  Write ('|');
  for Psn := 1 to Btn do
    Write (ButtonText[Psn - 1], '|');
  
  if SuppBlind then
    Beep (SINGLE);
  
  Act := Def;
  Old := Act;
  Psn := (Len div 2) - 5 * Btn;
  repeat
    if CRTIsMono then
      SetColor (MonSelItemVG, MonSelBG)
    else
      SetColor (ColSelItemVG, ColSelBG);
    
    GotoXY (Psn + Act * 10 + 1, Cnt + 3);
    Write (ButtonText[Act]);
    GotoXY (Psn + Act * 10 + 1, Cnt + 3);
    
    Key := Ord (ReadKey);
    if Key = 0 then
      Key := Ord (ReadKey) + KeyExtended;
    
    case Key of
      {Pfeil nach links}
      KeyArrowLeft: begin 
        if Act > 0 then
          Dec (Act);
      end;
      
      {Pfeil nach rechts}
      KeyArrowRight: begin 
        if Act < Btn - 1 then
          Inc (Act);
      end;
      
      {Tabulator}
      KeyTab: begin 
        if Act < Btn - 1 then
          Inc (Act)
        else
          Act := 0;
      end;
    end;
    
    if CRTIsMono then
      SetColor (MonWinItemVG, MonWinBG)
    else
      SetColor (ColWinItemVG, ColWinBG);
    
    GotoXY (Psn + Old * 10 + 1, Cnt + 3);
    Write (ButtonText[Old]);
    Old := Act;
  until (Key = KeyEnter) or (Key = KeyEscape) ;
  
  if Key = KeyEscape then
    MessageBox := ButtonResCancel
  else
    MessageBox := Act;
  
  RestoreWindow (CRTBuffer, FALSE)
end;

{InputBox ausgeben}
function InputBox (Head, Message: string; 
                  var IOStr: string; 
                  MaxLen: integer; 
                  Align: TAlign; 
                  Crypt: boolean) :integer;
var
  Len, Psn, Key, Act :integer;
  XPs, YPs, Cnt      :integer;
  InsMode            :boolean;
  Lines              :array[0..10] of string;
  LeftStr, RightStr  :string[64];
  CRTBuffer          :PCRTBuffer;
begin
  while KeyPressed do
    ReadKey;
  
  {Lnge und Position der Box berechnen}
  Len := Length (Head) + 2;
  Lines[0] := Message;
  Cnt := 0;
  repeat
    Psn := Pos ('|', Lines[Cnt]);
    Act := Length (Lines[Cnt]);
    if Psn <> 0 then begin 
      Lines[Cnt + 1] := Copy (Lines[Cnt], Psn + 1, Act - Psn + 1);
      Lines[Cnt] := Copy (Lines[Cnt], 1, Psn - 1);
      Inc (Psn);
    end
    else
      Lines[Cnt] := Copy (Lines[Cnt], 1, Act);
    
    Len := Max (Length (Lines[Cnt]) + 4, Len);
    Inc (Cnt);
  until (Psn = 0) or (Cnt = 10) ;
  
  Len := Max (MaxLen + 6, Len);
  XPs := round ((80 - Len) / 2);
  YPs := round ((24 - Cnt - 5) / 2);
  
  if Crypt then
    LeftStr := ''
  else
    LeftStr := IOStr;
  RightStr := '';
  
  {Inputbox ”ffnen}
  Window (XPs , YPs, XPs + Len + 1, YPs + Cnt + 5);
  if not SaveWindow (CRTBuffer) then begin 
    InputBox := ButtonResCancel;
    MemoryError;
    Exit;
  end;
  
  if CRTIsMono then
    SetColor (MonStatItemVG, MonStatBG)
  else
    SetColor (ColStatItemVG, ColStatBG);
  ClrScr;
  
  GotoXY (1, Cnt + 5);
  Write ('ศ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ผ');
  
  GotoXY (1, 1);
  Write ('ษ');
  for Act := 2 to Len + 1 do
    Write ('อ'); Write ('ป');
  
  InsLine;
  
  for Act := 2 to Cnt + 5 do begin 
    GotoXY (1, Act);
    Write ('บ');
    GotoXY (Len + 2, Act);
    Write ('บ');
  end;
  
  GotoXY (2 + round ((Len - Length (Head) - 2) / 2), 1);
  Write (' ', Head, ' ');
  
  if CRTIsMono then
    SetColor (MonWinItemVG, MonWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG);
  
  Window (XPs + 1, YPs + 1, XPs + Len, YPs + Cnt + 4);
  ClrScr;
  
  for Psn := 1 to Cnt do begin 
    GotoXY (3, Psn + 1);
    Write (Lines[Psn - 1]);
  end;
  
  if CRTIsMono then
    SetColor (MonSelItemVG, MonSelBG)
  else
    SetColor (ColSelItemVG, ColSelBG);
  
  Window (XPs + 2, YPs + Cnt + 3, XPs + MaxLen + 5, YPs + Cnt + 3);
  ClrScr;
  
  if SuppBlind then
    Beep (SINGLE);
  
  InsMode := TRUE;
  ShowCursor (CursorIns);
  Cnt := Length (LeftStr);
  Act := 0;
  repeat
    if Align = LEFT then begin 
      GotoXY (3, 1);
      if Crypt then
        Write (StrFill ('*', Length (LeftStr) + Length (RightStr)))
      else
        Write (LeftStr, RightStr);
      ClrEol;
      GotoXY (3 + Cnt, 1);
    end
    else begin 
      GotoXY (3, 1);
      ClrEOL;
      GotoXY (MaxLen - (Length (LeftStr) + Length (RightStr)) + 3, 1);
      if Crypt then
        Write (StrFill ('*', Length (LeftStr) + Length (RightStr)))
      else
        Write (LeftStr, RightStr);
      GotoXY (MaxLen - Length (RightStr) + 3, 1);
    end;
    
    Key := Ord (ReadKey);
    if Key = 0 then
      Key := Ord (ReadKey) + KeyExtended;
    
    {Eingabezeichen verarbeiten}
    if (Key >= KeyMinChr) and (Key <= KeyMaxChr) then begin 
      {Erstes Zeichen Eingabe? Dann alten Text l”schen}
      if Act = 0 then begin 
        LeftStr := Chr (Key); Cnt := 1;
      end
      else begin 
        {Insert Mode}
        if InsMode then begin 
          if Length (LeftStr) + Length (RightStr) < MaxLen then begin 
            LeftStr := LeftStr + Chr (Key);
            Inc (Cnt);
          end
          else
            Beep (SINGLE);
          {Overwrite Mode}
        end
        else begin 
          {Ist auf der rechten Seite noch ein Zeichen zum berschreiben?}
          if Length (RightStr) > 0 then begin 
            LeftStr := LeftStr + Chr (Key);
            RightStr := Copy (RightStr, 2, Length (RightStr) - 1);
            Inc (Cnt)
          end
          else begin 
            if Length (LeftStr) < MaxLen then begin 
              LeftStr := LeftStr + Chr (Key);
              Inc (Cnt)
            end
            else
              Beep (SINGLE);
          end;
        end;
      end;
    end
    else begin 
      case Key of
        {Backspace}
        KeyBackspace: begin 
          if Length (LeftStr) > 0 then begin 
            LeftStr := Copy (LeftStr, 1, Length (LeftStr) - 1);
            Dec (Cnt)
          end
          else
            Beep (SINGLE);
        end;
        
        {Delete}
        KeyDelete: begin 
          if Length (RightStr) > 0 then
            RightStr := Copy (RightStr, 2, Length (RightStr) - 1)
          else
            Beep (SINGLE);
        end;
        
        {Pfeil nach links}
        KeyArrowLeft: begin 
          if Length (LeftStr) > 0 then begin 
            RightStr := Copy (LeftStr, Length (LeftStr), 1) + RightStr;
            LeftStr := Copy (LeftStr, 1, Length (LeftStr) - 1);
            Dec (Cnt)
          end
          else
            Beep (SINGLE);
        end;
        
        {Pfeil nach rechts}
        KeyArrowRight: begin 
          if Length (RightStr) > 0 then begin 
            LeftStr := LeftStr + Copy (RightStr, 1, 1);
            RightStr := Copy (RightStr, 2, Length (RightStr) - 1);
            Inc (Cnt)
          end
          else
            Beep (SINGLE);
        end;
        
        {End}
        KeyEnd: begin 
          if Length (RightStr) > 0 then begin 
            LeftStr := LeftStr + RightStr;
            RightStr := '';
            Cnt := Length (LeftStr);
          end
          else
            Beep (SINGLE);
        end;
        
        {Home}
        KeyHome: begin 
          if Length (LeftStr) > 0 then begin 
            RightStr := LeftStr + RightStr;
            LeftStr := '';
            Cnt := 0;
          end
          else
            Beep (SINGLE);
        end;
        
        {Toggle Insert/Overwrite Mode}
        KeyInsert: begin 
          InsMode := not InsMode;
          if InsMode then
            ShowCursor (CursorIns)
          else
            ShowCursor (CursorOvr);
        end;
      end;
    end;
    Act := Key;
  until (Key = KeyEnter) or (Key = KeyEscape) ;
  
  if Key = KeyEnter then begin 
    InputBox := ButtonResOK;
    IOStr := LeftStr + RightStr;
  end
  else
    InputBox := ButtonResCancel;
  
  if not SuppBlind then
    HideCursor
  else
    ShowCursor (CursorIns);
  
  RestoreWindow (CRTBuffer, FALSE)
end;

{Passwort crypten}
procedure ProtectPWD (Password: PChar; Len: integer);
var
  Cnt :integer;
begin
  for Cnt := 0 to 15 do begin 
    {$IFOPT R+}
    {$DEFINE RANGECHECK}
    {$R-}
    {$ENDIF}
    {$IFOPT Q+}
    {$DEFINE OVERFLOWCHECK}
    {$Q-}
    {$ENDIF}
    if Odd (Len) then
      Password[Cnt] := Chr (Ord (Password[Cnt]) xor Ord (PWDTable1[Cnt]))
    else
      Password[Cnt] := Chr (Ord (Password[Cnt]) xor Ord (PWDTable2[Cnt]));
    {$IFDEF RANGECHECK}
    {$UNDEF RANGECHECK}
    {$R+}
    {$ENDIF}
    {$IFDEF OVERFLOWCHECK}
    {$UNDEF OVERFLOWCHECK}
    {$Q+}
    {$ENDIF}
  end;
  
  Password[16] := #0;
end;

{Passwort verifizieren}
function VerifyPassword (Title: string; PWD: PChar) :boolean;
var
  Pass1, Pass2 :string[16];
  Res, Cnt     :integer;
  Success      :boolean;
begin
  Pass1 := StrPas (PWD);
  Pass2 := '';
  Cnt := MaxErrCount;
  Success := FALSE;
  repeat
    Res := InputBox (BOX_INP_confPass_PRE + Title + BOX_INP_confPass_POST, 
      BOX_INP_confPass, Pass2, 16, LEFT, TRUE);
    if Res = ButtonResOK then
      if Pass2 = Pass1 then
        Success := TRUE
      else
        Beep (TRIPLE);
    Dec (Cnt);
  until Success or (Res = ButtonResCancel) or (Cnt = 0) ;
  
  VerifyPassword := Success;
end;

{Passowort abfragen}
function GetPassword (Title: string; Password: PChar) :boolean;
var
  Pass1, Pass2, Pass3 :string[16];
  Cnt, Res            :integer;
  Success             :boolean;
begin
  while KeyPressed do
    ReadKey;
  
  GetPassword := FALSE;
  Pass1 := StrPas (Password);
  
  Pass2 := '';
  Cnt := MaxErrCount;
  Res := InputBox (BOX_INP_newPass_PRE + Title + BOX_INP_newPass_POST, 
    BOX_INP_newPass, Pass2, 16, LEFT, TRUE);
  
  if Res = ButtonResOK then begin 
    {Paแwort soll gel”scht werden...}
    if Pass2 = '' then begin 
      {Gibt's berhaupt ein Paแwort?}
      if Pass1 <> '' then begin 
        Res := MessageBox (BOX_QUERY_delPass_PRE + Title + BOX_QUERY_delPass_POST, 
          BOX_QUERY + BOX_QUERY_delPass, 
          ButtonYesNo or ButtonDefNo);
        
        {Yup. Wir wollen das Paแwort wirklich l”schen}
        if Res = ButtonResYes then begin 
          Pass3 := '';
          Success := FALSE;
          Cnt := MaxErrCount;
          repeat
            Res := InputBox (BOX_INP_confPass_PRE + Title + BOX_INP_confPass_POST, 
              BOX_INP_confPass, Pass3, 16, LEFT, TRUE);
            
            if Res = ButtonResOK then
              if Pass3 = Pass1 then
                Success := TRUE
              else
                Beep (TRIPLE);
            
            Dec (Cnt);
          until (Res = ButtonResCancel) or (Cnt = 0) or (Success) ;
          
          if Success then begin 
            Password[0] := #0;
            GetPassword := TRUE;
            MessageBox (BOX_INFO_delPass_PRE + Title + BOX_INFO_delPass_POST, 
              BOX_INFO + BOX_INFO_delPass, 
              ButtonOK or ButtonDefOk);
          end;
        end;
      end;
      
      {Das Paแwort soll festgelegt oder gendert werden}
    end
    else if Pass2 <> Pass1 then begin 
      Pass3 := '';
      Success := FALSE;
      Cnt := MaxErrCount;
      repeat
        Res := InputBox (BOX_INP_rtypPass_PRE + Title + BOX_INP_rtypPass_POST, 
          BOX_INP_rtypPass, Pass3, 16, LEFT, TRUE);
        
        if Res = ButtonResOK then
          if Pass3 = Pass2 then
            Success := TRUE
          else
            Beep (TRIPLE);
        
        Dec (Cnt);
      until (Res = ButtonResCancel) or (Cnt = 0) or (Success) ;
      
      {Yup. Wir wollen das Paแwort festlegen oder ndern}
      if Success then begin 
        {Gibt's ein Passwort? Dann ndern...}
        if Pass1 <> '' then begin 
          Pass3 := '';
          Success := FALSE;
          Cnt := MaxErrCount;
          repeat
            Res := InputBox (BOX_INP_confPass_PRE + Title + BOX_INP_confPass_POST, 
              BOX_INP_confPass, Pass3, 16, LEFT, TRUE);
            
            if Res = ButtonResOK then
              if Pass3 = Pass1 then
                Success := TRUE
              else
                Beep (TRIPLE);
            
            Dec (Cnt);
          until (Res = ButtonResCancel) or (Cnt = 3) or (Success) ;
          
          if Success then begin 
            StrPCopy (Password, Pass2);
            GetPassword := TRUE;
            MessageBox (BOX_INFO_chngPass_PRE + Title + BOX_INFO_chngPass_POST, 
              BOX_INFO + BOX_INFO_chngPass, 
              ButtonOK or ButtonDefOK);
          end;
          
          {Paแwort soll festlegen werden}
        end
        else begin 
          StrPCopy (Password, Pass2);
          GetPassword := TRUE;
          MessageBox (BOX_INFO_setPass_PRE + Title + BOX_INFO_setPass_POST, 
            BOX_INFO + BOX_INFO_setPass, 
            ButtonOK or ButtonDefOK);
        end;
      end;
    end;
  end;
end;

{Men anzeigen}
procedure DisplayStatusBar (Menu: string);
var
  Cnt    :integer;
  Hilite :boolean;
begin
  GotoXY (1, 25);
  
  Hilite := FALSE;
  if CRTIsMono then
    SetColor (MonStatItemVG, MonStatBG)
  else
    SetColor (ColStatItemVG, ColStatBG);
  
  for Cnt := 1 to Length (Menu) do begin 
    {Farbauswahl mit '~' toggeln...}
    if Menu[Cnt] = '~' then begin 
      Hilite := not Hilite;
      if Hilite then
        if CRTIsMono then
          SetColor (MonStatKeyVG, MonStatBG)
        else
          SetColor (ColStatKeyVG, ColStatBG)
      else
        if CRTIsMono then
          SetColor (MonStatItemVG, MonStatBG)
      else
        SetColor (ColStatItemVG, ColStatBG)
    end
    else
      Write (Menu[Cnt]);
  end
end;

{Bildschirm aufbauen}
procedure SetupScreen;
var
  Cnt  :integer;
begin
  if CRTIsMono then begin 
    TextMode (Mono);
    SetColor (MonStatItemVG, MonStatBG)
  end
  else begin 
    TextMode (CO80);
    SetColor (ColStatItemVG, ColStatBG);
  end;
  
  ClrScr;
  GotoXY ((80 - Length (CString)) div 2, 1);
  Write (CString);
  
  DisplayStatusBar (WIN_MAIN_STAT);
  
  Window (1, 2, 80, 24);
  if CRTIsMono then
    SetColor (MonWinItemVG, MonWinBG)
  else
    SetColor (ColWinItemVG, ColWinBG); ClrScr;
  
  GotoXY (1, 1);
  Write ('ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป');
  Write ('บ                                                                              บ');
  GotoXY (5, 2);
  Write (WIN_MAIN_HDR);
  GotoXY (1, 3);
  Write ('วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ');
  
  {UM: the original version does not work for me :-(
  GotoXY (1, 22);
  Write ('ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ');
  GotoXY (1, 22);
  InsLine;
  }
  
  Window (1, 2, 80, 25);
  GotoXY (1, 23);
  Write ('ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ');
  
  for Cnt := 4 to 22 do begin 
    GotoXY (1, Cnt);
    Write ('บ');
    GotoXY (80, Cnt);
    Write ('บ')
  end;
  
  Window (2, 5, 79, 23);
  ClrScr;
end;

{Hilfe ausgeben}
procedure ExecHelp (KeyStr: string);
var
  CRTBuffer     :PCRTBuffer;
  Cnt, Lns, Pos :integer;
  Old, Key, Cur :integer;
  Line          :string;
  Buffer        :array[0..255] of PChar;
  HelpFile      :Text;
begin
  while KeyPressed do
    ReadKey;
  
  {$IFOPT I+}
  {$DEFINE IOCHECK}
  {$I-}
  {$ENDIF}
  Assign (HelpFile, XHELPPath);
  FileMode := 0;
  Reset (HelpFile);
  {$IFDEF IOCHECK}
  {$UNDEF IOCHECK}
  {$I+}
  {$ENDIF}
  if IOResult = 0 then begin 
    repeat
      readln (HelpFile, Line);
    until EOF (HelpFile) or (Line = KeyStr) ;
    
    if Line = KeyStr then begin 
      Lns := 0;
      repeat
        readln (HelpFile, Line);
        if Line <> CHelpEndKey then begin 
          if MaxAvail < Length (Line) + 1 then begin 
            MemoryError;
            Exit;
          end;
          GetMem (Buffer[Lns], Length (Line) + 1);
          
          StrPCopy (Buffer[Lns], Line);
          Inc (Lns)
        end;
      until EOF (HelpFile) or (Line = CHelpEndKey) ;
      
      if Lns > 0 then begin 
        Window (40, 3, 78, 23);
        if not SaveWindow (CRTBuffer) then begin 
          MemoryError;
          Exit;
        end;
        
        if CRTIsMono then
          SetColor (MonStatItemVG, MonStatBG)
        else
          SetColor (ColStatItemVG, ColStatBG);
        ClrScr;
        
        GotoXY (1, 20);
        Write ('ศอออออออออออออออออออออออออออออออออออออผ');
        GotoXY (1, 1);
        Write ('ษอออออออออออออออออออออออออออออออออออออป');
        InsLine;
        GotoXY (3, 1);
        Write (' ', WIN_HELP_HDR, ' ');
        
        GotoXY (1, 2);
        for Cnt := 2 to 20 do
          Write ('บ                                     บ');
        
        Window (42, 4, 76, 23);
        
        Pos := 0;
        Old := Lns;
        Cur := 0;
        repeat
          {Position gendert}
          if Pos <> Old then begin 
            for Cnt := Pos to Pos + 18 do begin 
              GotoXY (1, Cnt - Pos + 1);
              ClrEOL;
              if Cnt < Lns then
                Write (Buffer[Cnt])
            end;
            GotoXY (1, Cur + 1);
          end;
          
          Old := Pos;
          
          Key := Ord (ReadKey);
          if Key = 0 then
            Key := Ord (ReadKey) + KeyExtended;
          
          case Key of
            {Pfeil nach oben}
            KeyArrowUp: begin 
              if SuppBlind then
                if Cur > 0 then begin 
                  Dec (Cur);
                  GotoXY (1, Cur + 1);
                end
                else if Pos > 0 then
                  Dec (Pos)
                else
                  Beep (SINGLE)
              else
                if Pos > 0 then
                  Dec (Pos)
              else
                Beep (SINGLE);
            end;
            
            {Pfeil nach unten}
            KeyArrowDown: begin 
              if SuppBlind then
                if Cur < MaxHelpLines then
                  if Cur < Lns - Pos - 1 then begin 
                    Inc (Cur);
                    GotoXY (1, Cur + 1);
                  end
                  else
                    Beep (SINGLE)
                else if Pos < Lns - (MaxHelpLines + 1) then
                  Inc (Pos)
                else
                  Beep (SINGLE)
              else
                if Lns > MaxHelpLines then
                  if Pos < Lns - (MaxHelpLines + 1) then
                    Inc (Pos)
                  else
                    Beep (SINGLE)
              else
                Beep (SINGLE);
            end;
            
            {Seite nach oben}
            KeyPageUp: begin 
              if SuppBlind then begin 
                Cur := 0;
                GotoXY (1, Cur + 1);
              end;
              
              if Pos = 0 then
                Beep (SINGLE)
              else if Pos >= MaxHelpLines then
                Pos := Pos - MaxHelpLines
              else
                Pos := 0;
            end;
            
            {Seite nach unten}
            KeyPageDown: begin 
              if SuppBlind then begin 
                Cur := 0;
                GotoXY (1, Cur + 1);
              end;
              
              if Lns > MaxHelpLines then
                if Pos < Lns - (MaxHelpLines + 1) then
                  Pos := Pos + MaxHelpLines
                else
                  Beep (SINGLE)
              else
                Beep (SINGLE);
            end;
            
            KeyHome: begin 
              if SuppBlind then begin 
                Cur := 0;
                GotoXY (1, Cur + 1);
              end;
              
              if Pos = 0 then
                Beep (SINGLE)
              else
                Pos := 0;
            end;
            
            KeyEnd: begin 
              if SuppBlind then begin 
                Cur := 0;
                GotoXY (1, Cur + 1);
              end;
              
              if Lns > MaxHelpLines then
                Pos := Lns - (MaxHelpLines + 1)
              else
                Beep (SINGLE)
            end;
          end;
          
          if Lns > MaxHelpLines then
            if Pos > Lns - (MaxHelpLines + 1) then
              Pos := Lns - (MaxHelpLines + 1);
          
          if Pos < 0 then
            Pos := 0;
        until Key = KeyEscape ;
        
        for Cnt := 0 to Lns - 1 do
          FreeMem (Buffer[Cnt], strlen (Buffer[Cnt]) + 1);
        
        RestoreWindow (CRTBuffer, FALSE);
        Window (2, 5, 79, 23);
        
      end
      else MessageBox (BOX_WARN_errFile_HDR, 
          BOX_WARN + BOX_WARN_errFile_rdXFH, 
          ButtonOK or ButtonDefOK)
      
    end
    else MessageBox (BOX_INFO_noHelp_HDR, 
        BOX_INFO + BOX_INFO_noHelp, 
        ButtonOK or ButtonDefOK);
    Close (HelpFile)
    
  end
  else MessageBox (BOX_WARN_errFile_HDR, 
      BOX_WARN + BOX_WARN_errFile_opnXFH, 
      ButtonOK or ButtonDefOK)
end;

{Menuroutine}
function DoMenu (X, Y, Men, Sel: integer; 
                Header: string; 
                MenuEntr, MenuHelp: array of TMenuStr; 
                var CRTBuffer: PCRTBuffer) :integer;
var
  Cnt, Act, Old, Key :integer;
  Width              :integer;
begin
  while KeyPressed do
    ReadKey;
  
  {Men vorbereiten}
  Width := 0;
  for Cnt := 0 to Men - 1 do begin 
    if MenuEntr[Cnt] <> '' then begin 
      {Submens mit Pfeil}
      if MenuEntr[Cnt, Length (MenuEntr[Cnt])] = ChrArrow then
        Width := Max (Width, Length (MenuEntr[Cnt]) + 3)
        {Markierte Mens mit Hkchen oder Bullet}
      else if (MenuEntr[Cnt, 1] = ChrCheck) or (MenuEntr[Cnt, 1] = ChrBullet) then
        Width := Max (Width, Length (MenuEntr[Cnt]) + 3)
        {Normale Mens zwei Einrcken}
      else
        Width := Max (Width, Length (MenuEntr[Cnt]) + 4);
    end;
  end;
  for Cnt := 0 to Men - 1 do begin 
    if MenuEntr[Cnt] <> '' then begin 
      {Submens mit Pfeil darstellen}
      if MenuEntr[Cnt, Length (MenuEntr[Cnt])] = ChrArrow then begin 
        MenuEntr[Cnt] := '  ' +
          Copy (MenuEntr[Cnt], 1, Length (MenuEntr[Cnt]) - 1) +
          StrFill (' ', Width - Length (MenuEntr[Cnt]) - 2) +
          Copy (MenuEntr[Cnt], Length (MenuEntr[Cnt]), 1);
      end
      {Markierte Mens mit Hkchen oder Bullet darstellen}
      else if (MenuEntr[Cnt, 1] = ChrCheck) or (MenuEntr[Cnt, 1] = ChrBullet) then begin 
        MenuEntr[Cnt] :=
          Copy (MenuEntr[Cnt], 1, 1) + ' ' +
          Copy (MenuEntr[Cnt], 2, Length (MenuEntr[Cnt]) - 1);
      end
      {Normale Mens zwei Einrcken}
      else
        MenuEntr[Cnt] := '  ' + MenuEntr[Cnt];
    end;
  end;
  
  {Bildschirmaufbau}
  Window (X, Y, X + Width + 3, Y + Men + 1);
  if CRTBuffer = nil then begin 
    if not SaveWindow (CRTBuffer) then begin 
      DoMenu := 0;
      MemoryError;
      Exit;
    end;
    
    if CRTIsMono then
      SetColor (MonWinItemVG, MonWinBG)
    else
      SetColor (ColWinItemVG, ColWinBG);
    ClrScr;
    GotoXY (1, Men + 1);
    Write ('ศ', StrFill ('อ', Width + 2), 'ผ');
    GotoXY (1, 1);
    Write ('ษ', StrFill ('อ', Width + 2), 'ป');
    InsLine;
    GotoXY (3, 1);
    Write (' ', Header, ' ');
    GotoXY (1, 2);
    for Cnt := 2 to Men + 1 do
      Write ('บ', StrFill (' ', Width + 2), 'บ');
    
    Window (X + 1, Y + 1, X + Width + 2, Y + Men);
    for Cnt := 0 to Men - 1 do begin 
      if MenuEntr[Cnt] <> '' then begin 
        GotoXY (1, Cnt + 1);
        Write (' ', MenuEntr[Cnt])
      end
      else begin 
        Window (X, Y, X + Width + 3, Y + Men + 1);
        GotoXY (1, Cnt + 2);
        Write ('ว', StrFill ('ฤ', Width + 2), 'ถ');
        Window (X + 1, Y + 1, X + Width + 2, Y + Men);
      end
    end;
  end
  else
    Window (X + 1, Y + 1, X + Width + 2, Y + Men);
  
  Act := Sel;
  Old := Act;
  
  if CRTIsMono then
    SetColor (MonSelItemVG, MonSelBG)
  else
    SetColor (ColSelItemVG, ColSelBG);
  
  GotoXY (1, Act + 1);
  ClrEOL;
  Write (' ', MenuEntr[Act]);
  GotoXY (1, Act + 1);
  
  repeat
    Key := Ord (ReadKey);
    if Key = 0 then
      Key := Ord (ReadKey) + KeyExtended;
    
    case Key of
      {Pfeil nach oben}
      KeyArrowUp: begin 
        if Act > 0 then
          Dec (Act);
      end;
      
      {Pfeil nach unten}
      KeyArrowDown: begin 
        if Act < Men - 1 then
          Inc (Act);
      end;
      
      {F1 - Hilfe}
      KeyF1: begin 
        ExecHelp (MenuHelp[Act]);
        Window (X + 1, Y + 1, X + Width + 2, Y + Men);
        GotoXY (1, Act + 1);
      end;
    end;
    
    {Postition hat sich gendert?}
    if Act <> Old then begin 
      if MenuEntr[Act] = '' then
        if Old < Act then
          Inc (Act)
        else
          Dec (Act);
      
      if CRTIsMono then
        SetColor (MonWinItemVG, MonWinBG)
      else
        SetColor (ColWinItemVG, ColWinBG);
      
      GotoXY (1, Old + 1);
      ClrEOL;
      Write (' ', MenuEntr[Old]);
      
      if CRTIsMono then
        SetColor (MonSelItemVG, MonSelBG)
      else
        SetColor (ColSelItemVG, ColSelBG);
      
      GotoXY (1, Act + 1);
      ClrEOL;
      Write (' ', MenuEntr[Act]);
      GotoXY (1, Act + 1);
      
      Old := Act
    end;
  until (Key = KeyEnter) or (Key = KeyEscape) ;
  
  if Key = KeyEnter then
    DoMenu := Act + 1
  else
    DoMenu := 0;
end;

{Checksumme von XFDISK prfen}
function GetCheck (Path: string) :boolean;
var
  BinFile  :file;
  CheckSum :longint;
  SavedSum :longint;
  S1, S2   :longint;
  Cnt, Tmp :longint;
  Fr       :word;
  UseMem   :longint;
  ThisByte :byte;
  Buffer   :PBuffer;
begin
  GetCheck := FALSE;
  CheckSum := 0;
  
  Assign (BinFile, Path);
  FileMode := 0;
  Reset (BinFile, 1);
  if IOResult = 0 then begin 
    S1 := BinarySize + XMBRSize + XMENUSize;
    
    UseMem := Min (MaxAvail, CheckUseMaxMem);
    
    if S1 > 0 then begin 
      if S1 <= UseMem then begin 
        if MaxAvail < S1 then begin 
          MemoryError;
          Exit;
        end;
        GetMem (Buffer, S1);
        
        BlockRead (BinFile, Buffer^, S1, Fr);
        for Cnt := 0 to Fr - 1 do begin 
          {$IFOPT R+}
          {$DEFINE RANGECHECK}
          {$R-}
          {$ENDIF}
          {$IFOPT Q+}
          {$DEFINE OVERFLOWCHECK}
          {$Q-}
          {$ENDIF}
          CheckSum := CheckSum + longint (Buffer^[Cnt]) * longint (Cnt + 1);
          if Odd (CheckSum) then
            CheckSum := CheckSum xor XFDCheckOddMask
          else
            CheckSum := CheckSum xor XFDCheckEvenMask;
          {$IFDEF RANGECHECK}
          {$UNDEF RANGECHECK}
          {$R+}
          {$ENDIF}
          {$IFDEF OVERFLOWCHECK}
          {$UNDEF OVERFLOWCHECK}
          {$Q+}
          {$ENDIF}
        end;
        FreeMem (Buffer, S1)
      end
      else begin 
        S2 := UseMem; Tmp := 0;
        if MaxAvail < S2 then begin 
          MemoryError;
          Exit;
        end;
        GetMem (Buffer, S2);
        
        repeat
          BlockRead (BinFile, Buffer^, S2, Fr); Cnt := 0;
          while (Tmp < S1) and (Cnt < Fr) do begin 
            {$IFOPT R+}
            {$DEFINE RANGECHECK}
            {$R-}
            {$ENDIF}
            {$IFOPT Q+}
            {$DEFINE OVERFLOWCHECK}
            {$Q-}
            {$ENDIF}
            CheckSum := CheckSum + longint (Buffer^[Cnt]) * longint (Tmp + 1);
            if Odd (CheckSum) then
              CheckSum := CheckSum xor XFDCheckOddMask
            else
              CheckSum := CheckSum xor XFDCheckEvenMask;
            {$IFDEF RANGECHECK}
            {$UNDEF RANGECHECK}
            {$R+}
            {$ENDIF}
            {$IFDEF OVERFLOWCHECK}
            {$UNDEF OVERFLOWCHECK}
            {$Q+}
            {$ENDIF}
            Inc (Tmp);
            Inc (Cnt);
          end;
        until (EOF (BinFile)) or (Tmp = S1) ;
        FreeMem (Buffer, S2)
      end
    end;
    Seek (BinFile, S1);
    BlockRead (BinFile, SavedSum, 4, Fr);
    if Fr = 4 then
      if CheckSum = SavedSum then
        GetCheck := TRUE
  end;
  Close (BinFile)
end;

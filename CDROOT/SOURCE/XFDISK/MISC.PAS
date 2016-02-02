{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    MISC.PAS
    Helper Functions and Procedures

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

unit Misc;

interface

type
  {Buffer fr SaveScreen}
  PCRTBuffer = ^TCRTBuffer;
  TCRTBuffer = record
    CURPos :word;    {Cursor Position}
    CURSiz :word;    {Cursor Gr”áe}
    CRTMod :word;    {Bildschirm Modus}
    CRTWdt :word;    {Bildschirm Breite}
    CRTMem :pointer; {Bildschirm Adresse}
    WINMin :word;    {Fenster Links Oben}
    WINMax :word;    {Fenster Rechts Unten}
    WINWdt :byte;    {Fenster Breite}
    WINHgt :byte;    {Fenster H”he}
    WINMem :pointer; {Fenster Adresse}
  end;

var
  CRTSegment :word;
  CRTIsMono  :boolean;

function Int2Str (value, width: integer) :string;
function Word2Hex (value: word) :string;
function Byte2Hex (value: byte) :string;
function UpStr (InStr: string) :string;
function Max (a, b: longint) :longint;
function Min (a, b: longint) :longint;
function StrFill (s: string; len: integer) :string;
function SaveWindow (var Buffer: PCRTBuffer) :boolean;
function RestoreWindow (var Buffer: PCRTBuffer; SetMode: boolean) :boolean;
function GetWinVer :word;
function GetDosVer :word;
procedure SetColor (VG, HG: byte);
procedure HideCursor;
procedure ShowCursor (Cursor: word);
procedure ReBoot;
procedure Wait (ms: word);
function  GetTicks :longint;

implementation

uses
  CRT, WINDOS, DOS, STRINGS;

type
  PCRTMem = ^TCRTMem;
  TCRTMem = array[0..16383] of word;

function Int2Str (value, width: integer) :string;
var
  OStr :string[10];
  Cnt  :integer;
begin
  str (value: width, OStr);
  Cnt := 1;
  if width > 0 then
    while OStr[Cnt] = ' ' do begin 
      OStr[Cnt] := '0'; inc (Cnt)
    end;
  Int2Str := OStr;
end;

function Word2Hex (value: word) :string;
var
  HStr :string[10];
  HVal :word;
const
  HChr :array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  HStr := '0000';
  HVal := value;
  HStr[1] := HChr[HVal div 4096]; HVal := HVal mod 4096;
  HStr[2] := HChr[HVal div 256]; HVal := HVal mod 256;
  HStr[3] := HChr[HVal div 16]; HVal := HVal mod 16;
  HStr[4] := HChr[HVal];
  Word2Hex := HStr
end;

function Byte2Hex (value: byte) :string;
var
  HStr :string[10];
  HVal :byte;
const
  HChr :array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  HStr := '00';
  HVal := value;
  HStr[1] := HChr[HVal div 16]; HVal := HVal mod 16;
  HStr[2] := HChr[HVal];
  Byte2Hex := HStr
end;

function UpStr (InStr: string) :string;
var
  OutStr :string;
  Cnt    :integer;
  begin
    OutStr := InStr;
    for Cnt := 1 to length (InStr) do OutStr[Cnt] := UpCase (OutStr[Cnt]);
    UpStr := OutStr
  end;

function Max (a, b: longint) :longint;
begin
  if a > b then Max := a
  else Max := b
end;

function Min (a, b: longint) :longint;
begin
  if a < b then Min := a
  else Min := b
end;

function StrFill (s: string; len: integer) :string;
var
  cnt :integer;
  out :string;
begin
  out := '';
  for cnt := 1 to len do out := out + s;
  StrFill := out;
end;

function SaveWindow (var Buffer: PCRTBuffer) :boolean;
var
  Cx, Cy :word;
  Cnt    :word;
  CRTWdt :word;
  BufSiz :longint;
  CRTMem :PCRTMem;
  WINMem :PCRTMem;
 begin
   SaveWindow := FALSE;
   
   Buffer := nil;
   if MaxAvail < sizeof (TCRTBuffer) then exit;
   new (Buffer);
   
   Buffer^.CURPos := WhereX + WhereY shl 8;
   Buffer^.CURSiz := memw[$0040:$0060];
   {BIOS Variablen Cursor Gr”áe}
   
   Buffer^.CRTMod := LastMode;
   Buffer^.CRTMem := ptr (CRTSegment, 0);
   Buffer^.CRTWdt := memw[$0040:$004A];
   {BIOS Variablen Bildschirm Breite}
   
   Buffer^.WINMin := WindMin;
   Buffer^.WINMax := WindMax;
   Buffer^.WINWdt := lo (WindMax) - lo (WindMin) + 1;
   Buffer^.WINHgt := hi (WindMax) - hi (WindMin) + 1;
   
   BufSiz := Buffer^.WINWdt * Buffer^.WINHgt * 2;
   
   if MaxAvail < BufSiz then begin 
     dispose (Buffer);
     Buffer := nil;
     exit;
   end;
   GetMem (Buffer^.WINMem, BufSiz);
   
   CRTMem := PCRTMem (Buffer^.CRTMem);
   
   WINMem := PCRTMem (Buffer^.WINMem);
   CRTWdt := Buffer^.CRTWdt;
   Cnt := 0;
   for Cy := hi (Buffer^.WINMin) to hi (Buffer^.WINMax) do begin 
     for Cx := lo (Buffer^.WINMin) to lo (Buffer^.WINMax) do begin 
       WINMem^[Cnt] := CRTMem^[Cy * CRTWdt + Cx];
       inc (Cnt);
     end;
   end;
   
   SaveWindow := TRUE;
 end;

function RestoreWindow (var Buffer: PCRTBuffer; SetMode: boolean) :boolean;
var
  Regs   :Registers;
  Cx, Cy :word;
  Cnt    :word;
  CRTWdt :word;
  BufSiz :longint;
  CRTMem :PCRTMem;
  WINMem :PCRTMem;
begin
  RestoreWindow := FALSE;
  if Buffer <> nil then begin 
    if (LastMode <> Buffer^.CRTMod) or (SetMode) then
      TextMode (Buffer^.CRTMod);
    
    if Buffer^.WINMem <> nil then begin 
      CRTMem := PCRTMem (Buffer^.CRTMem);
      WINMem := PCRTMem (Buffer^.WINMem);
      
      CRTWdt := Buffer^.CRTWdt;
      Cnt := 0;
      for Cy := hi (Buffer^.WINMin) to hi (Buffer^.WINMax) do begin 
        for Cx := lo (Buffer^.WINMin) to lo (Buffer^.WINMax) do begin 
          CRTMem^[Cy * CRTWdt + Cx] := WINMem^[Cnt];
          inc (Cnt);
        end;
      end;
      
      BufSiz := Buffer^.WINWdt * Buffer^.WINHgt * 2;
      FreeMem (Buffer^.WINMem, BufSiz);
      Buffer^.WINMem := nil;
      
      RestoreWindow := TRUE;
    end;
    
    Regs.AH := $01;
    Regs.CX := Buffer^.CurSiz;
    intr ($10, Regs);
    
    WindMin := Buffer^.WINMin;
    WindMax := Buffer^.WINMax;
    
    GotoXY (lo (Buffer^.CurPos), hi (Buffer^.CurPos));
    
    dispose (Buffer);
    Buffer := nil;
  end;
end;

function GetDosVer :word;
var
  Regs :Registers;
begin
  Regs.AX := $3000;
  intr ($21, Regs);
  GetDosVer := Regs.AH + Regs.AL * 256;
end;

function GetWinVer :word;
var
  Regs :Registers;
begin
  Regs.AX := $160A;
  intr ($2F, Regs);
  if Regs.AX = $0000 then
    GetWinVer := Regs.BX
  else
    GetWinVer := 0;
end;

procedure SetColor (VG, HG: byte);
begin
  TextColor (VG); TextBackground (HG)
end;

procedure HideCursor;
var
  Regs :Registers;
    begin
      Regs.AH := $01;
      Regs.CX := $2000;
      intr ($10, Regs)
    end;

procedure ShowCursor (Cursor: word);
var
  Regs :Registers;
begin
  Regs.AH := $01;
  Regs.CX := Cursor;
  intr ($10, Regs)
end;

procedure ReBoot;
var
  ExecStr :string;
  WinDir  :PChar;
begin
  if GetWinVer <> 0 then begin 
    WinDir := GetEnvVar ('windir');
    if WinDir <> nil then begin 
      SwapVectors;
      Exec (StrPas (WinDir) + '\rundll.exe', 'User,ExitWindows');
      SwapVectors;
      halt (0);
    end;
  end
  else asm
      mov dx, $0040
      mov ds, dx
      mov di, $0072
      mov dx, $1234
      mov word ptr [di], dx
      mov ax, $FFFF
      push ax
      xor ax, ax
      push ax
      retf
end;
end;

procedure Wait (ms: word); Assembler;
asm
  mov ax, ms
  mov cx, $03E8
  mul cx
  xchg cx, dx
  mov dx, ax
  mov ah, $86
  int $15
end;

function GetTicks :longint; Assembler;
asm
  mov ax, $0040
  mov bx, $006C
  mov es, ax
  mov ax, word ptr es:[bx]
  mov dx, word ptr es:[bx + 2]
end;

var
  CRTPortAdr :word absolute $0040:$0063;

begin
  {Graphikkartentyp bestimmen}
  if CRTPortAdr = $3B4 then begin 
    CRTIsMono := true;
    CRTSegment := $B000
  end
  else begin 
    CRTIsMono := false;
    CRTSegment := $B800
  end;
end.

{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    SETCHECK.PAS
    Calculate File Checksum

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

program setcheck;

{$I-}
{$Q-}
{$R-}

uses
  crt, misc;

type
  {Sektorpuffer}
  PBuffer = ^TBuffer;
  TBuffer = array[0..511] of byte;

var
  BinFile  :file;
  CheckSum :longint;
  S1, S2   :longint;
  Cnt, Tmp :longint;
  UseMem   :longint;
  ThisByte :byte;
  Fr, Mark :word;
  Ch       :char;
  Buffer   :PBuffer;

begin
  writeln ('Calculate File Checksum...');
  if ParamCount <> 1 then begin 
    writeln; writeln ('Usage: SETCHECK <FILE>');
    halt (1)
  end;
  assign (BinFile, ParamStr (1)); reset (BinFile, 1);
  if IOResult = 0 then begin 
    Seek (BinFile, FileSize (BinFile) - 2);
    if IOResult = 0 then begin 
      BlockRead (BinFile, Mark, 2, Fr);
      if Fr = 2 then begin 
        if Mark = $A5A5 then begin 
          writeln ('File seems to be already marked.');
          write ('Check Anyway? (Y/N)');
          repeat
            ch := ReadKey;
          until upcase (ch) in ['Y', 'N'] ;
          if upcase (ch) = 'N' then halt (1);
        end
      end
      else begin 
        writeln ('Failed to read File.'); halt (1)
      end;
      Seek (BinFile, 0);
      if IOResult = 0 then begin 
        CheckSum := 0;
        
        S1 := FileSize (BinFile);
        
        UseMem := Min (MaxAvail, 16384);
        
        if S1 > 0 then begin 
          if S1 <= UseMem then begin 
            GetMem (Buffer, S1);
            BlockRead (BinFile, Buffer^, S1, Fr);
            for Cnt := 0 to Fr - 1 do begin 
              CheckSum := CheckSum + longint (Buffer^[Cnt]) * longint (Cnt + 1);
              if odd (CheckSum) then CheckSum := CheckSum xor $0A6F0F6E
              else CheckSum := CheckSum xor $14AD0C1D
            end;
            FreeMem (Buffer, S1)
          end
          else begin 
            S2 := UseMem; Tmp := 0;
            GetMem (Buffer, S2);
            repeat
              BlockRead (BinFile, Buffer^, S2, Fr); Cnt := 0;
              while (Tmp < S1) and (Cnt < Fr) do begin 
                CheckSum := CheckSum + longint (Buffer^[Cnt]) * longint (Tmp + 1);
                if odd (CheckSum) then CheckSum := CheckSum xor $0A6F0F6E
                else CheckSum := CheckSum xor $14AD0C1D;
                inc (Tmp); inc (Cnt);
              end;
            until EOF (BinFile) or (Tmp = S1) ;
            FreeMem (Buffer, S2)
          end
        end;
      end
      else begin 
        writeln ('Failed to seek Start of File.'); halt (1)
      end
    end
    else begin 
      writeln ('Failed to seek near End of File.'); halt (1)
    end
  end
  else begin 
    writeln ('Failed to open File.'); halt (1)
  end;
  Mark := $a5a5; Seek (BinFile, FileSize (BinFile));
  if IOResult = 0 then begin 
    BlockWrite (BinFile, CheckSum, 4, Fr);
    if Fr <> 4 then begin 
      writeln ('Failed to write File.'); halt (1)
    end;
    BlockWrite (BinFile, Mark, 2, Fr);
    if Fr <> 2 then begin 
      writeln ('Failed to write File.'); halt (1)
    end;
    writeln (FileSize (BinFile): 10, ' bytes checked.');
  end
  else begin 
    writeln ('Failed to seek End of File.'); halt (1)
  end;
  close (BinFile);
end.

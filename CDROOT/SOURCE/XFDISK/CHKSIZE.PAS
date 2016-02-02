{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    CHKSIZE.PAS
    Check Size of XFDisk Compiled Binary

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

program ChkSize;

{$M 2048,0,0}

var f: file of Byte;
    i: text;
    p: integer;
    line : string;
    size, size2 : longint;
begin
  writeln ('Check Size of Compiled Binary...');
  
  if ParamCount <> 2 then begin 
    writeln; writeln ('Usage: CHKSIZE <BINARY> <INCLUDE>');
    halt (1);
  end;
  
  assign (f, ParamStr (1));
  reset (f);
  size := filesize (f);
  close (f);
  
  assign (i, ParamStr (2));
  reset (i);
  readln (i, line);
  close (i);
  
  p := pos ('=', line);
  delete (line, 1, p);
  dec (line[0]);
  val (line, size2, p);
  
  if size <> size2 then begin 
    writeln ('Size expected: ', size2);
    writeln ('Size found: ', size);
    
    rewrite (i);
    writeln (i, '{do not change!} const BinarySize : longint =', size, ';');
    close (i);
    
    halt (2);
  end;
end.

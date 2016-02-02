uses diskctrl;

const Drive = $80;
var buf : array[1..17, 1..512] of byte;
    res : integer;
    f   : file;
begin
  writeln('XFDisk Test for Abit HotRod Cards');

  if paramstr(1)='clean' then begin
    writeln('cleaning...');

    fillchar(buf, sizeof(buf), $F6);

    InitReadWriteSectors (Drive, INITWRITE);
    Res := WriteSectors (Drive, 1, 16, @buf);
    DeInitReadWriteSectors (Drive);
  end else if paramstr(1)='dump' then begin
    writeln('writing file "test.dmp"');

    InitReadWriteSectors (Drive, INITREAD);
    Res := ReadSectors (Drive, 0, 17, @buf);
    DeInitReadWriteSectors (Drive);

    assign(f, 'test.dmp');
    rewrite(f,1);
    blockwrite(f,buf, sizeof(buf));
    close(f);
  end else begin
    writeln('usage: "test clean" to erase sectors 2-17 on first hard disk');
    writeln('       "test dump" to dump sectors 1-17 on first hard disk to file');
  end;
end.
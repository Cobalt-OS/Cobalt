uses DiskCtrl;
var i,n: integer;
    DG: PDriveData;
begin
  writeln ('BIOS meldet folgende Daten:');
  n := GetNumberOfDrives;
  writeln ('Anzahl vorhandenen Festplatten: ', n);
  for i := $80 to $80 - 1 + n do begin 
    if GetDriveFormat (i, DG) then begin 
      writeln ('Festplatte ', i - 128 + 1, ':');
      if dg^.int13x then
        writeln ('unterstÅtzt Int13X')
      else
        writeln ('unterstÅtzt kein Int13X');
      writeln ('Zylinder: ', dg^.Cylinders);
      writeln ('Kîpfe: ', dg^.Heads);
      writeln ('Sektoren: ', dg^.Sectors);
      writeln ('Gesamtzahl Sektoren: ', dg^.Totalsectors);
    end
    else
      writeln ('Fehler bei Abfrage von Festplatte ', i);
  end;
end.

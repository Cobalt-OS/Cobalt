--************************************************************************
--
--  SHSUServ.ADA               Version 3.1
--
--
--  A copyright-reserved, free use program.  Use at your own risk.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************
-- For Meridian ADA286
-- compile -fs -g -K
-- bamp with -M 5000 -s 44500 -G for 48 sessions
--

with Types;        use Types;
with NetBios;      use NetBios;
with Drivers;      use Drivers;
with ServerTasks;  use ServerTasks;
with CDRoms;       use CDRoms;
with system;
with memory;
with program_control;
with text_io;      use text_io;
with text_handler; use text_handler;
with arg;
with tty,box,cursor,video,common_display_types;
with unchecked_deallocation, unchecked_conversion;


procedure SHSUServ is

-- Need more than the default 20 handles per process

   NewHandles  : bytes(1..255);


MaxSessions    : constant := 48;
LastCD         : integer := -1;

NCB            : NetBiosCmdBlks;
package NB_SS48 is new NB_SSTypes(MaxSessions);use NB_SS48;
NBSS           : NB_SS48.NB_SSs;

ServerName     : string16 := ("SHSU-CD-SERVER  ");

CDTbl          : CDArrayAccess;

NET            : NetAccess := new Nets;
HUB            : SchedulerAccess := new Schedulers;

SessionTable   : array (1..MaxSessions ) of SessionsAccess;

xch            : common_display_types.byte;
ch             : character;
CMD_Parm_Error : exception;

function to_caps (C: character) return character is
begin
  if (C >= 'a' and C<= 'z') then
    return character'val(character'pos(C) -32);
  else
    return c;
  end if;
end to_caps;

procedure get_parms is
  parm: text(60);
begin
  for i in 2..arg.count loop      -- first arg is program name
    set(parm, arg.data(i));       -- convert string argument to text object.

    if not empty(parm) then
      if value(parm)(1) = '-' or value(parm)(1) = '/' then
        if length(parm) >= 4 then
          case value(parm)(2) is
            when 's'|'S' =>
              if length(parm) > 19 then
                put_line("Server name to long.  Max is 16 characters.");
                raise CMD_Parm_Error;
              else
                ServerName := (others=> ' ');
                ServerName(1..length(parm)-3) := value(parm)(4..length(parm));
                for i in 1..16 loop
                  ServerName(i) := to_caps(ServerName(i));
                end loop;
              end if;
            when others =>
              put_line("Unknown parameter """ & value(parm) & """");
          end case;
        else
          put_line("Invalid parameter """ & value(parm) & """");
        end if;
      else
        put_line("Parameter """ & value(parm) & """ doesn't start with - or /.");
      end if;
    end if;

  end loop;

end get_parms;

procedure SetUpCDs(pCDTbl: out CDArrayAccess) is

INI_FName: constant string := ".\SHSUSERV.INI";
INI      : Boolean := False;
INI_F    : text_io.File_Type;

Done     : Boolean := False;

type DrvrEntries is
  record
    EType  : CDRoms.EntryType;
    Name   : string(1..24);
  end record;
DrvrEntry: DrvrEntries;
DrvrString : string(1..64);
DefDriver : array(1..6) of string(1..16) :=
        (("CD  CD001       "),("CD  CD002       "),
         ("CD  MSCD001     "),("CD  MSCD002     "),
         ("IMG SHSUDRV0.IMG"),("IMG SHSUDRV1.IMG"));

DriverUnits    : integer;
CDTbl          : CDArrayAccess;
function CDA_to_DW is new unchecked_conversion(CDArrayAccess, DW);
DrvrIndex  : integer;

type ListEntry;
type LinkPtr is access ListEntry;
type ListEntry is
  record
    Link : LinkPtr;
    CDEntry: CDEntries;
  end record;
DriverList     : LinkPtr := null;
NextDriver     : LinkPtr;

LastCD         : integer := -1;

procedure GetDriveInfo(DriveEntry: in out CDEntries;
                       Units     : out integer) is
  DriverHandle   : integer;
  DriverSubUnits  : byte;
begin
  OpenDevice ( DeviceName => DriveEntry.Driver.Name,
               Handle     => DriverHandle);
  GetDeviceEntryAddresses (Handle           => DriverHandle,
                           DeviceStrategy   => DriveEntry.Driver.Strategy,
                           DeviceInterrupt  => DriveEntry.Driver.Interrupt,
                           SubUnits         => DriverSubUnits );
  CloseDevice (Handle => DriverHandle);
  Units := integer(DriverSubUnits);
  exception
    when DEV_Error => Units := 0;
end GetDriveInfo;

function OpenHDCDIMG(FileName: string) return IMG_Access is
  Img_F : IMG_Access := new DIO.File_Type;
begin
  DIO.Open(Img_F.all,DIO.In_File,FileName);
  return Img_F;
exception
  when others =>zapImg_F(Img_F);
                return null;
end OpenHDCDIMG;

procedure LinkUpDrivers(Head:in out LinkPtr;NewEntry:LinkPtr;Units:integer) is
  temp : LinkPtr;
begin
  if Units = 1 then
    NewEntry.CDEntry.Unit := byte(Units-1);
    NewEntry.Link := Head;
    Head  := NewEntry;
  else
    LinkUpDrivers(Head,NewEntry,Units-1);
    temp := new ListEntry;
    temp.all := NewEntry.all;
    temp.CDEntry.Unit := byte(Units-1);
    temp.Link := Head;
    Head  := temp;
  end if;
end LinkUpDrivers;

begin

begin
  text_io.Open(INI_F,text_io.In_File,INI_FName);
  INI := True;
exception
  when others => null;
end;

if  INI then
  tty.put(10,10,"Using INI file: "&INI_FName);
else
  tty.put(10,10,"Using default driver name list.");
end if;

DONE := not INI and then DefDriver'last < 1;

DrvrIndex := 1;
cursor.move(11,10);

while not Done loop
  begin
    declare
      k,n: natural;
    begin
      DrvrString:=(others=>' ');
      if INI then
        k := 0;
        while k = 0 loop
          text_io.get_line(INI_F,DrvrString,k);
        end loop;
      else
        k := DefDriver(DrvrIndex)'last;
        DrvrString(1..k):= DefDriver(DrvrIndex);
        DrvrIndex := DrvrIndex + 1;
        if DrvrIndex > DefDriver'last then
           DONE := True;
        end if;
      end if;
      ET_IO.get(from=> DrvrString, item=> DrvrEntry.EType, last=> n);
      for i in n+1..k loop
        if DrvrString(i) /= ' ' then
          n := i;
          exit;
        end if;
      end loop;
      DrvrEntry.Name := (others=>' ');
      DrvrEntry.Name(1..k-n+1) := DrvrString(n..k);
      declare
        r,c : integer;
      begin
        cursor.get_position(r,c);
        tty.put(r,10,"Looking for: "&DrvrString(n..k));
      end;
    end;
    NextDriver := new ListEntry;
    if DrvrEntry.EType = CD then
      NextDriver.CDEntry :=(EType => CD, Unit=>byte(0),Label=>(others=>' '),
              Status  => long_to_DW(0), VolSize => long_to_DW(0),
              Driver =>(Name => DrvrEntry.Name, Strategy => 0, Interrupt => 0));
      GetDriveInfo(DriveEntry => NextDriver.CDEntry, Units => DriverUnits);
    else
      NextDriver.CDEntry :=(EType => IMG, Unit=>byte(0),Label=>(others=>' '),
              Status  => long_to_DW(0), VolSize => long_to_DW(0),
              File =>(Name => DrvrEntry.Name,
                          Img_F =>OpenHDCDIMG(DrvrEntry.Name)));
      if NextDriver.CDEntry.File.Img_F = null then
        DriverUnits := 0;
      else
        DriverUnits := 1;
      end if;
    end if;

    if DriverUnits /= 0 then
      LinkUpDrivers(DriverList,NextDriver,DriverUnits);
      LastCd := LastCd + DriverUnits;
      tty.put_line(" Found");
    else
      tty.put_line(" Not Found");
    end if;

  exception
    when data_error => null;       -- ET_IO error, skip this line
    when others =>     DONE:=True;
  end;
end loop; --while

if INI then
  text_io.Close(INI_F);
end if;

if LastCD < 0 then
  raise DEV_Error;
end if;

-- Allocate the driver table

CDTbl := new CDArray(0..LastCD);

-- Fill in the table entries
declare
ptr:LinkPtr:= DriverList;
begin
for i in reverse CDTbl'range loop
  CDTbl(i) := ptr.CDEntry;
  ptr := ptr.Link;
end loop;

--for i in CDTbl'range loop
--  if CDTbl(i).EType = CD then
--    put_line(CDTbl(i).Driver.Name);
--  else
--    put_line(CDTbl(i).File.Name);
--  end if;
--end loop;
end;

pCDTbl := CDTbl;

end SetUpCDs;

procedure ExtendFileHandleTable is

-- This increases the max file handles for a process.  Set max for DOS with
--   Files= in CONFIG.SYS
--
-- Must declare NewHandles in static storage area
--    NewHandles  : bytes(1..255);
--

PSP_Handles_Offset : memory.segment_offset := memory.segment_offset(16#18#);
PSP_MaxNumberHandles_Offset : memory.segment_offset := memory.segment_offset(16#32#);
PSP_HandleTablePtr_Offset : memory.segment_offset := memory.segment_offset(16#34#);
PSP_seg : memory.memory_segment:= program_control.segment_prefix;

OldHandles  : bytes(1..20);

for OldHandles use at memory.make(segment => PSP_seg,
                                  offset  => PSP_Handles_Offset);
MaxNumberHandles : word;
for MaxNumberHandles use at memory.make(segment => PSP_seg,
                                   offset  => PSP_MaxNumberHandles_Offset);
HandleTablePtr: system.address;
for HandleTablePtr use at memory.make(segment => PSP_seg,
                                       offset  => PSP_HandleTablePtr_Offset);

begin
  NewHandles        := (others=>16#FF#);    -- unused handle code
  NewHandles(1..20) := OldHandles;          -- copy existing handles
  MaxNumberHandles  := NewHandles'last;     -- set new max handles
  HandleTablePtr    := NewHandles(1)'address;  -- point to new handle table

end ExtendFileHandleTable;

procedure ResetFileHandleTable is
PSP_Handles_Offset : memory.segment_offset := memory.segment_offset(16#18#);
PSP_MaxNumberHandles_Offset : memory.segment_offset := memory.segment_offset(16#32#);
PSP_HandleTablePtr_Offset : memory.segment_offset := memory.segment_offset(16#34#);
PSP_seg : memory.memory_segment:= program_control.segment_prefix;

MaxNumberHandles : word;
for MaxNumberHandles use at memory.make(segment => PSP_seg,
                                   offset  => PSP_MaxNumberHandles_Offset);
HandleTablePtr: system.address;
for HandleTablePtr use at memory.make(segment => PSP_seg,
                                       offset  => PSP_HandleTablePtr_Offset);
begin
  MaxNumberHandles  := 20;
  HandleTablePtr    := memory.make(segment => PSP_seg,
                                  offset  => PSP_Handles_Offset);
end ResetFileHandleTable;

begin  -- Main **************************************************************

tty.clear_screen;
box.draw(0,0,9,79,box.double_sided);
tty.put(2,27,"SHSU CDROM SERVER 3.1");
tty.put(3,20,"A copyright-reserved, free use program.");
tty.put(4,28,"(c)John H. McCoy, 1994, 1995, 1996");
tty.put(5,22,"Sam Houston St. Univ., TX 77341-2206");
tty.put(7,17,"Latest version is available from FTP.SHSU.EDU");
tty.put(8,13,"Experimental Academic Software.  Use at your own risk.");

get_parms;

ExtendFileHandleTable;

SetUpCDs(CDTbl);

LastCD := CDTbl.all'last;

CDs.Setup(CDTbl);
delay(1.0);
declare
r,c: integer;
begin
cursor.get_position(r,c);
tty.put(r,10,"Validating: "&ServerName);
end;

NET.Start(ServerName);

tty.put(24,0,"Server name validated");

Console.Init(MaxSessions => MaxSessions,
             LastCd      => LastCd,
             ServerName  => ServerName);

delay(5.0);            -- so initial screen display will complete

for i in 1..MaxSessions loop
    SessionTable(i) := new Sessions;
    SessionTable(i).Start(Net,ServerName,types.byte(integer'succ(LastCD)),HUB);
end loop;

tty.put(24,0,"Enter S to stop the server. ");

loop
  cursor.move(24,27);
  if tty.char_ready then
    tty.get(xch,ch);
    if ch = 'S' or ch = 's' then
      tty.put(24,0,"Are you sure you want to stop? (Y/N)");
      for i in 1..10 loop
        if tty.char_ready then
          tty.get(xch,ch);
          exit;
        end if;
        delay(1.0);
      end loop;
      exit when (ch = 'y' or ch = 'Y');
    elsif ch = 'D' or ch = 'd' then
      tty.put(24,41," Drive Update Toggle in Progress.     ");
      Console.ToggleDriveUpdate;
      delay(1.0);
    end if;
    video.scroll_up(0,24,0,24,40);
    tty.put(24,0,"Enter S to stop the server.");
  end if;
  delay(1.0);

end loop;

video.scroll_up(0,24,0,24,79);

NET.Hold;

tty.put(24,0,"Stopping Net.  May take 30 seconds for timeout.");


NCB.Name    := ServerName;
NCB.Command := NB_SessionStatus;
NCB.BufferPtr := NBSS'address;
NCB.BufferLength := NBSS'size/8;
NetBiosCall(SA_to_ncbAccess(NCB'address));
for i in 1..NBSS.SessionsCount loop
  if (NBSS.SS(integer(i)).State /= NB_SS_ListenPending) then
    nethangup(NBSS.SS(integer(i)).lsn);
  end if;
end loop;

declare
    done: boolean;
begin
    loop
    done := true;
      for i in 1..MaxSessions loop
        if SessionTable(i)'terminated then
          null;
        else
          done:=false;
        end if;
      delay (0.0);
     end loop;
    exit when done;
    end loop;
end;

-- tty.put(24,0,"Console Shutdown.");

video.clear_screen;


Console.Shutdown;      --don't shutdown console til all sessions done



-- tty.put(24,0,"Hub Shutdown.");

HUB.Shutdown;
loop
  exit when HUB'terminated;
end loop;

CDs.Shutdown;
loop
  exit when CDs'terminated;
end loop;

Net.Shutdown;

tty.put(24,0,"Normal End of SHSU CD SERVER.");

ResetFileHandleTable;

exception
  when CMD_Parm_Error         => Put_line("Command line parm error.");
  when DEV_Error              => Put_line("No CD Roms found.");
                                 ResetFileHandleTable;
  when NBX_NetBiosNotLoaded   => Put_line("Net BIOS not loaded.");
                                 ResetFileHandleTable;
  when NBX_NameAlreadyclaimed => Put_line("Name already in use.");
                                 ResetFileHandleTable;
  when NBX_GeneralError       => Put("Error attempting to start ");
                                 Put_line(ServerName);
                                 ResetFileHandleTable;

end SHSUServ;
--************************************************************************
--
--  SERVTASK.ADB               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996 Sam Houston St. Univ., TX 77341-2206
--
--************************************************************************

with drivers; use drivers;
with cdroms; use cdroms;
with video, cursor, box;
with calendar;
with tty;
with text_io; use text_io;
with unchecked_deallocation;
with common_display_types; use common_display_types;

package body ServerTasks is


MaxBlocks   : constant := 15;       -- NETBIOS limits dta to 64K -1 bytes
BlockSize   : constant := 2048;     -- assume cooked mode only

type UserEntries is record
     SessionNumber: types.byte;
     Client       : String16;
     StartTime    : calendar.time;
     LastAccess   : calendar.time;
  end record;

type UsersArray is array (natural range <>) of UserEntries;
type UsersAccess is access UsersArray;

task type LCalls is
  entry Que(Ncb: ncbAccess);
  entry Xit;
  entry Cancel;
  entry ShutDown;
  pragma priority (19);
end LCalls;
for LCalls'storage_size use 512;

N_Listen : LCalls;

NCBInit: NetBiosCmdBlks;

task body Consoles is

  MaxClients      : integer;
  Users           : UsersAccess;
  LastUser        : integer := 0;
  LastCD          : integer;
  I               : integer;
  ServerName      : string16;
  DriverName      : string16;
  DeviceStatus    : DEV_ReturnCodes;
  CDStatus        : DW;
  Stop            : boolean := false;
  DriverUnit      : types.byte;
  CDLabel         : string11;
  StatusCode      : string(1..1);
  UpdateDriveInfo : boolean := True;

  task type Displays is
      entry init;
      entry Shutdown;
  end Displays;
  Display: Displays;

  task body Displays is
    Stop    : boolean:= false;
    procedure paint is
    begin
      tty.put(2,39,TOD(calendar.clock));
      tty.put(1,76,"   ");
      tty.put(1,76,integer'image(LastUser));
      video.scroll_up(0,4,0,23,36);
      if LastUser <= 0 then
        tty.put(4,8,"No Users Connected.");
      else
        for j in reverse 1..LastUser loop
          tty.put(4,0,types.byte'image(Users(j).SessionNumber));
          tty.put(4,5,Users(j).Client);
          tty.put(4,22,TOD(Users(j).StartTime));
          tty.put(4,29,TOD(Users(j).LastAccess));
          if LastUser > 20 then
            delay(0.10);
          end if;
          if j /= 1 then
            video.scroll_down(1,4,0,23,36);
          end if;
        end loop;
      end if;
      if UpdateDriveInfo then
        video.scroll_up(0,4,37,23,79);
        for i in reverse 0..LastCd loop
          delay(0.0);
          GetCDStatus(i,DeviceStatus,CDStatus,DriverName,DriverUnit,CDLabel);
          delay(0.0);
          tty.put(row_range(4), column_range(37),integer'image(i));
          tty.put(row_range(4), column_range(41),DriverName);
          tty.put(row_range(4), column_range(59),
                  integer'image(integer(DriverUnit)));
          if DeviceStatus = DeviceDone then
            StatusCode(1) := ASCII.nul;
            if (CDStatus(2) and 8) /= 0 then
              CDLabel := "---Empty---";
            elsif (CDStatus(1) and 2) /= 0 then
              StatusCode  := "U";
            else
              StatusCode  := "L";
            end if;
            tty.put(row_range(4), column_range(62),CDLabel);
            tty.put(row_range(4), column_range(74),StatusCode);
          elsif ((DeviceStatus and DeviceNotReady)/= word_to_W(0)) then
            tty.put(row_range(4), column_range(62),
                      "Drive not ready. ");
          else
            tty.put(row_range(4), column_range(62),
                      "                  ");
          end if;
          if i /= 0 then
            video.scroll_down(1,4,37,23,79);
          end if;
        end loop;
        tty.put(24,41,"Drive Update ON.  Enter D to Turn OFF.");
      else
        tty.put(24,41,"Drive Update OFF.  Enter D to Turn ON.");
      end if;
    end paint;

  begin
    accept init;                      -- holds til main has loaded everything
    video.clear_screen;
    box.draw(0,0,2,79,box.double_sided);
    tty.put(0,13," Sam Houston State University CD-ROM SERVER 3.0 ");
    tty.put(1,1,"Server:");
    tty.put(1,9,ServerName);
    tty.put(1,27,"Up: ");
    tty.put(1,31,MDY(calendar.clock));
    tty.put(1,40,TOD(calendar.clock));
    tty.put(1,48,"Max Users:");
    tty.put(1,58,integer'image(MaxClients));
    tty.put(1,63,"Active Users:");
    tty.put(1,76,integer'image(LastUser));
    tty.put(2,10," Users ");
    tty.put(2,32," Time:       ");
    tty.put(2,58," CDs ");
    tty.put(3,0," No   User            First  Last");
    tty.put(3,37,"No   Driver/File    Unit    ID     Status");
    paint;
    loop
      select
        delay(20.0);
        paint;
        cursor.move(24,0);
      or
        accept Shutdown do
          Stop := True;
        end Shutdown;
      end select;
      exit when Stop;
    end loop;
  end Displays;

begin

  accept Init(MaxSessions: integer;
              LastCd     : integer;
              ServerName : string16 ) do
    MaxClients          := MaxSessions;
    Consoles.LastCd     := LastCd;
    Consoles.ServerName := ServerName;
  end Init;

  Users := new UsersArray(0..MaxClients);
  Display.init;

  loop
      select
        accept SignIn(SessionNumber: types.byte;
                      Client       : String16) do
          LastUser       := LastUser + 1;
          Users(LastUser).SessionNumber:= SessionNumber;
          Users(LastUser).Client:= Client;
          Users(LastUser).StartTime := calendar.clock;
          Users(LastUser).LastAccess := calendar.clock;
        end SignIn;
      or
        accept SignOut(SessionNumber: types.byte) do
          Users(0).SessionNumber := SessionNumber;
          I := LastUser;
          loop
            exit when Users(I).SessionNumber = SessionNumber;
            I := I - 1;
          end loop;
          LastUser := LastUser-1;
          for j in I..LastUser loop
            Users(j) := Users(j + 1);
          end loop;
        end SignOut;
      or
        accept CkIn(SessionNumber: types.byte) do
          Users(0).SessionNumber := SessionNumber;
          I := LastUser;
          loop
            exit when Users(I).SessionNumber = SessionNumber;
            I := I - 1;
          end loop;
          Users(I).LastAccess := calendar.clock;
        end CkIn;
      or
        accept ToggleDriveUpdate do
          if UpdateDriveInfo then
            UpdateDriveInfo := False;
          else
            UpdateDriveInfo := True;
          end if;
        end ToggleDriveUpdate;
      or
        accept Shutdown do
          Display.Shutdown;
          Stop := True;
        end Shutdown;
      end select;
    exit when Stop;
  end loop;
end Consoles;

task body Sessions is
  rh           : rhs;
  pkt          : pkts;
  dta          : bytesAccess;
  NCB, NCBinit : ncbAccess := new NetBiosCmdBlks;
  NCBclr       : ncbAccess := new NetBiosCmdBlks;
  Net          : NetAccess;
  LocalSession : types.byte;
  DtaSave      : DW;
  CB           : CBAccess := new CBs;
  CbSave       : DW;
  CdSubUnits   : types.byte;
  Hub          : SchedulerAccess;
  AllocError   : boolean;
  procedure Wait is
  begin
    loop
      delay (0.0);                      -- give other tasks a chance
      exit when (NCB.CommandStatus /= NB_CommandPending);
    end loop;
  end Wait;

  procedure SendRH is
  begin
     -- send rh back to client
     pkt := Rhs_to_Pkts(rh);
     pkt(3..pkts'last-1) := pkt(4..pkts'last);
     NCB.all              := NCBinit.all;
     NCB.Command          := NB_Send_NoWait;
     NCB.BufferPtr        := pkt(1)'address;
     NCB.BufferLength     := word(rh.length);
     Net.Call(NCB);
     Wait;
  end SendRH;

begin
  accept Start(Net       : NetAccess;
               LocalName : string16;
               SubUnits  : types.byte;
               Scheduler : SchedulerAccess ) do
    Sessions.Net  := Net;
    NCBclr.name   := LocalName;
    CdSubUnits    := SubUnits;
    Hub           := Scheduler;
  end Start;


que: loop                             -- new sessions start here

  loop
    NCB.all         := NCBclr.all;
    NCB.Command     := NB_Listen_NoWait;
    NCB.CallName(1) := '*';             -- listen for any caller
    Hub.Listen(Net, NCB);               -- get in queue for a call
                                        -- won't return until a session request
                                        -- is received or NET is terminated.
    case NCB.ReturnCode is
      when NB_Ok   =>  exit;
      when NB_CommandCanceled  => exit Que;   -- only occurs if shutdown
      when others => delay(0.0);
    end case;

  end loop;

  LocalSession         := NCB.LocalSession;
  NCBinit.LanAdapter   := NCB.LanAdapter;
  NCBinit.LocalSession := NCB.LocalSession;
  NCBinit.CallName     := NCB.CallName;
  NCBinit.Name         := NCB.Name;
  NCBinit.NameNumber   := NCB.NameNumber;

  Console.SignIn(SessionNumber => LocalSession,
                 Client        => NCBinit.CallName);

session: loop                       -- intra session loop starts here

  -- get request header

  NCB.all              := NCBinit.all;
  NCB.Command          := NB_Receive_NoWait;
  NCB.BufferPtr        := pkt(1)'address;
  NCB.BufferLength     := pkts'last;
  Net.Call(NCB);
  Wait;
  exit session when NCB.ReturnCode /= NB_Ok;      -- abort session

  Console.CkIn(SessionNumber => LocalSession);
  pkt(4..pkts'last) := pkt(3..pkts'last-1);
  pkt(3)    := 0;
  rh        := Pkts_to_Rhs(pkt);

  case rh.rhX.command is
    when DeviceReadLong =>
      if rh.rhX.ReadLong.SectorsToRead = Word_to_W(0) then
        rh.rhX.ReadLong.Status := DeviceDone;
        -- send rh back to client
        SendRH;
        exit session when NCB.ReturnCode /= NB_Ok;
      else
        if W_to_Word(rh.rhX.ReadLong.SectorsToRead) > MaxBlocks then
          -- request to big, chop to fit.
          rh.rhX.ReadLong.SectorsToRead := Word_to_W(MaxBlocks);
        end if;
        loop -- retry storage allocation until enough memory to alloc
          begin
          -- local block for exception handler
          dta := new types.bytes(1..
                     W_to_Word(rh.rhX.ReadLong.SectorsToRead)*(BlockSize));
          AllocError := false;
          exception
            when storage_error    => AllocError := true;
          end;  -- end of local block
          exit when not AllocError;
          delay(0.0);
        end loop;
          --  pass rh on to the CD
          dtaSave := rh.rhX.ReadLong.DtaPtr;       -- save remote dta ptr
          rh.rhX.ReadLong.dtaPtr:=bytesAccess_to_DW(dta);  -- point to local
          CDs.Call(rh);                             -- device status now in rhX
          rh.rhX.ReadLong.DtaPtr := dtaSave;       -- restore for return
          -- send rh back to client
          SendRH;
          if NCB.ReturnCode /= NB_Ok then
             ZapBytes(dta);
             exit session;
          end if;
          -- send dta back to client
          NCB.all := NCBinit.all;
          NCB.BufferLength:= W_to_Word(rh.rhX.ReadLong.SectorsToRead)*(BlockSize);
          if rh.rhX.ReadLong.status = DeviceDone
             and then NCB.BufferLength /= 0 then
            NCB.Command          := NB_Send_NoWait;
            NCB.BufferPtr        := dta(1)'address;
            Net.Call(NCB);
            Wait;
            if NCB.ReturnCode /= NB_Ok then
              ZapBytes(dta);
              exit session;
            end if;
          end if;
      ZapBytes(dta);
      end if;
    when DeviceIoctlInput =>
        --  get the Command Block from client
        NCB.all              := NCBinit.all;
        NCB.Command          := NB_Receive_NoWait;
        NCB.BufferPtr        := CB(1)'address;
        NCB.BufferLength     := W_to_word(rh.rhX.IoctlIn.TransferCount);
        Net.Call(NCB);              -- get the CB
        Wait;                               -- until command completes
        exit session when NCB.ReturnCode /= NB_Ok;  -- abort session
        --  pass it on to the CD
        CBSave       := rh.rhX.IoctlIn.CBPtr;            -- save remote CB ptr
        rh.rhX.IoctlIn.CBPtr := SA_to_DW(CB(1)'address); -- point to local CB
        CDs.Call(rh);     -- device status now in rhX
        -- send rh back to client
        rh.rhX.IoctlIn.CBPtr := CBSave;        -- set dta back for return
        SendRH;
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session
        -- send CB back to client
        NCB.all              := NCBinit.all;
        NCB.Command          := NB_Send_NoWait;
        NCB.BufferPtr        := CB(1)'address;
        NCB.BufferLength     := W_to_Word(rh.rhX.IoctlIn.TransferCount);
        Net.Call(NCB);
        Wait;
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session
    when DeviceIoctlOutput =>
        --  get the Command Block from client
        NCB.all              := NCBinit.all;
        NCB.Command          := NB_Receive_NoWait;
        NCB.BufferPtr        := CB(1)'address;
        NCB.BufferLength     := W_to_word(rh.rhX.IoctlOut.TransferCount);
        Net.Call(NCB);              -- get the CB
        Wait;                               -- until command completes
        exit session when NCB.ReturnCode /= NB_Ok;  -- abort session
        --  pass it on to the CD
        CBSave       := rh.rhX.IoctlOut.CBPtr;   -- save remote CB ptr
        rh.rhX.IoctlOut.CBPtr := SA_to_DW(CB(1)'address); -- point to local CB
        CDs.Call(rh);     -- device status now in rhX
        -- send rh back to client
        rh.rhX.IoctlOut.CBPtr         := CBSave;       -- set dta back for return
        SendRH;
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session
    when DeviceInit =>
        rh.rhX.Init.NumberUnits := CDSubUnits;  -- for MSCDEX only, client
        rh.rhX.Init.Status := DeviceDone;       -- always tells DOS 1
        -- send rh back to client
        SendRH;
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session
    when DeviceSeek | DeviceReadLongPrefetch =>
        --  pass it on to the CD
        CDs.Call(rh);     -- device status now in rhX
        -- send rh back to client
        SendRH;
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session

    when Others =>      -- should never come here, but !!!
        rh.rhX.Other.Status := DeviceDone OR
                               DeviceUnknownCommand OR
                               DeviceError;
        SendRH;          -- just send it back
        exit session when NCB.ReturnCode /= NB_Ok;   -- aborts session

  end case;

end loop session;

  -- session aborted to get here

  Console.SignOut(SessionNumber => LocalSession);

  if (NCB.ReturnCode /= NB_SessionClosed) and then
     (NCB.ReturnCode /= NB_SessionEndedAbnormally) then
     NetHangUp (LocalSession);
  end if;

end loop que;

  -- only comes here on shutdown so no need to clean up

exception

  -- not safe to zap the NCBs unless the NB lsn using it has terminated

  when others =>  tty.put(24,55,"Queue terminated.");

end Sessions;

task body Schedulers is
  Stop   : boolean:= False;
begin

loop
  select
    accept Shutdown do
      Stop := True;
    end Shutdown;
  or
    accept Listen(Net : NetAccess; Ncb : ncbAccess ) do
      N_Listen.Que(NCB);
      loop
        exit when NCB.CommandStatus /= NB_CommandPending;
        delay (0.0);
      end loop;
      N_Listen.Xit;
    end Listen;
  end select;
  exit when Stop;
  delay(0.0);
end loop;

end Schedulers;

task body LCalls is
  Stop   : boolean := False;
  Holding: boolean:= False;
  NcbListen: ncbAccess := new NetBiosCmdBlks;
begin
  loop
    select
      accept Que(Ncb: ncbAccess) do
        NcbListen.bufferptr:=ncbAccess_to_SA(NCB);
        if Holding then
            NCB.ReturnCode    := NB_CommandCanceled;
            NCB.CommandStatus := NB_CommandCanceled;
        else
            NetBiosCall (Ncb => Ncb);
        end if;
      end Que;
    or
      accept Xit do
        NcbListen.bufferptr := ncbAccess_to_SA(null);
      end Xit;
    or
      accept Cancel do
        Holding := True;
        if not(SA_to_ncbAccess(NcbListen.BufferPtr) = null) then
          NcbListen.Command := NB_Cancel;
          NetBiosCall (NcbListen);
        end if;
      end Cancel;
    or
      accept ShutDown do
        Stop := True;
      end ShutDown;
    end select;
    exit when Stop;
  end loop;
end LCalls;

task body Nets is
  Stop   : boolean := False;
  Holding: boolean:= False;

  begin
    accept Start(Name: string16) do
      NetAddName(NetName => Name);
    end Start;
  loop
    select
      accept Call(Ncb: ncbAccess) do
        if Holding then
          NCB.ReturnCode    := NB_CommandCanceled;
          NCB.CommandStatus := NB_CommandCanceled;
        else
          NetBiosCall (Ncb => Ncb);
        end if;
      end Call;
    or
      accept Hold do
        Holding := True;
        N_Listen.Cancel;
      end Hold;
    or
      accept ShutDown do
        Stop := True;
        N_Listen.Shutdown;
      end ShutDown;
    end select;
    exit when Stop;
  end loop;
end Nets;

end ServerTasks;
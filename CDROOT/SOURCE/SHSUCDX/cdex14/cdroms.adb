--************************************************************************
--
--  CDROMS.ADB               Version 3.1
--
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with system;
with text_io; use text_io;
with memory, unchecked_conversion;
with tty;

package body CDRoms is

CDTbl          : CDArrayAccess;

procedure GetCDStatus(Drive       : integer;
                      DeviceStatus: out DEV_ReturnCodes;
                      CDStatus    : out DW;
                      DriverName  : out string16;
                      DriverUnit  : out byte;
                      Label       : out string11) is

rh             : rhs;
dta            : bytesAccess;
CB             : CBAccess := new CBs;
begin
   CB(1):= Ioctl_GetCDStatus;
   Label := ("           ");
   rh := ( Length  => 2+rhXs'size/8,
           SubUnit => byte(Drive),
           rhX     => (DeviceIoctlInput,
                      (Status        => DeviceError,
                       reserved      => (others =>0),
                       MediaDesc     => 0,
                       CBPtr         => SA_to_DW(CB(1)'address),
                       TransferCount => word_to_W(5),
                       Start         => word_to_W(0),
                       VolIdPtr      => SA_to_DW(0) )) );

   CDs.Call(rh);
   delay(0.0);
   DeviceStatus := rh.rhX.IoctlIn.status;
   if rh.rhX.IoctlIn.status = DeviceDone then
     CDStatus := CB(2..5);
     if (CB(2) AND byte(1) ) = 0 then  -- door closed
       dta := new bytes(1..2048);
       rh := (Length  => 2+rhXs'size/8,
              SubUnit => byte(Drive),
              rhX     => (DeviceReadLong,
                         (Status          => DeviceError,
                          reserved        => (others =>0),
                          AddressMode     => 0,
                          DtaPtr          => SA_to_DW(dta(1)'address),
                          SectorsToRead   => word_to_W(1),
                          StartSector     => Long_to_DW(16),
                          ReadMode        => 0,
                          InterleaveSize  => 0,
                          InterleaveSkip  => 0,
                          filler          => (0,0))));
       CDs.Call(rh);
       delay(0.0);
       if rh.rhX.ReadLong.status = DeviceDone then
         if dta(2..6) = string_to_bytes("CD001") then
           CDTbl(Drive).Label:= bytes_to_string(dta(41..51));
           Label := CDTbl(Drive).Label;
           CDTbl(Drive).VolSize := dta(81..84);
         elsif dta(10..14) = string_to_bytes("CDROM") then
           CDTbl(Drive).Label:= bytes_to_string(dta(49..59));
           Label := CDTbl(Drive).Label;
           CDTbl(Drive).VolSize := dta(89..92);
         else --  we don't know where label is, blank it
           CDTbl(Drive).Label:= (others=>' ');
         end if;
       end if;
       ZapBytes(dta);
       DeviceStatus := rh.rhX.ReadLong.status;
     end if;
   end if;
   ZapCBs(CB);
   DriverName :=(others=>' ');
   if CDTbl(Drive).EType = CD then
     DriverName(1..8) := CDTbl(Drive).Driver.Name(1..8);
   else
     DriverName := CDTbl(Drive).File.Name(1..16);
   end if;
   DriverUnit := CDTbl(Drive).Unit;
end GetCDStatus;

task body CDRoms is
  Stop            : boolean := False;
  LastCD          : integer := -1;
  pkt             : pkts;
  rh              : rhs;
  CB             : CBAccess := new CBs;

  procedure CallCDDriver (rh: in out rhs) is
    CDIndex         : integer;
  begin
    CDIndex         := integer(rh.SubUnit);
    if CDIndex > LastCD then
      -- the following is a kludge
      -- bytes 5 & 6 in rh are device return code(status) for all subcommands
      pkt    := Rhs_to_Pkts(rh);  -- just convert dont't shift
      pkt(5..6) := W(DeviceError OR DeviceDone OR DeviceUnknownUnit);
      rh     := Pkts_to_Rhs(pkt);
    elsif (CDTbl(CDIndex).EType = IMG) then
      case rh.rhX.command is
        when DeviceReadLong =>
          DIo.Set_Index(CDTbl(CDIndex).File.Img_F.all,
                  DIO.positive_count(long_integer'succ(
                            DW_to_Long(rh.rhX.ReadLong.StartSector))));
          declare
            dtaPtr : DW :=rh.rhX.ReadLong.dtaPtr;
          begin
            for i in 1..W_to_Word(rh.rhX.ReadLong.SectorsToRead) loop
              if DIO.End_of_File(CDTbl(CDIndex).File.Img_F.all) then
                rh.rhX.ReadLong.SectorsToRead := Word_to_W(i-1);
                exit;
              end if;
              DIO.Read(CDTbl(CDIndex).File.Img_F.all,
                  DW_to_SectorsAccess(dtaPtr).all);
              dtaPtr := Long_to_DW(DW_to_Long(dtaPtr)+long_integer(Sectors'last));
            end loop;
          end;
          rh.rhX.ReadLong.Status := DeviceDone;
        when DeviceIoctlInput =>
          CB := DW_to_CBAccess(rh.rhX.IoctlIn.CBPtr);
          case CB(1) is
            when Ioctl_ReadDriveBytes => rh.rhX.IoctlIn.Status := DeviceDone;
            when Ioctl_GetCDStatus => CB.all(2..5):= CDTbl(CDIndex).Status;
                      rh.rhX.IoctlIn.Status := DeviceDone;
            when Ioctl_RetSectorSize => CB.all(2) := 0;
                      CB.all(3..4):= Word_to_W(2048);
                      rh.rhX.IoctlIn.Status := DeviceDone;
            when Ioctl_RetVolSize => CB.all(2..5):= CDTbl(CDIndex).VolSize;
                      rh.rhX.IoctlIn.Status := DeviceDone;
            when Ioctl_MediaChanged => CB.all(2):= 1;
                      rh.rhX.IoctlIn.Status := DeviceDone;
            when others => rh.rhX.IoctlIn.Status := DeviceDone OR
                                              DeviceUnknownCommand OR
                                              DeviceError;
          end case;
        when DeviceIoctlOutput =>
                       rh.rhX.IoctlOut.Status := DeviceDone;
        when DeviceReadLongPrefetch =>
                       rh.rhX.ReadLong.Status := DeviceDone;
        when DeviceSeek =>
                       rh.rhX.Seek.Status := DeviceDone;
       when Others =>  rh.rhX.Other.Status := DeviceDone OR
                                              DeviceUnknownCommand OR
                                              DeviceError;
      end case;
    else
      rh.SubUnit          := CDTbl(CDIndex).Unit;
      pkt                 := Rhs_to_Pkts(rh);
      pkt(3..pkts'last-1) := pkt(4..pkts'last);
      CallDriver (rh              => pkt(1)'address,
                  DeviceStrategy  => CDTbl(CDIndex).Driver.Strategy,
                  DeviceInterrupt => CDTbl(CDIndex).Driver.Interrupt);
      pkt(4..pkts'last)   := pkt(3..pkts'last-1);
      pkt(3)              := 0;
      rh                  := Pkts_to_Rhs(pkt);
      rh.SubUnit          := byte(CDIndex);
    end if;
  end CallCDDriver;
  pragma inline(CallCDDriver);

begin
  accept SetUp(pCDTbl: CDArrayAccess ) do
    CDTbl := pCDTbl;
    LastCd := CDTbl.all'last;
  end SetUp;
loop
  select
    accept Call (rh: in out rhs) do
      CallCDDriver(rh);
    end Call;
  or
    accept ShutDown do
      Stop := True;
    end ShutDown;
  end select;
  exit when Stop;
end loop;
end CDRoms;

end CDRoms;
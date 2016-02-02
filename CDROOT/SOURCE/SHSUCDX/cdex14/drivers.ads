--************************************************************************
--
--  DRIVERS.ADS               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with system, memory, unchecked_conversion, unchecked_deallocation;
with Types; use Types;

package drivers is

type CdDrivers is                  -- character device with cd rom extensions
record
   NextDevice        : system.address;
   Attributes        : word;                 -- 16#C800#
   Strategy          : memory.segment_offset;
   Interrupt         : memory.segment_offset;
   Name              : string8;              -- pad with blanks
   CD_Reserved       : word;     -- 0
   CD_Drive          : byte;     -- 0    first drive letter assigned by mscdex
   CD_Count          : byte;     -- >= 1 number of cd's controlled by driver
end record;


type DEV_CommandCodes is new byte;

   DeviceInit              : constant DEV_CommandCodes := 0;
   DeviceMediaCheck        : constant DEV_CommandCodes := 1;
   DeviceBuildBpb          : constant DEV_CommandCodes := 2;
   DeviceIoctlInput        : constant DEV_CommandCodes := 3;
   DeviceRead              : constant DEV_CommandCodes := 4;
   DeviceWrite             : constant DEV_CommandCodes := 8;
   DeviceIoctlOutput       : constant DEV_CommandCodes := 12;
   DeviceOpen              : constant DEV_CommandCodes := 13;
   DeviceClose             : constant DEV_CommandCodes := 14;
   DeviceReadLong          : constant DEV_CommandCodes := 128;
   DeviceReadLongPrefetch  : constant DEV_CommandCodes := 130;
   DeviceSeek              : constant DEV_CommandCodes := 131;
   DeviceDummy             : constant DEV_CommandCodes := 255;
-- DOS DRIVER STATUS

type DEV_ReturnCodes is new W;

   DeviceError       : constant DEV_ReturnCodes := Word_to_W(16#8000#);
   DeviceBusy        : constant DEV_ReturnCodes := Word_to_W(16#0200#);
   DeviceDone        : constant DEV_ReturnCodes := Word_to_W(16#0100#);
   DeviceUnknownUnit : constant DEV_ReturnCodes := Word_to_W(16#0001#);
   DeviceNotReady    : constant DEV_ReturnCodes := Word_to_W(16#0002#);
   DeviceUnknownCommand : constant DEV_ReturnCodes := Word_to_W(16#0003#);
   DeviceFailure     : constant DEV_ReturnCodes := Word_to_W(16#000C#);
   InvalidDiskChange : constant DEV_ReturnCodes := Word_to_W(16#000F#);


-- DOS DRIVER Exceptions

   DEV_Error         : exception;
   DEV_NameError     : exception;
   DEV_IOCtlError    : exception;
   DEV_NotFound      : exception;
   DEV_NotReady      : exception;

type rhInits is record
      Status              : DEV_ReturnCodes;
      reserved            : bytes(6..13);
      NumberUnits         : byte;
      EndAddress          : DW;
      CommandLine         : DW;
      BlockDeviceNumber   : byte;
   end record;

type rhIoctlIns is record
      Status              : DEV_ReturnCodes;
      reserved            : bytes(6..13);
      MediaDesc           : byte;
      CBPtr               : DW;
      TransferCount       : W;
      Start               : W;
      VolIdPtr            : DW;
   end record;

type rhIoctlOuts is record
      Status              : DEV_ReturnCodes;
      reserved            : bytes(6..13);
      MediaDesc           : byte;
      CBPtr               : DW;
      TransferCount       : W;
      Start               : W;
      VolIdPtr            : DW;
   end record;

type rhReadLongs is record
      Status              : DEV_ReturnCodes;
      Reserved            : bytes(6..13);
      AddressMode         : byte;
      DtaPtr              : DW;
      SectorsToRead       : W;
      StartSector         : DW;
      ReadMode            : byte;
      InterleaveSize      : byte;
      InterleaveSkip      : byte;
      filler              : W;        -- ?? MSCDEX V2.21 says length is 29
   end record;

type rhSeeks is record
      Status              : DEV_ReturnCodes;
      Reserved            : bytes(6..13);
      AddressMode         : byte;
      DtaPtr              : DW;
      SectorCount         : W;
      StartSector         : DW;
   end record;

type rhOthers is record
      Status              : DEV_ReturnCodes;
      filler              : bytes(6..32);
   end record;

type rhXs (Command: DEV_CommandCodes:= DeviceReadLong) is
   record
   case Command is
      when DeviceInit             =>  Init    : rhInits;
      when DeviceIoctlInput       =>  IoctlIn : rhIoctlIns;
      when DeviceIoctlOutput      =>  IoctlOut: rhIoctlOuts;
      when DeviceReadLong |
           DeviceReadLongPrefetch =>  ReadLong: rhReadLongs;
      when DeviceSeek             =>  Seek    : rhSeeks;
      when others                 =>  Other   : rhOthers;
   end case;

   end record;

type rhs is
   record
      Length     : byte;
      SubUnit    : byte;        -- big problem here
      rhX        : rhXs;        -- byte 3 is a constraint flag inserted by ADA
   end record;                  -- byte 4 command

subtype pkts is bytes(1..rhs'size/8);

CBMax       : constant := 131;      -- max command block size
subtype CBs is bytes(1..CBMax);
type CBAccess is access CBs;

function DW_to_CBAccess is new unchecked_conversion(DW, CBAccess);
function CBAccess_to_DW is new unchecked_conversion(CBAccess, DW);
procedure ZapCBs is new unchecked_deallocation(CBs,CBAccess);

function Rhs_to_Pkts is new unchecked_conversion(Rhs, Pkts);
function Pkts_to_Rhs is new unchecked_conversion(Pkts, Rhs);

subtype IoctlInSubCommand is byte;
type IoctlOutSubCommand is new byte;

Ioctl_RetDevHdrAddr  : constant IoctlInSubCommand := 0;
Ioctl_ReadDriveBytes : constant IoctlInSubCommand := 5;
Ioctl_GetCDStatus    : constant IoctlInSubCommand := 6;
Ioctl_RetSectorSize  : constant IoctlInSubCommand := 7;
Ioctl_RetVolSize     : constant IoctlInSubCommand := 8;
Ioctl_MediaChanged   : constant IoctlInSubCommand := 9;

type IoCB_RetDevHdrAddrs is
   record
      RDHACode           : IoctlInSubCommand := Ioctl_RetDevHdrAddr;
      DeviceHeaderAddress: DW;
   end record;

procedure OpenDevice (DeviceName : string8;
                      Handle     : out integer);

procedure CloseDevice (Handle: integer);

procedure GetDeviceEntryAddresses (Handle           : integer;
                                   DeviceStrategy   : out system.address;
                                   DeviceInterrupt  : out system.address;
                                   SubUnits         : out byte          );

procedure CallDriver (rh             : system.address;
                      DeviceStrategy : system.address;
                      DeviceInterrupt: system.address);

end drivers;
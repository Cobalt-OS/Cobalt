--************************************************************************
--
--  CDROMS.ADS               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with Types; use Types;
with Drivers; use Drivers;
with system, unchecked_conversion, direct_io, unchecked_deallocation;
with text_io;

package CDRoms is

-- The server joins the CDs sequentially with the first as drive 0.
-- All calls to CDRoms use these drive number.  CDRoms does a table
-- lookup to find the correct driver and subunit on that driver.

subtype Sectors is bytes(1..2048);
package DIO is new Direct_IO(Sectors); use DIO;
type SectorsAccess is access Sectors;
function DW_to_SectorsAccess is new unchecked_conversion(DW, SectorsAccess);

type EntryType is (IMG, CD);
type IMG_Access is access DIO.File_Type;
package ET_IO is new text_io.enumeration_io(EntryType);
procedure zapImg_F is new unchecked_deallocation(DIO.File_Type,IMG_Access);
procedure zapSectors is new unchecked_deallocation(Sectors,SectorsAccess);

type Drives is
  record
    Name        : string8;
    Strategy    : system.address;
    Interrupt   : system.address;
  end record;

type Images is
  record
    Name        : string(1..24);
    Img_F       : IMG_Access;
  end record;

type CDEntries(EType: EntryType := IMG) is
  record
    Unit        : byte;
    Label       : string11 := (others=>' ');
    Status      : DW := Long_to_DW(0);
    VolSize     : DW := Long_to_DW(0);
    case EType is
      when CD  => Driver : Drives;
      when IMG => File   : Images;
    end case;
  end record;

type CDArray is array (integer range <>) of CDEntries;
type CDArrayAccess is access CDArray;

procedure GetCDStatus(Drive       : integer;
                      DeviceStatus: out DEV_ReturnCodes;
                      CDStatus    : out DW;
                      DriverName  : out string16;
                      DriverUnit  : out byte;
                      Label       : out string11);

task type CDRoms is
  entry SetUp(pCDTbl: CDArrayAccess );
  entry Call(rh: in out rhs);
  entry ShutDown;
  pragma priority(10);
end CDRoms;
for CDRoms'storage_size use 3584;

type CDAccess is access CDRoms;

CDs : CDRoms;

end CDRoms;
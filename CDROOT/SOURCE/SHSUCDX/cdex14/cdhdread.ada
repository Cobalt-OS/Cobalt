--************************************************************************
--
--  CDREADER.ADA               Version 3.1
--
--     This program reads the primary descriptor from a CD ROM cache iamge
--     created by the CDCACHER program.
--
--  A copyright-reserved, free use program.  Use at your own risk.
--  (c)John H. McCoy, 1994, 1995, 1996 Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with types; use types;
with unchecked_conversion, system;
with drivers; use drivers;
with text_io; use text_io;
with direct_io;

procedure CDHDRead is
package BIO is new integer_io(byte); use BIO;

subtype Sectors is bytes(1..2048);
type SAccess is access Sectors;

function SA_to_DW is new unchecked_conversion(system.address, DW);
function DW_to_SAccess is new unchecked_conversion(DW, SAccess);

S: SAccess := new Sectors;
Sector: SAccess := DW_to_SAccess(SA_to_DW(S(1)'address));


package DIO is new Direct_IO(Sectors); use DIO;

subtype String12 is string(1..12);
Img_F: DIO.File_Type;
Img_FName : String12 := "HDCD0001.IMG";

SSector,ESector: long_integer;

type IsoPrimaryDescriptors is
    record
        DescriptorType          : bytes(1..1);
        ID                      : string(2..6);
        Version                 : bytes(7..7);
        R1                      : bytes(8..8);
        SystemID                : string(9..40);
        VolumeID                : string(41..72);
        R2                      : bytes(73..80);
        VolSpaceSize            : bytes(81..84);
        R3                      : bytes(85..2048);
    end record;

function bytes_to_string is new unchecked_conversion(bytes, string);
ISO            : IsoPrimaryDescriptors;
DirSN          : constant:= 16;
ImageBlock     : long_integer;

begin

open(Img_F,in_File,IMG_FName);
ImageBlock := long_integer'succ(DirSN);
read(Img_F,Sector.all,DIO.positive_count(ImageBlock));

ESector   := DW_to_Long(Sector(81..84));
put_line("Space: "&long_integer'image(ESector));
for i in S'range loop
 put(S(i));
end loop;
new_line;
put_line("done");
Close(Img_F);


end CDHDRead;
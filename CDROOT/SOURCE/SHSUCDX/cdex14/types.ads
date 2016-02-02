--************************************************************************
--
--  TYPES.ADS               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with system, memory;
with calendar;
with unchecked_conversion, unchecked_deallocation;

package Types is

type byte is range 0..255;
type word is range 0..65_535;

--  The following types and functions are declared for use instead of
--  words, and system.address to avoid "alignment holes" in record
--  declarations.  This may not be a problem with other than Meridian's
--  implementation fo ADA.

type bytes is array (word range <>) of byte;
type words is array (word range <>) of word;
subtype  W is bytes(1..2);
subtype DW is bytes(1..4);
function W_to_Word is new unchecked_conversion(W, Word);
function DW_to_SA is new unchecked_conversion(DW, system.address);
function DW_to_Long is new unchecked_conversion(DW, Long_Integer);
function Word_to_W is new unchecked_conversion(Word, W);
function SA_to_DW is new unchecked_conversion(system.address, DW);
function Long_to_DW is new unchecked_conversion(Long_Integer, DW);

type bytesAccess is access bytes;
function SA_to_BytesAccess is new unchecked_conversion(system.address, bytesAccess);
function BytesAccess_to_SA is new unchecked_conversion(bytesAccess, system.address);
function DW_to_BytesAccess is new unchecked_conversion(DW, bytesAccess);
function BytesAccess_to_DW is new unchecked_conversion(bytesAccess, DW);
procedure ZapBytes is new unchecked_deallocation(bytes,bytesAccess);
function bytes_to_string is new unchecked_conversion(bytes, string);
function string_to_bytes is new unchecked_conversion(string, bytes);


subtype string5  is string(1..5);
subtype string8  is string(1..8);
subtype string11 is string(1..11);
subtype string16 is string(1..16);


function "+"(left, right: byte) return byte;
function "OR"(left, right: byte) return byte;
function "AND"(left, right: byte) return byte;
function "+"(left, right: word) return word;
function "OR"(left, right: word) return word;
function "AND"(left, right: word) return word;
function "+"(left, right: W) return W;
function "OR"(left, right: W) return W;
function "AND"(left, right: W) return W;

function TOD (Date: calendar.time) return string5;
function MDY (Date: calendar.time) return string8;

end Types;
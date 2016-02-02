--************************************************************************
--
--  TYPES.ADB               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994,1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************
with text_io; use text_io;
with bit_ops, unchecked_conversion;
package body Types is

function byte_to_byte_integer is new
   unchecked_conversion(byte, byte_integer);
function byte_integer_to_byte is new
   unchecked_conversion(byte_integer, byte);

function "+"(left, right: byte) return byte is
begin
   return byte_integer_to_byte(byte_to_byte_integer(left) +
                               byte_to_byte_integer(right));
end "+";

function "OR"(left, right: byte) return byte is
use bit_ops;
begin
   return byte_integer_to_byte(byte_to_byte_integer(left) OR
                               byte_to_byte_integer(right));
end "OR";

function "AND"(left, right: byte) return byte is
use bit_ops;
begin
   return byte_integer_to_byte(byte_to_byte_integer(left) AND
                               byte_to_byte_integer(right));
end "AND";

function word_to_integer is new
   unchecked_conversion(word, integer);
function integer_to_word is new
   unchecked_conversion(integer, word);

function "+"(left, right: word) return word is
begin
   return integer_to_word(word_to_integer(left) +
                          word_to_integer(right));
end "+";

function "OR"(left, right: word) return word is
use bit_ops;
begin
   return integer_to_word(word_to_integer(left) OR
                          word_to_integer(right));
end "OR";

function "AND"(left, right: word) return word is
use bit_ops;
begin
   return integer_to_word(word_to_integer(left) AND
                          word_to_integer(right));
end "AND";

function W_to_integer is new
   unchecked_conversion(W, integer);
function integer_to_W is new
   unchecked_conversion(integer, W);

function "+"(left, right: W) return W is
begin
   return integer_to_W(W_to_integer(left) +
                          W_to_integer(right));
end "+";

function "OR"(left, right: W) return W is
use bit_ops;
begin
   return integer_to_W(W_to_integer(left) OR
                          W_to_integer(right));
end "OR";

function "AND"(left, right: W) return W is
use bit_ops;
begin
   return integer_to_W(W_to_integer(left) AND
                          W_to_integer(right));
end "AND";

function TOD (Date: calendar.time) return string5 is
     Sec : calendar.Day_Duration := (calendar.Seconds(Date));
     Min : integer ;
     Hr  : integer ;
     T   : string5 := "  :  ";
     subtype Numerals is character range '0'..'9';
     Offset : integer := character'pos(Numerals'first);
begin
     Min := integer(Sec/60);
     Hr  := Min/60;
     Min := Min - Hr * 60;
     if Hr = 24 then
        Hr := 0;
     end if;
     T(1) := Numerals'val(Hr/10 + Offset);
     T(2) := Numerals'val(Hr rem 10 + Offset);
     T(4) := Numerals'val(Min/10 + Offset);
     T(5) := Numerals'val(Min rem 10 + Offset);
     return T;
end TOD;

function MDY (Date: calendar.time) return string8 is
     M : calendar.month_number := calendar.month(Date);
     D : calendar.day_number := calendar.day(Date);
     Y : integer := calendar.year(Date) - 1900;
     T : string8 := "  /  /  ";
     subtype Numerals is character range '0'..'9';
     Offset : integer := character'pos(Numerals'first);
begin
put_line(integer'image(Y));
     T(1) := Numerals'val(M/10 + Offset);
     T(2) := Numerals'val(M rem 10 + Offset);
     T(4) := Numerals'val(D/10 + Offset);
     T(5) := Numerals'val(D rem 10 + Offset);
     T(7) := Numerals'val(Y/10 + Offset);
     T(8) := Numerals'val(Y rem 10 + Offset);
     return T;
end MDY;

end Types;
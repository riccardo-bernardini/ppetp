with Text_Io;
use  Text_Io;

package body Make_Timestamp is

   function Next_Timestamp return Unsigned_32 is

   begin
      Put_line("in=" & Unsigned_32'Image(Time));
      Time := Time + 1;
      Put_line("out=" & Unsigned_32'Image(Time));
      return Time;

   end Next_Timestamp;

   function Current_Timestamp return Unbounded_String is
      Time_Hex : String(1..6);
   begin
      To_Hex(Time + 1, 6, Time_Hex);
      return To_Unbounded_String(Time_Hex);
   end Current_Timestamp;


end Make_Timestamp;

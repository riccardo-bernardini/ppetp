with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Interfaces;
use  Interfaces;

procedure Timestamp_Function(Count : out Interfaces.Unsigned_32) is
   Value : Interfaces.Unsigned_32 := 15;
begin
   Count := Value + 1;
end Timestamp_Function;


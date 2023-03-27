--  Funzione per la conversione di uno Stream_Element_Array
--  in una Stringa.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Characters;
use  Ada.Characters;

with Ada.Streams;
use  Ada.Streams;

function Byte_To_String(Input : in Stream_Element_Array) return Unbounded_String is

   Output : String(Integer(Stream_Element_Offset(Input'first))..
                   Integer(Stream_Element_Offset(Input'Last)));

begin
   for Count in Input'Range loop
      Output(Integer(Count)) := Character'Val(Input(Stream_Element_Offset(Count)));
   end loop;

   return To_Unbounded_String(Output);

end Byte_To_String;





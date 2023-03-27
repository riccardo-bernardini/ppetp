--  Funzione per la conversione di una stringa in un Stream_Element_Array.
--  Richiede come parametro una stringa e fornisce in uscita uno
--  Stream_Element_Array.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Characters;
use  Ada.Characters;

with Ada.Streams;
use  Ada.Streams;

function To_Byte(Input  : in Unbounded_String) return Stream_Element_Array is

   Output : Stream_Element_Array(Stream_Element_Offset(To_String(Input)'First)..
                                 Stream_Element_Offset(To_String(Input)'Last));

begin

   for Count in To_String(Input)'Range loop
      Output(Stream_Element_Offset(Count)) := Character'Pos(To_String(Input)(Count));
   end loop;

   return Output;

end To_Byte;




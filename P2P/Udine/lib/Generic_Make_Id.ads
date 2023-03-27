--  Funzione per la generazione dell'ID del client.

with Id_Counters;
use  Id_Counters;

with Text_IO;
use  Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

generic

   Dim : Positive;

package Generic_Make_Id is

   type String_Array is array(Positive range <>) of Unbounded_String;

   --     function Make_Id_Current return Unbounded_String;

   --  Funzione per la generazione dell'ID del client.

   function Make_Id_Current return Id;

   --  Funzione che accetta come parametro uno String_Array e ne restituisce
   --  il contenuto in forma di Stringa.

   function Image(X : String_Array) return String;

   --  Funzione che accetta come parametro un type Id, definito nel package
   --  Id_Counters, e restituisce tale valore in forma di Unbounded_String.

   function Id_To_UString(Input : in Id) return Unbounded_String;

private

   C      : Counter;

end Generic_Make_Id;

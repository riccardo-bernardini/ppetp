with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Tokenize;
use  Tokenize;

with Interfaces;
use  Interfaces;

with Make_Timestamp;
use  Make_Timestamp;

with To_Hex;

with Create_CRC32;

package Make_String is

   type String_Array is array(0..2) of Unbounded_String;

   --  Funzione che accetta come parametro uno String_Array
   --  e resituisce tale valore in forma di Stringa.

   function Image(X : String_Array) return String;

end Make_String;

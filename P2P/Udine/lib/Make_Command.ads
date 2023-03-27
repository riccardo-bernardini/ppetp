--                  -*- Mode: Ada -*-
--  Filename        : Make_Command.ads
--  Description     : Make Command
--
--  Package for the construction of the Strings that identify every message,
--  which will be sent or received by the sockets.
--
--  It is mainly constituted by two functions: Make_Command_1 and Image.
--
--  Make_Command_1 is a Function that accepts as parameters a String,
--  correspondent to the name of the command and a String_Array,
--  that serve to complete the message corresponding to such command.
--  Inside such function the timestamp will also be produced and
--  will be calculated the relative code CRC32, that respectively corresponds
--  to the second and last field of every message.
--
--  Image accepts as parameter a String_Array and resituisce
--  such value in form of String.


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

package Make_Command is

   type String_Array is array(Positive range <>) of Unbounded_String;

   --  Funzione che accetta come parametri una stringa, corrispondente al
   --  nome del comando, e uno String_Array, che servono a completare
   --  il messaggio corrispondente a tale comando.
   --  All'interno di tale funzione verrà anche generato il timestamp
   --  e sarà calcolato il relativo codice CRC32, che corrispondono
   --  rispettivamente al secondo e ultimo campo di ciascun messaggio.

   function Make_Command_1(Command    : in String;
                           Parameters : in String_Array) return String;

   --  Funzione che accetta come parametro uno String_Array
   --  e resituisce tale valore in forma di Stringa.

   function Image(X : String_Array) return String;

end Make_Command;

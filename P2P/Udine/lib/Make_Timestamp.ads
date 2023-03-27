--                  -*- Mode: Ada -*-
--  Filename        : Make_Timestamp.ads
--  Description     : Make Timestamp
--
--  Package for the generation of the timestamp.
--
--  It is mainly constituted by two functions:
--  Next_Timestamp and Current_Timestamp
--
--  Next_Timestamp simply every time that will be called,
--  increases the value of the timestamp.
--
--  Crurrent_Timestamp returns me the current timestamp.
--  This value I can use it then in the moment in which
--  i will want to record such value in the hash_auth.

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with To_Hex;

with Interfaces;
use  Interfaces;

package Make_Timestamp is

   function Next_Timestamp return Unsigned_32;

   function Current_Timestamp return Unbounded_String;

private

   Time : Unsigned_32 := 0;

end Make_Timestamp;

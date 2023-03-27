--                  -*- Mode: Ada -*-
--  Filename        : To_Hex.adb
--  Description     : Convert to Hexadecimal
--
--  Procedure for the conversion of a decimal number in hexadecimal.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Characters;
use  Ada.Characters;

with Interfaces;
use  Interfaces;


procedure To_Hex(Input : in Unsigned_32; Digit : in Positive;  Y : out String) is
   Vet_Aux : String := ("0123456789ABCDEF");
   Vet     : array(1..Digit) of Unsigned_32;
   X       : Unsigned_32 := Input;
begin
   for Count in reverse 1 .. Digit loop
      Vet(Count) := (X rem 16);
      X := X / 16;
   end loop;
   for Count in 1 .. Digit loop
      Y(Count) := Vet_Aux(Integer(Vet(Count))+1);
   end loop;

end To_Hex;



--                  -*- Mode: Ada -*-
--  Filename        : Int_To_Hex.adb
--  Description     : Convert Integer to Hexadecimal
--
--  Procedure for the conversion of an integer to hexadecimal.



with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Characters;
use  Ada.Characters;

with Interfaces;
use  Interfaces;


procedure Int_To_Hex(Input : in Integer; Digit : in Positive;  Y : out String) is
   Vet_Aux : String := "0123456789ABCDEF";
   Vet     : array(1..Digit) of Integer;
   X       : Integer := Input;
begin
   for Count in reverse 1 .. Digit loop
      Vet(Count) := (X rem 16);
      X := X / 16;
   end loop;
   for Count in 1 .. Digit loop
      Y(Count) := Vet_Aux(Integer(Vet(Count))+1);
   end loop;

end Int_To_Hex;

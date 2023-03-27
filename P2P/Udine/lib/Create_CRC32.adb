--                  -*- Mode: Ada -*-
--  Filename        : Create_CRC32.adb
--  Description     : Create CRC32
--
--  Procedure for the calculation of the Hexadecimal String CRC32.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.CRC32;
use  GNAT.CRC32;

with Interfaces;

with To_Hex;

procedure Create_CRC32(Input : in String; Output : out String) is
   C : CRC32;
   A : Interfaces.Unsigned_32;
begin
   Initialize(C);
   Update(C, Input);
   A := Get_Value(C);
   To_Hex(A, 8, Output);
end Create_CRC32;


--                  -*- Mode: Ada -*-
--  Filename        : MD5_Function.adb
--  Description     : MD5 Function
--
--  Procedure for the coding MD5.

with GNAT.MD5;
use  GNAT.MD5;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

procedure MD5_Function(Input : in String; Output : out String) is

begin
   Output := Digest(Input);
--     Put_Line(Output);
end MD5_Function;



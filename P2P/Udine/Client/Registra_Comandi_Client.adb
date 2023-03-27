--                  -*- Mode: Ada -*-
--  Filename        : Registra_Comandi_Server.adb
--  Description     : Register Server Commands

--  Procedure for the recording of the commands recognized by the Client.


with Ada.Text_IO;
use  Ada.Text_IO;

with Tokenize;
use  Tokenize;

with Parsers;
use  Parsers;

with Ada.Exceptions;
use  Ada.Exceptions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Process_Function;
use  Process_Function;

procedure Registra_Comandi_Client(P : in out Parser) is

   Failed : exception;

   type Test_Case is record
      Command    : Unbounded_String;
      Callback   : Callback_Function;
   end record;


     Test : array (positive range <>) of Test_Case
     := (1 =>
           (Command    => To_Unbounded_String("AUTH"),
            Callback   => Auth_Process'Access),
         2 =>
           (Command    => To_Unbounded_String("ID"),
            Callback   => Id_Process'Access),
         3 =>
           (Command    => To_Unbounded_String("SEND"),
            Callback   => Send_Process'Access),
         4 =>
           (Command    => To_Unbounded_String("PLAY"),
            Callback   => Play_Process'Access)
        );

   procedure Esegui (T : Test_Case) is
   begin
      Put_Line("The command " & To_String(T.Command) & " is available");
   end Esegui;
begin
   for I in Test'Range loop
      P.Register(Test(I).Command, Test(I).Callback);
   end loop;

   for I in Test'Range loop
      Put("Test#" & Integer'Image(I) & "...");
      Esegui(Test(I));
   end loop;
end Registra_Comandi_Client;

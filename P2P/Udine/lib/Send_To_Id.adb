--                  -*- Mode: Ada -*-
--  Filename        : Send_To_Id.adb
--  Description     : Send a message to a corrispondently client.
--
--  Procedure that accepts as parameter a record type Id_Data and it deals him
--  with to send the command SEND to the opportune client.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Send_Command;

with Actual_Make_Id;

with Hash_Id;
use  Hash_Id;

with Int_To_Hex;

procedure Send_To_Id(Input : in Id_Data) is
   Hex_Port    : String(1..4);
   Address     : Unbounded_String;
   New_Address : Unbounded_String;
   Port        : Integer;
   Group       : constant String  := "127.0.0.1";
begin
   Address := To_Unbounded_String(Image(Input.Address));
--     Put_Line(To_String(Address));
   Port := Integer(Input.UDP_Data);
   Int_To_Hex(Port, 4, Hex_Port);
   New_Address := Address & To_Unbounded_String("U")
                  & To_Unbounded_String(Hex_Port);
   Send_Command("SEND", ( 1 => To_Unbounded_String(Integer'Image(Input.N_Flusso)),
                          2 => New_Address
                         ),
                             (Family => Family_Inet,
                              Addr   => Inet_Addr(Group),
                              Port   => Input.UDP_Control));
end Send_To_Id;


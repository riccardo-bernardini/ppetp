--                  -*- Mode: Ada -*-
--  Filename        : Send_Command.adb
--  Description     : Send Command
--

--  Procedure for the dispatch of messages among the sockets
--
--  Accept as parameters the identification String of the command,
--  a list of the other parameters of the message and the socket address
--  to which to send such message.



with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Tokenize;
use  Tokenize;

with Interfaces;
use  Interfaces;

with GNAT.Sockets;
use  GNAT.Sockets;

with Make_Command;
use  Make_Command;

with Send_To_Socket;

procedure Send_Command(Command    : in String;
                       Parameters : in String_Array;
                       Address    : in Sock_Addr_Type;
                       Socket_Mode : Mode_Type := Socket_Datagram) is
begin
   declare
      Total : String := Make_Command_1(Command, Parameters);
   begin
      Send_To_Socket(To_Unbounded_String(Total), Address, Socket_Mode);
   end;
end Send_Command;

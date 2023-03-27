--                  -*- Mode: Ada -*-
--  Filename        : Receive_From_Socket.adb
--  Description     : Receive From Socket
--
--  Procedure for the receipt of a message from a socket.
--
--  Accept as parameter a variable type Socket and it furnishes
--  in exit a String corresponding to the received message
--  and the socket address from which such message originates.


with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with Byte_To_String;

procedure Receive_From_Socket(Socket  :  in  Socket_Type;
                              Message :  out Unbounded_String;
                              From    :  out Sock_Addr_Type) is

   Last    : Stream_Element_Offset;
   Item    : Stream_Element_Array(1..1024);
                                  --(To_String(Message)'Last));


begin

   Receive_Socket(Socket, Item, Last, From);
   Message := Byte_To_String(Item(Item'First..Last));

end Receive_From_Socket;



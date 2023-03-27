--                  -*- Mode: Ada -*-
--  Filename        : Send_To_Socket.adb
--  Description     : Send To Socket
--
--  Procedure for the consignment of messages toward a socket.
--
--  Accept as parameters the String corresponding to the command and
--  the address of the socket recipient of the message, and it sends
--  a message type Stream_Element_Array.


with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with To_Byte;

procedure Send_To_Socket(Input : in Unbounded_String;
                         To    : in Sock_Addr_Type;
                         Socket_Mode : Mode_Type := Socket_Datagram) is

   Socket 	: Socket_Type;
   Last   	: Stream_Element_Offset;
   Address      : Sock_Addr_Type;

begin

   declare
      Item : Stream_Element_Array := To_Byte(Input);
   begin

      Address := To;

      case Socket_Mode is
         when Socket_Datagram =>
            Create_Socket(Socket, Family_Inet, Socket_Mode);
            Send_Socket(Socket, Item, Last, To);
         when Socket_Stream =>
            Create_Socket(Socket, Family_Inet, Socket_Stream);
            Connect_Socket(Socket, Address);
            Send_Socket(Socket, Item, Last, To);
            Close_Socket(Socket);
      end case;


   end;

end Send_To_Socket;

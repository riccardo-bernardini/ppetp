with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Tokenize;
use  Tokenize;

with GNAT.Sockets;
use  GNAT.Sockets;

with Send_To_Socket;

procedure Send_Flows(Parameters  : in Unbounded_String;
                     Address     : in Sock_Addr_Type;
                     Socket_Mode : Mode_Type := Socket_Datagram) is
begin

   Send_To_Socket(To_Unbounded_String(Total), Address, Socket_Mode);

end Send_Flows;

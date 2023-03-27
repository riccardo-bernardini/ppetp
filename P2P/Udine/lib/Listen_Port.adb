--  Procedura per far si che il client si metta in ascolto sulla porta UDP.
--
--  Tale procedura crea un socket e si mette in ricezione di eventuali dati
--  provenienti dal server. Fornisce in uscita il descrittore socket creato,
--  l'eventuale messaggio ricevuto e l'indirizzo dell'host da cui proviene tale
--  messaggio.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Exceptions;
use  Ada.Exceptions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with Byte_To_String;

procedure Listen_Port(Port    :  in Port_Type;
                      Message :  out Unbounded_String;
                      From    :  out Sock_Addr_Type) is

   Last     : Stream_Element_Offset;
   Item     : Stream_Element_Array(1..1024);
   Group    : constant String := "127.0.0.1";
   Address  : Sock_Addr_Type;
   Socket   :   Socket_Type;
begin

   Create_Socket (Socket, Family_Inet, Socket_Datagram);
	put_line("aaa");
   Address.Addr := Any_Inet_Addr;
   Address.Port := Port;
put_line("aab");
   Bind_Socket (Socket, Address);
put_line("aac");
   Receive_Socket(Socket, Item, Last, From);
put_line("aad");
   Message := Byte_To_String(Item(Item'First..Last));
   put_line("aax");
   Close_Socket(Socket);
end Listen_Port;

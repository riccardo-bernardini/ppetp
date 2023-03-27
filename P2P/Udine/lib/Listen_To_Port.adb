--                  -*- Mode: Ada -*-
--  Filename        : Listen_To_Port.adb
--  Description     : Listen To Port

--  Procedure for the creation of the socket and the opening
--  of a port of communication
--
--  Procedure is structured in the following way:
--
--  - Creation of the socket.
--
--  - Assignment of the Ip address of the socket.
--
--  - Called to Bind_Socket to put on in listening of the data,
--    with a loop to verify if a port has not already been used.


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

with GNAT.Sockets;
use  GNAT.Sockets;

procedure Listen_To_Port(Socket      : in out Socket_Type;
                         Port        : in  Port_Type;
                         Die_If_Busy : Boolean := False) is

   Group    : constant String := "127.0.0.1";
   Address  : Sock_Addr_Type;

begin

   --  Creazione del socket.

   Create_Socket (Socket, Family_Inet, Socket_Datagram);

   --  Assegnazione dell'indirizzo Ip del socket.

--     Address.Addr := Inet_Addr(Group);
   Address.Addr := Any_Inet_Addr;
   Address.Port := Port;

   --  Chiamata a Bind_Socket per mettersi in ascolto dei dati.
   --  Presenza di un loop per verificare che la porta
   --  non sia già stata utilizzata.

   loop
      begin
         Bind_Socket (Socket, Address);
         exit;
   exception
      when Pippo : Socket_Error =>
            if Resolve_Exception(Pippo) = Address_Already_In_Use then
               Address.Port := Address.Port + 1;
            else
               raise Socket_Error;
            end if;
      end;
   end loop;

end Listen_To_Port;

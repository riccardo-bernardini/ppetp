--                  -*- Mode: Ada -*-
--  Filename        : Create_Server.adb
--  Description     : Create Server
--
--  This procedure initializes a server in a p2p network.
--
--  The functions principal turns in this procedure are the followings:
--
--  -  Are recorded commands that will be recognized by the servers.
--
--  -  Initialization of the socket and opening of the port
--     UDP_Data.
--
--  -  Loop for the receipt and elaboration of the messages coming from the
--     clients.
--
--  -  Raising of an exception in the case of receipt of a non belonging
--     message to the protocol.

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Exceptions;
use  Ada.Exceptions;

with Parsers;
use  Parsers;

with Hash_Auth;
use  Hash_Auth;

with Globals;
use  Globals;

with Registra_Comandi_Server;

with Listen_To_Port;

with Receive_From_Socket;


procedure Create_Server is

   From       : Sock_Addr_Type;
   Server     : Socket_Type;
   P          : Parser;
   Message    : Unbounded_String;

begin

   --  Procedura per la registrazione di tutti i comandi del server.

   Registra_Comandi_Server(P);

   Initialize;

   -- Creazione del socket Server ed apertura della porta UDP di comunicazione.

   Listen_To_Port(Server, 55505);

   --  Loop per la ricezione ed elaborazione dei messaggi provenienti dai client.


   loop
      Receive_From_Socket(Server, Message, From);
      Put_Line("[" & To_String(Message) & "]");
      P.Process(Message);
      Put_LIne("Dopo process");
   end loop;

--     Close_Socket(Server);

   Finalize;

end Create_Server;




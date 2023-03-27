--                  -*- Mode: Ada -*-
--  Filename        : Create_Client.adb
--  Description     : Create Client
--
--  This procedure initializes a client in a p2p network.
--
--  The functions principal turns in this procedure are the followings:
--
--  -  A cicle is anticipated to contemoporarily make to run
--     more clients.
--
--  -  Are recorded commands that will be recognized by the clients.
--
--  -  Initialization of the socket and opening of the ports
--     UDP_Data and UDP_Control.
--
--  -  Dispatch of the Command of STRT for the initialization of the phase
--     of negotiation between client and server.
--
--  -  Loop for the receipt and elaboration of the messages coming from the
--     servers.
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

with Ada.Command_Line;
use  Ada.Command_Line;

with Parsers;
use  Parsers;

with Listen_UDP;
use  Listen_UDP;

with Listen_TCP;
use  Listen_TCP;

with Query_State;
use  Query_State;

with Globals;
use  Globals;

with Listen_To_Port;

with Registra_Comandi_Client;

with Send_Command;

with String_Queues;

procedure Create_Client is

   Group          : constant String := "127.0.0.1";
   P              : Parser;
   Message        : Unbounded_String;
   Command        : Unbounded_String;
   TCP_Control    : Integer;
   UDP_Receiver   : Listen_UDP.Listener;
   TCP_Receiver   : Listen_TCP.Listener;
   DB_Port        : Integer;

begin

   Initialize;

   --  Prevedere un ciclo per far girare più client contemporaneamente.

   if Argument_Count >= 0 then
      DB_Port := Integer'Value(Argument(1));
   else
      return;
   end if;


   --  Inizializzazione dei comandi che saranno riconosciuti dai client.

   Registra_Comandi_Client(P);

   --  Collegamento del Client al server centralizzato DB.
   --  Dovrò scrivere in DB il numero di porta TCP tramite la funzione SET,
   --  e poi potrò leggere tale valore tramite la funzione GET.

   TCP_Control := Return_Port;

   Connect(Connection => DB,
           Port       => Port_Type(DB_Port));

   Set(DB, "P2P.INTERNAL_PORT", TCP_Control);

   UDP_Receiver.Start(UDP_Port, Buffer'Access);
   TCP_Receiver.Start(TCP_Control, Buffer'Access);


--     Listen_To_Port(Data_Client, Port_Type(First_Port));
--     Listen_To_Port(Command_Client, Port_Type(First_Port + 1));

   --  Il messaggio da inviare al server dovrà essere costituito almeno
   --  da 3 tokens, e questo controllo lo effettuo nel package Parsers.

   --  Invio del Comando di STRT per l'inizializzazione della fase di
   --  negoziazione tra client e server.

--     Send_Command("STRT", ( 1 => To_Unbounded_String("www.google.it"),
--                            2 => To_Unbounded_String(Integer'Image(First_Port)),
--                            3 => To_Unbounded_String(Integer'Image(First_Port + 1)),
--                            4 => To_Unbounded_String("1")
--                           ),
--                             (Family => Family_Inet,
--                              Addr   => Inet_Addr(Group),
--                              Port   => 55505));

   --  Il comando Buffer.Extract(Command) lo devo inserire all'interno del
   --  loop. Serve per estrarre il tipo di comando che se intende processare.




   --  Loop per la ricezione ed elaborazione dei messaggi provenienti dal client.
   loop
      begin
         Buffer.Extract(Command);
         Put_Line("[" & To_String(Command) & "]");
         P.Process(Command);
         Put_Line("DOpo Process");
      exception
         when E : others =>
            --  Sollevamento di un'eccezione nel caso di ricezione
            --  di un messaggio non appartenente al protocollo.

            Put_Line(Exception_Name (E) & ": " & Exception_Message (E));
      end;
   end loop;

   Finalize;
end Create_Client;


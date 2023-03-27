--                  -*- Mode: Ada -*-
--  Filename        : Listen_UDP.adb
--  Description     : Listen to UDP Port.
--
--  The aim of this package is

--  Package che contiene un task che permette al client di mettersi in ascolto
--  sulla porta UDP.
--
--  La funzione Listen_Port crea un socket, e si mette in ricezione dei dati
--  provenienti dal Server. Ogni qualvolta riceve un messaggio lo ritorna,
--  e tale messaggio verrà poi scritto all'interno di un Buffer protetto.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Exceptions;
use  Ada.Exceptions;

with GNAT.Sockets;
use  GNAT.Sockets;

with Generic_Protected_Queue;

with String_Queues;

with Listen_Port;

package Listen_UDP is

   task type Listener is

      entry Start(Port          : in Integer;
                  String_Buffer : in String_Queues.Access_Queue_Type);

   end Listener;

end Listen_UDP;




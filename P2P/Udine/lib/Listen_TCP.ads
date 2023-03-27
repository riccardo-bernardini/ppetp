--                  -*- Mode: Ada -*-
--  Filename        : Listen_TCP.adb
--  Description     : Listen to TCP Port.
--
--

--  Package che contiene un task che consente al client di mettersi in ascolto
--  sulla porta TCP per vedere le eventuali connessioni che gli arrivano.
--
--  Dato che sto lavorando con il protocollo TCP, dopo aver creato il socket
--  e invocato la funzione Listen, è presente un loop per accettare delle
--  connessioni su quella porta.
--
--  Ogni qualvolta mi arriverà una nuova connessione, tramite un puntatore ad
--  un altro task (TCP_Insert), faccio in modo che il messaggio ricevuto
--  da ciascuna connessione venga scritto sul Buffer protetto.
--
--  E' previsto un array di puntatori al task di lunghezza 5.

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Generic_Protected_Queue;

with String_Queues;

with TCP_Inserters;
use  TCP_Inserters;

with Ada.Streams;
use  Ada.Streams;

with Listen_Port;

package Listen_TCP is

   task type Listener is

      entry Start(Port          : in Integer;
                  String_Buffer : in String_Queues.Access_Queue_Type);

   end Listener;

end Listen_TCP;

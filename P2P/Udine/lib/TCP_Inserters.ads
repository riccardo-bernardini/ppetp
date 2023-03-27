--  Package che contiene un task che ha il compito di leggere i dati
--  provenienti dalle varie connessioni, e scrive nel buffer protetto i vari
--  messaggi provenienti da esse.


with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with Byte_To_String;

with Generic_Protected_Queue;

with String_Queues;

package TCP_Inserters is

   task type TCP_Insert is

      entry Start(String_Buffer : in String_Queues.Access_Queue_Type;
                  Socket        : in Socket_Type);

   end TCP_Insert;

   type Listen_Access is access all TCP_Insert;

end TCP_Inserters;

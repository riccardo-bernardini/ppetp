with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with GNAT.Sockets;
use  GNAT.Sockets;

with String_Queues;

package body TCP_Inserters is

   task body TCP_Insert is

      Buffer     : String_Queues.Access_Queue_Type;
      Message    : Unbounded_String;
      From       : Sock_Addr_Type;
      TCP_CLient : Socket_Type;
      Last       : Stream_Element_Offset;
      Item       : Stream_Element_Array(1..1024);

   begin

      accept Start(String_Buffer : in String_Queues.Access_Queue_Type;
                   Socket        : in Socket_Type) do

         Buffer     := String_Buffer;
         TCP_Client := Socket;

      end Start;

      loop

         Receive_Socket(TCP_Client, Item, Last, From);

         exit when Last = Item'First - 1;

         Message := Byte_To_String(Item(Item'First..Last));

         Buffer.Insert(Message);

      end loop;

   exception
      when Tasking_Error => Put("Exception in Task write");
         New_Line;


   end TCP_Insert;

end TCP_Inserters;

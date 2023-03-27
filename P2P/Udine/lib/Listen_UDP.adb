
package body Listen_UDP is

   task body Listener is

      N_Port     : Port_Type;
      Buffer     : String_Queues.Access_Queue_Type;
      Message    : Unbounded_String;
      From       : Sock_Addr_Type;
--        UDP_CLient : Socket_Type;

   begin

      accept Start(Port          : in Integer;
                   String_Buffer : in String_Queues.Access_Queue_Type) do

         N_Port := Port_Type(Port);
         Buffer := String_Buffer;

      end Start;

      loop
         Listen_Port(N_Port, Message, From);
         Put_Line(To_String(Message));
         Buffer.Insert(Message);
         Put_Line("Message");
      end loop;

   exception
      when Tasking_Error => Put("Exception in Task write");
         New_Line;


   end Listener;

end Listen_UDP;




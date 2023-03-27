
package body Listen_TCP is


   task body Listener is

      Ascoltatori : array(1..5) of Listen_Access;
      N_Port      : Port_Type;
      Buffer      : String_Queues.Access_Queue_Type;
      Message     : Unbounded_String;
      From        : Sock_Addr_Type;
      Address     : Sock_Addr_Type;
      TCP_Client  : Socket_Type;
      Server      : Socket_Type;
--        Last        : Stream_Element_Offset;
--        Item        : Stream_Element_Array(1..1024);

   begin

      accept Start(Port          : in Integer;
                   String_Buffer : in String_Queues.Access_Queue_Type) do

         N_Port := Port_Type(Port);
         Buffer := String_Buffer;

      end Start;

      Create_Socket (TCP_Client, Family_Inet);

      Address.Addr := Any_Inet_Addr;
      Address.Port := N_Port;

      Bind_Socket(TCP_Client, Address);
      Listen_Socket(TCP_Client);

      loop

         Accept_Socket(TCP_Client, Server, From);

         --  Prevedere un ciclo per verificare che la posizione di Ascoltatori
         --  sia libera.

         for I in Ascoltatori'Range loop
            if Ascoltatori(I) = null then
              Ascoltatori(I) := new TCP_Insert;
               Ascoltatori(I).Start(Buffer, Server);
               exit;
            end if;

         end loop;



--           Ascoltatori(3) := new TCP_Insert;
--           Ascoltatori(3).Start(Buffer, Server);

      end loop;

   exception
      when Tasking_Error => Put("Exception in Task write");
         New_Line;

   end Listener;

end Listen_TCP;

with GNAT.Sockets;    use  GNAT.Sockets;

package body Tcp_Server is
   Unavailable_Port : exception;

   --
   -- Utility function which prepares a Socket for accepting TCP
   -- connection to a given port.  If the port is busy, the port
   -- number is increased until a free port is found or Max_Port
   -- is reached.  The port number actually used is returned in Port.
   -- If no free port is found, exception Unavailable_Port is raised.
   --
   procedure Open_Tcp_Server (Socket   : in out Socket_Type;
                              Port     : in out Port_Type;
                              Max_Port : in     Port_Type := Port_Type'last)
   is
      Done : Boolean;
   begin
      Create_Socket (Socket);

  Port_Searching:
      loop
         Done := True;
         begin
            Bind_Socket (Socket  => Socket,
                         Address => (Family => Family_Inet,
                                     Addr   => Any_Inet_Addr,
                                     Port   => Port));
         exception
            when Socket_Error =>
               Done := False;
               if (Port = Max_Port) then
                  raise Unavailable_Port;
               end if;

               Port := Port + 1;
         end;

         exit Port_Searching when Done;
      end loop Port_Searching;

      Listen_Socket(Socket);
   end Open_Tcp_Server;

   ---------------------
   -- Server_Listener --
   ---------------------

   task body Server_Listener is
      My_Port        : Port_Type;
      Server_Socket  : Socket_Type;
      New_Connection : Callback_Type;
      Last_Port      : Port_Type;
   begin
      accept Initialize (Port       : in out Port_Type;
                         Callback   : in     Callback_Type;
                         Port_Fixed : in     Boolean       := True) do
         if (Port_Fixed) then
            Last_Port := Port;
         else
            Last_Port := Port_Type'Last;
         end if;

         Open_Tcp_Server(Server_Socket, Port, Last_Port);
         My_Port := Port;
         New_Connection := Callback;
      exception
         when Unavailable_Port =>
            My_Port := No_Port;
      end Initialize;

      if (My_Port /= No_Port) then
     Main_Loop:
         declare
            Socket       : Socket_Type;
            Peer_Address : Sock_Addr_Type;
            Done         : Boolean;
         begin
            Done := False;
            while (not Done) loop
               select
                  delay 1.0;
               then abort
                  Socket := No_Socket;
                  Accept_Socket(Server  => Server_Socket,
                                Socket  => Socket,
                                Address => Peer_Address);
               end select;

               if (Socket /= No_Socket) then
                  New_Connection (Socket, Peer_Address);
               end if;

               select
                  accept Finish do
                     Done := True;
                  end Finish;
               else
                  null;
               end select;
            end loop;
         end Main_Loop;
      end if;
   end Server_Listener;

end Tcp_Server;

